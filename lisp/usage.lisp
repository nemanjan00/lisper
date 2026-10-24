;;; usage.lisp — token usage and a rough cost for the session (/usage).
;;;
;;; Every model response sets LISPING-CORE:*LAST-USAGE* to a fresh hash
;;; table.  An observer on the event stream notices a new one (by EQ),
;;; adds it to the totals of the model that was in use, and saves the
;;; totals to the session's usage.json, so a restart resumes the count.
;;; The proxy we use reports prompt_tokens INCLUDING the cached tokens
;;; (and cache writes), so the uncached part is prompt - cached - write.
;;; The Anthropic API reports them separately instead; usage-add notices
;;; that (cached + write > prompt) and adds the parts, so the totals
;;; always mean "all input tokens" and the hit rate stays at most 100%.
;;; cache_write_tokens is not a standard OpenAI field, so writes are
;;; usually reported as 0: the write count and its cost are an estimate.
;;;
;;; Prices are USD per million tokens (input, cache read, cache write
;;; (5-minute, 1.25x input), output), picked by a substring of the model
;;; name with "." read as "-" (first match wins, so the specific rows
;;; come first).  From Anthropic's public list prices (checked 2026-10-24;
;;; Haiku 5.5 is the up-to-100k-prompt tier).  Through a subscription
;;; proxy the cost is nominal: what the tokens would cost at list price,
;;; not what is billed.  A model with no entry has no cost.

(defvar *usage-totals* (make-hash-table :test 'equal)
  "model name -> plist (:calls :prompt :cached :write :completion).")
(defvar *usage-seen* nil "The last *LAST-USAGE* object that was counted.")
(defvar *usage-lock* (sb-thread:make-mutex :name "usage"))

(defvar *usage-prices*
  '(("opus-5-5"  4.0   0.2    5.0   20.0)
    ("opus-5"    5.0   0.5    6.25  25.0)
    ("opus-4-1" 15.0   1.5   18.75  75.0)
    ("opus-4-0" 15.0   1.5   18.75  75.0)
    ("opus-4-20" 15.0  1.5   18.75  75.0)   ; claude-opus-4-2025..., the original Opus 4
    ("opus"      5.0   0.5    6.25  25.0)   ; 4.5 - 4.8
    ("sonnet-5"  2.0   0.1    2.5   10.0)
    ("sonnet"    3.0   0.3    3.75  15.0)   ; 4.x
    ("haiku-5"   0.1   0.01   0.125  0.5)
    ("haiku"     1.0   0.1    1.25   5.0)   ; 4.5
    ("kimi"      0.6   0.15   0.6    2.5))
  "(SUBSTRING IN CACHE-READ CACHE-WRITE OUT), USD per million tokens.")

(defun usage-price (model)
  (let ((m (substitute #\- #\. (string-downcase (or model "")))))
    (cdr (find-if (lambda (e) (search (first e) m)) *usage-prices*))))

(defun usage-int (table &rest keys)
  (let ((v table))
    (dolist (k keys) (setf v (and (hash-table-p v) (gethash k v))))
    (if (integerp v) v 0)))

(defun usage-file () (lisping-core:session-path "usage.json"))

(defun usage-save ()
  (ignore-errors
   (let ((o (lisping-core:obj)))
     (maphash (lambda (model p)
                (setf (gethash model o)
                      (lisping-core:obj "calls" (getf p :calls) "prompt" (getf p :prompt)
                                        "cached" (getf p :cached) "write" (getf p :write)
                                        "completion" (getf p :completion))))
              *usage-totals*)
     (with-open-file (out (usage-file) :direction :output :if-exists :supersede
                                       :external-format :utf-8)
       (write-string (lisping-core:json-encode o) out)))))

(defun usage-load ()
  (clrhash *usage-totals*)
  (ignore-errors
   (when (probe-file (usage-file))
     (let ((o (lisping-core:json-decode (lisping-core:read-file (usage-file)))))
       (maphash (lambda (model v)
                  (setf (gethash model *usage-totals*)
                        (list :calls (usage-int v "calls") :prompt (usage-int v "prompt")
                              :cached (usage-int v "cached") :write (usage-int v "write")
                              :completion (usage-int v "completion"))))
                o))))
  (hash-table-count *usage-totals*))

(defun usage-add (usage model)
  "Add one response's USAGE table to MODEL's totals."
  (sb-thread:with-mutex (*usage-lock*)
    (let* ((p (or (gethash model *usage-totals*)
                  (list :calls 0 :prompt 0 :cached 0 :write 0 :completion 0)))
           (prompt (usage-int usage "prompt_tokens"))
           (cached (usage-int usage "prompt_tokens_details" "cached_tokens"))
           (write (usage-int usage "prompt_tokens_details" "cache_write_tokens")))
      ;; Cached parts bigger than the prompt: this provider reports them
      ;; apart from prompt_tokens (Anthropic style), so add them.
      (when (> (+ cached write) prompt)
        (setf prompt (+ prompt cached write)))
      (setf (gethash model *usage-totals*)
            (list :calls (1+ (getf p :calls))
                  :prompt (+ (getf p :prompt) prompt)
                  :cached (+ (getf p :cached) cached)
                  :write (+ (getf p :write) write)
                  :completion (+ (getf p :completion) (usage-int usage "completion_tokens"))))))
  (usage-save))

(defun usage-observe (event)
  "Observer: count a new *LAST-USAGE* when one has appeared."
  (declare (ignore event))
  (let ((u lisping-core:*last-usage*))
    (when (and (hash-table-p u) (not (eq u *usage-seen*)))
      (setf *usage-seen* u)
      (usage-add u (or lisping-core:*model* "?")))))

(defun usage-cost (model p)
  "Dollars for plist P at MODEL's prices, or NIL without a price."
  (let ((pr (usage-price model)))
    (when pr
      (destructuring-bind (in read write out) pr
        (let ((uncached (max 0 (- (getf p :prompt) (getf p :cached) (getf p :write)))))
          (/ (+ (* uncached in) (* (getf p :cached) read)
                (* (getf p :write) write) (* (getf p :completion) out))
             1000000.0))))))

(defun usage-sum ()
  "Totals over all models: (values plist cost-or-NIL partial-p).  PARTIAL-P
is true when some model had no price, so the cost leaves it out."
  (let ((sum (list :calls 0 :prompt 0 :cached 0 :write 0 :completion 0))
        (cost 0.0) (any nil) (partial nil))
    (sb-thread:with-mutex (*usage-lock*)
      (maphash (lambda (model p)
                 (dolist (k '(:calls :prompt :cached :write :completion))
                   (incf (getf sum k) (getf p k)))
                 (let ((c (usage-cost model p)))
                   (if c (setf any t cost (+ cost c)) (setf partial t))))
               *usage-totals*))
    (values sum (and any cost) partial)))

(defun usage-fmt-tokens (n)
  (cond ((>= n 1000000) (format nil "~,2FM" (/ n 1000000.0)))
        ((>= n 1000) (format nil "~,1Fk" (/ n 1000.0)))
        (t (format nil "~D" n))))

(defun usage-fmt-cost (c)
  (cond ((null c) "n/a")
        ((< c 0.01) (format nil "~~$~,4F" c))
        (t (format nil "~~$~,2F" c))))

(defun usage-hit-rate (p)
  (if (plusp (getf p :prompt)) (round (* 100 (getf p :cached)) (getf p :prompt)) 0))

(defun usage-summary-line ()
  "One line for the context pane, or NIL before any call."
  (multiple-value-bind (s cost) (usage-sum)
    (when (plusp (getf s :calls))
      (format nil "session: ~D calls · ~A in (~D% cached) · ~A out · ~A nominal"
              (getf s :calls) (usage-fmt-tokens (getf s :prompt)) (usage-hit-rate s)
              (usage-fmt-tokens (getf s :completion)) (usage-fmt-cost cost)))))

(defun usage-compact-line ()
  "Short form for a narrow pane: calls, cache hit rate and cost."
  (multiple-value-bind (s cost) (usage-sum)
    (when (plusp (getf s :calls))
      (format nil "~D calls · ~D% cached · ~A" (getf s :calls) (usage-hit-rate s)
              (usage-fmt-cost cost)))))

(defun usage-report ()
  "Lines for /usage."
  (multiple-value-bind (s cost partial) (usage-sum)
    (if (zerop (getf s :calls))
        (list "No model calls counted yet.")
        (append
         (list (format nil "Session usage: ~D model calls, ~A tokens in (~D% from cache, ~A written to cache), ~A out."
                       (getf s :calls) (usage-fmt-tokens (getf s :prompt)) (usage-hit-rate s)
                       (usage-fmt-tokens (getf s :write)) (usage-fmt-tokens (getf s :completion)))
               (format nil "Nominal cost at list prices: ~A~A. Through a subscription proxy this is not what you are billed."
                       (usage-fmt-cost cost) (if partial ", without the unpriced models" ""))
               "Cache writes are an estimate (most APIs do not report them; they bill at 1.25x input), so the cost may be low.")
         (let ((rows nil))
           (maphash (lambda (model p)
                      (push (format nil "  ~A: ~D calls, ~A in, ~A out, ~A"
                                    model (getf p :calls) (usage-fmt-tokens (getf p :prompt))
                                    (usage-fmt-tokens (getf p :completion))
                                    (usage-fmt-cost (usage-cost model p)))
                            rows))
                    *usage-totals*)
           (sort rows #'string<))))))

(defun slash-usage (ui args)
  (declare (ignore ui))
  (let ((a (string-downcase (string-trim " " (or args "")))))
    (cond ((string= a "reset")
           (clrhash *usage-totals*) (usage-save)
           (slash-say "Usage counters reset."))
          (t (dolist (l (usage-report)) (slash-say l))))))

(defun usage-start ()
  "Called from init.lisp: load the saved totals, count from the event stream, register /usage."
  (unless lisping-core:*test-boot* (usage-load))
  (setf *usage-seen* lisping-core:*last-usage*)   ; what exists now was counted already (or predates us)
  (pushnew 'usage-observe lisping-core:*observers*)
  (define-slash "usage" 'slash-usage "tokens, cache hit rate and rough cost of this session (reset)"
    (lambda (ui partial) (declare (ignore ui partial)) '("reset"))))
