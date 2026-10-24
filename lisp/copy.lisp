;;; copy.lisp — copy chat text to the clipboard (todo: copy from chat).
;;;
;;; The clipboard is set with an OSC 52 escape written to the terminal, so
;;; it works over SSH and on a phone.  Inside tmux the app's own OSC 52 is
;;; dropped under `set-clipboard external` (we never change tmux's
;;; settings), so there the text goes through `tmux set-buffer -w`, which
;;; hands it to the outer terminal's clipboard itself.
;;;
;;;   /copy            last assistant reply
;;;   /copy code [N]   last code block of the last reply (N = that many
;;;                    blocks back, 1 = the last)
;;;   /copy user       your last message
;;;   Ctrl-Y           same as /copy
;;; The history of replies comes from *MESSAGES*, so what is copied is the
;;; raw text (markdown, no wrapping or colours).

(defun copy-base64 (string)
  "Base64 of STRING's UTF-8 bytes."
  (let* ((bytes (sb-ext:string-to-octets string :external-format :utf-8))
         (tbl "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (n (length bytes)))
    (with-output-to-string (out)
      (loop for i from 0 below n by 3
            do (let* ((b0 (aref bytes i))
                      (b1 (if (< (+ i 1) n) (aref bytes (+ i 1)) 0))
                      (b2 (if (< (+ i 2) n) (aref bytes (+ i 2)) 0))
                      (v (logior (ash b0 16) (ash b1 8) b2)))
                 (write-char (char tbl (ldb (byte 6 18) v)) out)
                 (write-char (char tbl (ldb (byte 6 12) v)) out)
                 (write-char (if (< (+ i 1) n) (char tbl (ldb (byte 6 6) v)) #\=) out)
                 (write-char (if (< (+ i 2) n) (char tbl (ldb (byte 6 0) v)) #\=) out))))))

(defun copy-osc52 (string)
  "The OSC 52 sequence that sets the clipboard to STRING."
  (format nil "~C]52;c;~A~C\\" #\Esc (copy-base64 string) #\Esc))

(defun copy-in-tmux-p ()
  (let ((v (sb-ext:posix-getenv "TMUX"))) (and v (plusp (length v)))))

(defun copy-to-clipboard (string)
  "Put STRING on the clipboard.  Returns :tmux or :osc52."
  (cond ((copy-in-tmux-p)
         ;; The text goes through a temp file: no shell quoting, any size.
         (let ((f (lisping-core:session-path "tmp/copy.txt")))
           (ensure-directories-exist f)
           (with-open-file (o f :direction :output :if-exists :supersede
                                :external-format :utf-8)
             (write-string string o))
           (lisping-core:shell (format nil "tmux load-buffer -w ~A"
                                       (lisping-core::curl-quote (namestring f))))
           (ignore-errors (delete-file f)))
         :tmux)
        (t (tty-write (copy-osc52 string)) :osc52)))

(defun copy-message-text (m)
  (let ((c (gethash "content" m)))
    (and (stringp c) (plusp (length c)) c)))

(defun copy-last-message (role)
  "Text of the last message with ROLE (not a system-written one)."
  (loop for m in (reverse lisping-core:*messages*)
        when (and (equal (gethash "role" m) role)
                  (not (lisping-core:synthetic-message-p m))
                  (copy-message-text m))
          do (return (copy-message-text m))))

(defun copy-code-blocks (text)
  "The bodies of the ``` fenced blocks in TEXT, in order."
  (let ((blocks nil) (pos 0))
    (loop
      (let ((a (search "```" text :start2 pos)))
        (unless a (return))
        (let* ((eol (position #\Newline text :start a))
               (b (and eol (search "```" text :start2 (1+ eol)))))
          (unless b (return))
          (push (string-right-trim '(#\Newline) (subseq text (1+ eol) b)) blocks)
          (setf pos (+ b 3)))))
    (nreverse blocks)))

(defun copy-describe (text)
  (let ((lines (1+ (count #\Newline text))))
    (format nil "~D line~:P, ~D chars" lines (length text))))

(defun ui-split-words (s)
  (let ((r nil) (start nil))
    (dotimes (i (length s))
      (if (member (char s i) '(#\Space #\Tab))
          (when start (push (subseq s start i) r) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq s start) r))
    (nreverse r)))

(defun copy-run (what)
  "Copy by WHAT: \"\" / reply, \"code [N]\", \"user\".  Returns a message."
  (let* ((words (remove "" (ui-split-words what) :test #'string=))
         (kind (or (first words) "reply")))
    (cond
      ((member kind '("reply" "last") :test #'string=)
       (let ((text (copy-last-message "assistant")))
         (if text
             (progn (copy-to-clipboard text)
                    (format nil "Copied the last reply (~A)." (copy-describe text)))
             "No reply to copy yet.")))
      ((string= kind "user")
       (let ((text (copy-last-message "user")))
         (if text
             (progn (copy-to-clipboard text)
                    (format nil "Copied your last message (~A)." (copy-describe text)))
             "No message of yours to copy.")))
      ((string= kind "code")
       (let* ((n (or (ignore-errors (parse-integer (or (second words) "1"))) 1))
              (blocks (copy-code-blocks (or (copy-last-message "assistant") "")))
              (k (- (length blocks) (max 1 n))))
         (cond ((null blocks) "No code block in the last reply.")
               ((< k 0) (format nil "The last reply has only ~D code block~:P." (length blocks)))
               (t (let ((text (nth k blocks)))
                    (copy-to-clipboard text)
                    (format nil "Copied code block ~D of ~D (~A)." (1+ k) (length blocks)
                            (copy-describe text)))))))
      (t "Usage: /copy [reply | code [N] | user]"))))

(defun slash-copy (ui args)
  (declare (ignore ui))
  (slash-say (copy-run (or args ""))))

(defun copy-start ()
  "Called from init.lisp: register /copy."
  (define-slash "copy" 'slash-copy "copy the last reply, or: code [N], user"
    (lambda (ui partial) (declare (ignore ui))
      (remove-if-not (lambda (w) (and (<= (length partial) (length w))
                                      (string-equal partial w :end2 (length partial))))
                     '("reply" "code" "user")))))
