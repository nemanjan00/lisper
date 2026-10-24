;;;; lisp/markdown.lisp -- a small markdown renderer for assistant text.
;;;;
;;;; (md-lines TEXT) -> list of chunk-lists, one per logical line, ready for
;;;; the chat buffer: **bold**, *italic*, `code` (no backticks), # headings,
;;;; - and 1. lists (the wrapper hangs the continuation under the text),
;;;; > quotes, ``` fenced blocks (dim background, no fence lines), simple
;;;; | tables, [text](url) links, --- rules.  Pure: no UI state.  Used for
;;;; assistant text only; user text and tool output are never touched.

(in-package :lisping)

(defparameter *md-code-attr* (ui-attr 6))
(defparameter *md-block-attr* (ui-attr 7 :bg 0 :bgbright t))
(defparameter *md-link-attr* (logior (ui-attr 6) +attr-underline+))

(defun md-blank-line () (list (cons " " 7)))

(defun md-blank-p (chunks)
  (and chunks (null (cdr chunks)) (equal (car (first chunks)) " ") (eql (cdr (first chunks)) 7)))

(defun md-italic-close (s c from)
  "Index of the char C closing an italic run opened just before FROM, or NIL."
  (let ((n (length s)))
    (loop for e from from below n
          when (and (char= (char s e) c)
                    (> e from)
                    (char/= (char s (1- e)) #\Space)
                    (or (= (1+ e) n)
                        (if (char= c #\_)
                            (not (alphanumericp (char s (1+ e))))
                            (char/= (char s (1+ e)) #\*))))
            return e)))

(defun md-inline (s attr)
  "Chunks for the inline markdown in S on top of ATTR."
  (let ((out nil) (lit (make-string-output-stream)) (i 0) (n (length s)))
    (labels ((ch (k) (and (< -1 k n) (char s k)))
             (flush ()
               (let ((x (get-output-stream-string lit)))
                 (when (plusp (length x)) (push (cons x attr) out))))
             (put (chunks) (flush) (dolist (c chunks) (push c out)))
             (literal (c) (write-char c lit) (incf i)))
      (loop while (< i n)
            do (let ((c (char s i)))
                 (cond
                   ((char= c #\`)
                    (let ((e (position #\` s :start (1+ i))))
                      (if (and e (> e (1+ i)))
                          (progn (put (list (cons (subseq s (1+ i) e) *md-code-attr*)))
                                 (setf i (1+ e)))
                          (literal c))))
                   ((and (char= c #\*) (eql (ch (1+ i)) #\*)
                         (ch (+ i 2)) (not (eql (ch (+ i 2)) #\Space)))
                    (let ((e (search "**" s :start2 (+ i 2))))
                      (if (and e (> e (+ i 2)) (not (eql (ch (1- e)) #\Space)))
                          (progn (put (md-inline (subseq s (+ i 2) e) (logior attr +attr-bold+)))
                                 (setf i (+ e 2)))
                          (literal c))))
                   ((and (member c '(#\* #\_))
                         (ch (1+ i)) (not (member (ch (1+ i)) '(#\Space #\*)))
                         (or (char= c #\*) (not (and (ch (1- i)) (alphanumericp (ch (1- i)))))))
                    (let ((e (md-italic-close s c (1+ i))))
                      (if e
                          (progn (put (md-inline (subseq s (1+ i) e) (logior attr +attr-italic+)))
                                 (setf i (1+ e)))
                          (literal c))))
                   ((char= c #\[)
                    (let* ((rb (position #\] s :start (1+ i)))
                           (lp (and rb (eql (ch (1+ rb)) #\()))
                           (rp (and lp (position #\) s :start (+ rb 2)))))
                      (if (and rp (> rb (1+ i)))
                          (let ((text (subseq s (1+ i) rb))
                                (url (subseq s (+ rb 2) rp)))
                            (put (append (md-inline text (logior (logand attr 255) +attr-underline+ 6))
                                          (unless (string= text url)
                                            (list (cons (format nil " (~A)" url) 8)))))
                            (setf i (1+ rp)))
                          (literal c))))
                   (t (literal c)))))
      (flush))
    (nreverse out)))

(defun md-hr-p (trim)
  (and (>= (length trim) 3)
       (member (char trim 0) '(#\- #\* #\_))
       (every (lambda (c) (member c (list (char trim 0) #\Space))) trim)
       (>= (count (char trim 0) trim) 3)))

(defun md-list-item (ln)
  "(values indent marker rest) when LN is a list item, else NIL."
  (let* ((ind (or (position #\Space ln :test-not #'char=) (length ln)))
         (rest (subseq ln ind)))
    (cond ((and (>= (length rest) 2) (member (char rest 0) '(#\- #\* #\+))
                (char= (char rest 1) #\Space))
           (values ind "•" (subseq rest 2)))
          (t (let ((p (position-if-not #'digit-char-p rest)))
               (when (and p (< 0 p 4) (< (1+ p) (length rest))
                          (member (char rest p) '(#\. #\)))
                          (char= (char rest (1+ p)) #\Space))
                 (values ind (subseq rest 0 (1+ p)) (subseq rest (+ p 2)))))))))

(defun md-split-cells (line)
  (let* ((s (string-trim " " line))
         (s (if (and (plusp (length s)) (char= (char s 0) #\|)) (subseq s 1) s))
         (s (if (and (plusp (length s)) (char= (char s (1- (length s))) #\|))
                (subseq s 0 (1- (length s))) s)))
    (loop with start = 0
          for p = (position #\| s :start start)
          collect (string-trim " " (subseq s start p))
          while p do (setf start (1+ p)))))

(defun md-table-separator-p (cells)
  (and cells (every (lambda (c) (and (plusp (length c))
                                     (every (lambda (x) (member x '(#\- #\:))) c)))
                    cells)))

(defun md-table-lines (rows)
  "Chunk-lists for the table ROWS (strings): cells padded to their column
width, a rule under the header."
  (let* ((parsed (remove-if #'md-table-separator-p (mapcar #'md-split-cells rows)))
         (header-p (and (cdr rows) (md-table-separator-p (md-split-cells (second rows)))))
         (ncols (reduce #'max parsed :key #'length :initial-value 0))
         (cells (loop for r in parsed for ri from 0
                      collect (loop for k below ncols
                                    collect (ui-truncate-chunks
                                             (or (md-inline (or (nth k r) "")
                                                            (if (and header-p (zerop ri))
                                                                (logior 7 +attr-bold+) 7))
                                                 (list (cons "" 7)))
                                             40))))
         (widths (loop for k below ncols
                       collect (reduce #'max cells
                                       :key (lambda (r) (length (chunks-text (nth k r))))
                                       :initial-value 1)))
         (out nil))
    (loop for r in cells for ri from 0
          do (push (append (list (cons " " 7))
                           (loop for cell in r for w in widths for k from 0
                                 append (append (when (plusp k) (list (cons " │ " 8)))
                                                cell
                                                (list (cons (make-string (max 0 (- w (length (chunks-text cell))))
                                                                         :initial-element #\Space)
                                                            7)))))
                   out)
             (when (and header-p (zerop ri))
               (push (list (cons (format nil " ~{~A~^─┼─~}"
                                         (mapcar (lambda (w) (make-string w :initial-element #\─)) widths))
                                 8))
                     out)))
    (nreverse out)))

(defun md-table-line-p (ln)
  (let ((s (string-trim " " ln)))
    (and (> (length s) 1) (char= (char s 0) #\|) (find #\| s :start 1))))

(defun md-lines (text)
  "Markdown TEXT -> (values chunk-lists hangs): one chunk-list per logical
line, and for each the indent its wrapped continuation lines take (a list
item hangs under its text)."
  (let ((lines (ui-split-lines
                (substitute #\Space #\Tab (remove #\Return text))))
        (out nil) (in-code nil))
    (flet ((put (chunks &optional (hang 0)) (push (cons chunks hang) out)))
      (loop while lines
            do (let* ((ln (pop lines)) (trim (string-trim " " ln)))
                 (cond
                   ((and (>= (length trim) 3) (string= (subseq trim 0 3) "```"))
                    (setf in-code (not in-code)))
                   (in-code
                    (put (list (cons (concatenate 'string " " ln) *md-block-attr*)) 1))
                   ((zerop (length trim)) (put (md-blank-line)))
                   ((md-table-line-p ln)
                    (let ((rows (list ln)))
                      (loop while (and lines (md-table-line-p (first lines)))
                            do (push (pop lines) rows))
                      (dolist (l (md-table-lines (nreverse rows))) (put l 1))))
                   ((md-hr-p trim) (put (list (cons (make-string 24 :initial-element #\─) 8))))
                   ((char= (char trim 0) #\#)
                    (let* ((k (or (position #\# trim :test-not #'char=) (length trim)))
                           (body (string-left-trim " " (subseq trim k))))
                      (if (and (<= k 6) (plusp (length body)))
                          (put (md-inline body (logior (if (<= k 2) 4 7) +attr-bold+)))
                          (put (md-inline ln 7)))))
                   ((char= (char trim 0) #\>)
                    (put (cons (cons "▎ " 8)
                               (md-inline (string-left-trim " " (subseq trim 1)) 7))
                         2))
                   (t (multiple-value-bind (ind marker rest) (md-list-item ln)
                        (if marker
                            (put (append (list (cons (make-string (1+ ind) :initial-element #\Space) 7)
                                               (cons marker 4)
                                               (cons " " 7))
                                         (md-inline rest 7))
                                 (+ ind 1 (length marker) 1))
                            (put (md-inline ln 7)))))))))
    ;; Collapse runs of blank lines, trim blanks at both ends.
    (let ((res nil))
      (dolist (l (nreverse out))
        (unless (and (md-blank-p (car l)) (or (null res) (md-blank-p (car (car res)))))
          (push l res)))
      (loop while (and res (md-blank-p (car (car res)))) do (pop res))
      (setf res (nreverse res))
      (values (mapcar #'car res) (mapcar #'cdr res)))))
