
(DEFCONSTANT +UI-FG-MASK+ 15) 
(DEFCONSTANT +UI-BG-MASK+ 240) 
(DEFUN UI-ATTR (FG &KEY BRIGHT BG BGBRIGHT)
  (LOGIOR (LOGAND FG 7)
          (IF BRIGHT
              8
              0)
          (IF BG
              (ASH (1+ (LOGAND BG 7)) 4)
              0)
          (IF BGBRIGHT
              128
              0))) 
(DEFUN UI-ATTR-CODES (ATTR)
  (LET ((FG (LOGAND ATTR 7))
        (FB (LOGBITP 3 ATTR))
        (BG (ASH ATTR -4))
        (BB (LOGBITP 7 ATTR))
        (CODES NIL))
    (WHEN (PLUSP BG)
      (PUSH
       (+ 39 BG
          (IF BB
              60
              0))
       CODES))
    (PUSH
     (+ 30 FG
        (IF FB
            60
            0))
     CODES)
    (NREVERSE CODES))) 
(DEFUN UI-ATTR-STRING (ATTR PREV)
  (IF (= ATTR PREV)
      ""
      (FORMAT NIL "~C[0m~C[~{~d~^;~}m" #\Esc #\Esc (UI-ATTR-CODES ATTR)))) 
(DEFUN UI-C256 (N)
  (COND ((< N 16) N)
        ((< N 232)
         (LET* ((M (- N 16))
                (R (FLOOR M 36))
                (G (FLOOR (MOD M 36) 6))
                (B (MOD M 6))
                (HI (MAX R G B))
                (LO (MIN R G B)))
           (IF (<= (- HI LO) 1)
               (IF (< (/ (+ R G B) 3.0) 2.5)
                   0
                   7)
               (+
                (COND ((AND (>= R G) (>= G B)) 3) ((AND (>= G R) (>= R B)) 2)
                      ((AND (>= R B) (>= B G)) 1) ((AND (>= G B) (>= B R)) 2)
                      ((AND (>= B R) (>= R G)) 5) (T 4))
                (IF (>= HI 4)
                    8
                    0)))))
        (T
         (IF (> (- N 232) 12)
             15
             8)))) 
(DEFUN UI-SGR-APPLY (PARAMS ATTR)
  (LET ((PS PARAMS))
    (LOOP WHILE PS
          DO (LET ((P (POP PS)))
               (COND ((= P 0) (SETF ATTR 7))
                     ((= P 1) (SETF ATTR (LOGIOR ATTR 8)))
                     ((= P 22) (SETF ATTR (LOGAND ATTR (LOGNOT 8))))
                     ((= P 39)
                      (SETF ATTR
                              (LOGIOR (LOGAND ATTR (LOGNOT +UI-FG-MASK+)) 7)))
                     ((= P 49) (SETF ATTR (LOGAND ATTR (LOGNOT +UI-BG-MASK+))))
                     ((<= 30 P 37)
                      (SETF ATTR
                              (LOGIOR (LOGAND ATTR (LOGNOT +UI-FG-MASK+))
                                      (- P 30))))
                     ((<= 40 P 47)
                      (SETF ATTR
                              (LOGIOR (LOGAND ATTR (LOGNOT +UI-BG-MASK+))
                                      (ASH (1+ (- P 40)) 4))))
                     ((<= 90 P 97)
                      (SETF ATTR
                              (LOGIOR (LOGAND ATTR (LOGNOT +UI-FG-MASK+))
                                      (+ 8 (- P 90)))))
                     ((<= 100 P 107)
                      (SETF ATTR
                              (LOGIOR (LOGAND ATTR (LOGNOT +UI-BG-MASK+))
                                      (ASH (1+ (- P 100)) 4) 128)))
                     ((OR (= P 38) (= P 48))
                      (LET* ((FGP (= P 38))
                             (MASK
                              (IF FGP
                                  +UI-FG-MASK+
                                  +UI-BG-MASK+))
                             (MODE (AND PS (POP PS))))
                        (COND
                         ((AND MODE (= MODE 5) PS)
                          (LET ((C (UI-C256 (POP PS))))
                            (SETF ATTR
                                    (LOGIOR (LOGAND ATTR (LOGNOT MASK))
                                            (IF FGP
                                                C
                                                (LOGIOR (ASH (1+ C) 4)
                                                        128))))))
                         ((AND MODE (= MODE 2))
                          (DOTIMES (K 3) (WHEN PS (POP PS)))))))
                     (T NIL))))
    ATTR)) 
(DEFUN UI-SPLIT-PARAMS (S)
  (IF (ZEROP (LENGTH S))
      (LIST 0)
      (LOOP WITH START = 0
            FOR I = (POSITION #\; S :START START)
            COLLECT (PARSE-INTEGER S :START START :END (OR I (LENGTH S))
                                   :JUNK-ALLOWED T)
            WHILE I
            DO (SETF START (1+ I))))) 
(DEFUN STR-REPLACE (NEW OLD S)
  (WITH-OUTPUT-TO-STRING (OUT)
    (LET ((START 0))
      (LOOP FOR I = (SEARCH OLD S :START2 START)
            WHILE I
            DO (WRITE-STRING S OUT :START START :END I) (WRITE-STRING NEW
                                                                      OUT) (SETF START
                                                                                   (+
                                                                                    I
                                                                                    (LENGTH
                                                                                     OLD))))
      (WRITE-STRING S OUT :START START)))) 
(DEFUN UI-PARSE-ANSI (LINE)
  (LET ((CHUNKS NIL)
        (ATTR 7)
        (CUR (MAKE-STRING-OUTPUT-STREAM))
        (I 0)
        (N (LENGTH LINE)))
    (LABELS ((FLUSH ()
               (LET ((S (GET-OUTPUT-STREAM-STRING CUR)))
                 (WHEN (PLUSP (LENGTH S)) (PUSH (CONS S ATTR) CHUNKS)))))
      (LOOP WHILE (< I N)
            DO (LET ((CH (CHAR LINE I)))
                 (COND
                  ((CHAR= CH #\Esc)
                   (LET ((J (1+ I)))
                     (IF (AND (< J N) (CHAR= (CHAR LINE J) #\[))
                         (LET ((K (1+ J)))
                           (LOOP WHILE (AND (< K N)
                                            (NOT (ALPHA-CHAR-P (CHAR LINE K))))
                                 DO (INCF K))
                           (WHEN (< K N)
                             (WHEN (CHAR= (CHAR LINE K) #\m)
                               (FLUSH)
                               (SETF ATTR
                                       (UI-SGR-APPLY
                                        (UI-SPLIT-PARAMS
                                         (SUBSEQ LINE (1+ J) K))
                                        ATTR)))
                             (SETF I (1+ K))))
                         (SETF I (1+ J)))))
                  ((OR (CHAR= CH #\Return) (CHAR= CH #\Nul)) (INCF I))
                  (T (WRITE-CHAR CH CUR) (INCF I)))))
      (FLUSH))
    (OR (NREVERSE CHUNKS) (LIST (CONS "" 7))))) (progn (load (merge-pathnames "state/ui.lisp" *root*)))

(require :sb-posix)

