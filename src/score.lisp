(in-package :poker)

(defmacro define-score-type (name &body bytes-in-increasing-order)
  "Define a type named NAME which encodes a score as a bignum, with
fields for each of the BYTES-IN-INCREASING-ORDER. Also, for each
symbol FOO in BYTES-IN-INCREASING-ORDER, define functions FOO-LDB and
FOO-DPB, which act analogously to LDB and DPB except that they do not
take a bytespec; and *FOO-BYTESPEC*, which holds a bytespec that can
be passed to LDB and DPB."
  (let ((fill-pointer 0))
    (labels ((sym-to-string
                 (sym-or-string)
               (cond ((stringp sym-or-string) sym-or-string)
                     ((symbolp sym-or-string) (symbol-name sym-or-string))
                     (t (error "not a symbol or a string: ~s" sym-or-string))))
             
             (symbol-concat
                 (&rest syms-or-strings)
               (let ((strings (map 'list #'sym-to-string syms-or-strings)))
                 (intern (apply #'concatenate 'string strings))))

             (expand-byte-name
                 (byte)
               (let ((load-byte (symbol-concat byte "-LDB"))
                     (deposit-byte (symbol-concat byte "-DPB"))
                     ;; (set-byte (symbol-concat "SET-" byte))
                     (bytespec (symbol-concat "*" byte "-BYTESPEC*")))
                 (prog1
                     `(progn
                        (defvar ,bytespec
                          (byte ,*face-byte-width* ,fill-pointer))
                        (defun ,load-byte (score)
                          (ldb ,bytespec score))
                        (defun ,deposit-byte (newbyte score)
                          (dpb newbyte ,bytespec score)))
                    (setf fill-pointer
                          (+ fill-pointer
                             *face-byte-width*))))))
      
      `(progn 
         ,@(map 'list #'expand-byte-name bytes-in-increasing-order)
         (deftype ,name ()
           '(unsigned-byte ,fill-pointer))))))

(define-score-type score
  low-card
  4th-card
  3rd-card
  2nd-card
  high-card
  2-of
  higher-pair
  3-of
  straight
  flush
  full-house
  4-of
  straight-flush)

(declaim (ftype (function (hand)
                          list)
                face-histogram-for-hand))
(defun face-histogram-for-hand (hand)
  "Return an alist whose CARs are face-values and CDRs are number of
appearances in HAND"
  (declare (type hand hand))
  (iterate (for card in-vector hand)
           (with histogram = '())
           (after-each
            (let* ((face (card-face card))
                   (old-count (or (cdr (assoc face histogram)) 0)))
              (setf histogram
                    (acons face (1+ old-count) histogram))))
           (finally (return (remove-duplicates histogram
                                               :key #'car
                                               :from-end t)))))

(declaim (ftype (function (hand)
                          (or null face))
                hand-is-a-straight))
(defun hand-straight-value (hand)
  ;; note: hand is already sorted by the time it gets here
  (declare (type hand hand))
  (dotimes (i 4
            (card-face (sorted-hand-high-card hand)))
    (unless (eq (card-face (aref hand i))
                (1+ (card-face (aref hand (1+ i)))))
      (return nil))))

(defun find-higher-pair (existing-pair-face histogram)
  "If HISTOGRAM contains two distinct pairs, given the lower one as
EXISTING-PAIR-FACE, find the higher pair's face value. Otherwise,
return NIL"
  (when (and existing-pair-face histogram)
    (flet ((same-face-p (pair)
             (= (car pair) existing-pair-face)))
      (let* ((hand-except-first-pair (remove-if #'same-face-p
                                                histogram)))
        (car (rassoc 2 hand-except-first-pair))))))

(declaim (ftype (function (hand)
                          (or null face))
                hand-flush-value))
(defun hand-flush-value (hand)
  "If HAND (which must be sorted) is a flush, return the face-value
for the flush place of a score, or NIL otherwise."
  (dotimes (i 4
            (the face
                 (card-face (sorted-hand-high-card hand))))
    (unless (eq (card-suit (aref hand i))
                (card-suit (aref hand (1+ i))))
      (return nil))))

(declaim (ftype (function (hand)
                          score)
                hand-score))
(defun hand-score (hand)
  "Calculate an integer representing the score of HAND. For any two
poker hands A and B, A beats B => (> (HAND-SCORE A) (HAND-SCORE B)), A
ties B => (= (HAND-SCORE A) (HAND-SCORE B)), and B beats A
=> (< (HAND-SCORE A) (HAND-SCORE B))."
  (let* ((sorted (sort-hand hand))
         (high-card (card-face (aref sorted 0)))
         (2nd-card (card-face (aref sorted 1)))
         (3rd-card (card-face (aref sorted 2)))
         (4th-card (card-face (aref sorted 3)))
         (5th-card (card-face (aref sorted 4)))
         (histogram (face-histogram-for-hand sorted))
         (score 0)
         (4-of (car (rassoc 4 histogram)))
         (3-of (car (rassoc 3 histogram)))
         (2-of (car (rassoc 2 histogram)))
         (higher-pair (find-higher-pair 2-of histogram))
         (straight (hand-straight-value sorted))
         (flush (hand-flush-value sorted))
         (straight-flush (when (and straight flush) straight))
         (full-house (when (and 2-of 3-of) 3-of)))
    (setf (ldb *low-card-bytespec* score) 5th-card)
    (setf (ldb *4th-card-bytespec* score) 4th-card)
    (setf (ldb *3rd-card-bytespec* score) 3rd-card)
    (setf (ldb *2nd-card-bytespec* score) 2nd-card)
    (setf (ldb *high-card-bytespec* score) high-card)
    (setf (ldb *2-of-bytespec* score) (or 2-of 0))
    (setf (ldb *higher-pair-bytespec* score) (or higher-pair 0))
    (setf (ldb *3-of-bytespec* score) (or 3-of 0))
    (setf (ldb *straight-bytespec* score) (or straight 0))
    (setf (ldb *flush-bytespec* score) (or flush 0))
    (setf (ldb *full-house-bytespec* score) (or full-house 0))
    (setf (ldb *4-of-bytespec* score) (or 4-of 0))
    (setf (ldb *straight-flush-bytespec* score) (or straight-flush 0))
     score))
