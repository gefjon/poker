(in-package :poker)

(deftype deck ()
  `(vector (or card null) 52))

(defun make-vector-empty (vector)
  (setf (fill-pointer vector) 0))

(defun reset-deck (deck)
  (make-vector-empty deck)
  (iterate
    (for suit in '(:heart :diamond :spade :club))
    (iterate (for face from 2 to 14)
             (after-each (vector-push (make-card suit face) deck)))
    (finally (return deck))))

(declaim (ftype (function () deck)
                make-empty-deck
                make-deck))
(defun make-empty-deck ()
  (make-array 52
              :element-type '(or null card)
              :initial-element nil
              :fill-pointer 0))

(defun make-deck ()
  (reset-deck (make-empty-deck)))

(declaim (ftype (function (deck) deck) shuffle-deck))
(defun shuffle-deck (deck)
  (check-type deck deck)
  (let ((n (length deck)))
    (iter
      (for i from (1- n) downto 1)
      (let* ((j (random (coerce (1+ i) 'fixnum)))
             (ith (aref deck i))
             (jth (aref deck j)))
        (setf (aref deck i) jth)
        (setf (aref deck j) ith))))
  deck)
