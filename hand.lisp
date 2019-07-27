(deftype hand ()
  '(vector (or null card) 5))

(defun make-hand ()
  (make-array '(5) :element-type '(or card null)
                   :initial-element nil
                   :fill-pointer 0))

(defun sort-hand (hand)
  "sort HAND into a (VECTOR CARD 5), where the highest-faced card is
at (AREF HAND 0) and the lowest-faced card is at (AREF HAND 4)"
  (sort (coerce hand 'hand) #'> :key #'card-face))

(defun sorted-hand-high-card (sorted-hand)
  (aref sorted-hand 0))

(defun sorted-hand-low-card (sorted-hand)
  (aref sorted-hand 4))
