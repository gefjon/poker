(defpackage :poker
  (:use :cl :iterate))
(in-package :poker)

(defclass card ()
  ((suit :initarg :suit
         :accessor card-suit)
   (value :initarg :value
          :accessor card-value)))

(defun make-card (suit value)
  (make-instance 'card
                 :suit suit
                 :value value))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type t)
    (with-slots (suit value) card
      (format stream "the ~d of ~as" value suit))))

(defun make-deck ()
  (iterate
    (with deck = (make-array 52
                             :element-type '(or null card)
                             :initial-element nil
                             :fill-pointer 0))
    (for suit in '(:heart :diamond :spade :club))
    (iterate (for value from 2 to 14)
             (after-each (vector-push (make-card suit value) deck)))
    (finally (return deck))))

(deftype hand ()
  '(vector card 5))

(defun sort-hand (hand)
  (sort (coerce hand 'hand) #'> :key #'card-value))
(defun sorted-hand-high-card (sorted-hand)
  (aref sorted-hand 0))
(defun sorted-hand-low-card (sorted-hand)
  (aref sorted-hand 4))

(defclass valuation ()
  ((cards :initarg :cards
          :accessor valuation-cards
          :type hand)))

(defclass tuple (valuation)
  ((value :initarg :value
          :accessor tuple-value)))

(defclass 4-of-a-kind (tuple)
  ())

(defun 4-of-a-kind-p (hand)
  (typep hand '4-of-a-kind))

(defclass 3-of-a-kind (tuple)
  ())

(defun 3-of-a-kind-p (hand)
  (typep hand '3-of-a-kind))

(defclass full-house (valuation)
  ((3-of :initarg :3-of
         :accessor full-house-3-of)
   (2-of :initarg :2-of
         :accessor full-house-2-of)))

(defun full-house-p (hand)
  (typep hand 'full-house))

(defclass pair (tuple)
  ())

(defun pairp (hand)
  (typep hand 'pair))

(defclass 2-pair (valuation)
  ((higher :initarg :higher
           :accessor 2-pair-higher)
   (lower :initarg :lower
          :accessor 2-pair-lower)))

(defun 2-pair-p (hand)
  (typep hand '2-pair))

(defclass flush (valuation)
  ())

(defun flushp (hand)
  (typep hand 'flush))

(defclass straight (valuation)
  ((top :initarg :top
        :accessor straight-top)))

(defun straightp (hand)
  (typep hand 'straight))

(defclass straight-flush (straight flush)
  ())

(defun straight-flush-p (hand)
  (typep hand 'straight-flush))

(defun valuation-high-card (poker-hand)
  (with-slots (cards) poker-hand
    (aref cards 0)))

(defun make-poker-hand (cards)
  (let* ((vector (coerce cards 'hand))
         (sorted (sort vector #'> :key #'card-value)))
    (or (hand-is-a-tuple sorted)
        (let* ((flush-suit (hand-is-a-flush sorted))
               (straight-top (hand-is-a-straight sorted)))
          (cond ((and flush-suit straight-top)
                 (make-instance 'straight-flush
                                :top straight-top
                                :cards sorted))
                (flush-suit (make-instance 'flush
                                           :cards sorted))
                (straight-top (make-instance 'straight
                                             :top straight-top
                                             :cards sorted))
                (t (make-instance 'valuation :cards sorted)))))))

(defun value-histogram-for-hand (hand)
  (declare (type hand hand))
  (iterate (for card in-vector hand)
           (with histogram = '())
           (after-each
            (let* ((value (card-value card))
                   (old-count (or (cdr (assoc value histogram)) 0)))
              (setf histogram
                    (acons value (1+ old-count) histogram))))
           (finally (return histogram))))

(defun hand-is-a-tuple (hand)
  (declare (type hand hand))
  (let ((histogram (value-histogram-for-hand hand)))
    (let ((4-of (car (rassoc 4 histogram)))
          (3-of (car (rassoc 3 histogram)))
          (2-of (car (rassoc 2 histogram))))
      (cond (4-of (make-instance '4-of-a-kind
                                 :value 4-of
                                 :cards hand))
            ((and 3-of 2-of) (make-instance 'full-house
                                            :3-of 3-of
                                            :2-of 2-of
                                            :cards hand))
            (2-of (flet ((same-value-p (pair)
                           (= (car pair) 2-of)))
                    (let* ((hand-except-first-pair (remove-if #'same-value-p
                                                             histogram))
                           (higher-pair (car
                                         (rassoc 2 hand-except-first-pair))))
                      (if higher-pair (make-instance '2-pair
                                                     :lower 2-of
                                                     :higher higher-pair
                                                     :cards hand)
                          (make-instance 'pair
                                         :value 2-of
                                         :cards hand)))))))))

(defun hand-is-a-flush (hand)
  (declare (type hand hand))
  (dotimes (i 1 (card-suit (aref hand 0)))
    (unless (eq (card-suit (aref hand i))
                (card-suit (aref hand (1+ i))))
      (return nil))))

(defun hand-is-a-straight (hand)
  ;; note: hand is already sorted by the time it gets here
  (declare (type hand hand))
  (let ((top-value (card-value (sorted-hand-high-card hand)))
        (low-value (card-value (sorted-hand-low-card hand))))
    (when (= 4 (- top-value
                  low-value))
      top-value)))

(defun valuation-high-card-value (valuation)
  (card-value (valuation-high-card valuation)))

(defun compare (left right &key (key #'identity) fallthrough)
  ;; mostly, i don't use the fallthrough, which is probably good,
  ;; because it leads to a lot of recursive calls
  (let ((left-v (funcall key left))
        (right-v (funcall key right)))
    (cond ((> left-v right-v) :left)
          ((< left-v right-v) :right)
          ((= left-v right-v) (if fallthrough
                                  (compare left right :key fallthrough)
                                  :tie)))))

(defun compare-poker-hands (left right)
  (cond ((and (straight-flush-p left)
              (straight-flush-p right))
         (compare left right :key #'valuation-high-card-value))
        
        ((straight-flush-p left) :left)
        ((straight-flush-p right) :right)

        ((and (4-of-a-kind-p left)
              (4-of-a-kind-p right))
         (compare left right :key #'tuple-value))

        ((4-of-a-kind-p left) :left)
        ((4-of-a-kind-p right) :right)

        ((and (full-house-p left)
              (full-house-p right))
         (compare left right
                  :key #'full-house-3-of
                  :fallthrough #'full-house-2-of))

        ((full-house-p left) :left)
        ((full-house-p right) :right)

        ((and (flushp left)
              (flushp right))
         (compare left right :key #'valuation-high-card-value))

        ((flushp left) :left)
        ((flushp right) :right)

        ((and (straightp left)
              (straightp right))
         (compare left right :key #'straight-top))

        ((straightp left) :left)
        ((straightp right) :right)

        ((and (3-of-a-kind-p left)
              (3-of-a-kind-p right))
         (compare left right :key #'tuple-value))

        ((3-of-a-kind-p left) :left)
        ((3-of-a-kind-p right) :right)

        ((and (2-pair-p left)
              (2-pair-p right))
         (compare left right :key #'2-pair-higher
                  :fallthrough #'2-pair-lower))

        ((2-pair-p left) :left)
        ((2-pair-p right) :right)

        ((and (pairp left)
              (pairp right))
         (compare left right :key #'tuple-value))

        ((pairp left) :left)
        ((pairp right) :right)

        (t (compare left right :key #'valuation-high-card-value))))
