(in-package :poker)

(deftype suit ()
  '(member
    :heart
    :diamond
    :club
    :spade))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *face-byte-width* 8
    "Face values are represented internally as a byte, which allows
  scores to be represented as a byte-vector, or, interchangably, a
  bignum."))

(deftype face ()
  `(unsigned-byte ,*face-byte-width*))

(defclass card ()
  ((suit :initarg :suit
         :accessor card-suit
         :type suit)
   (face :initarg :face
          :accessor card-face
          :type face)))

(defun make-card (suit face)
  (check-type suit suit)
  (check-type face face)
  (make-instance 'card
                 :suit suit
                 :face face))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type t)
    (with-slots (suit face) card
      (format stream "the ~d of ~as" face suit))))
