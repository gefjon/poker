(defpackage :poker.card
  (:use :cl)
  (:nicknames :card)
  (:export :suit
           :face
           :*face-byte-width*
           :make-card
           :card
           :card-face
           :card-suit))
(in-package :poker.card)

(deftype suit ()
  '(member
    :heart
    :diamond
    :club
    :spade))

(defparameter *face-byte-width* 8)
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
