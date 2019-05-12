(defpackage :poker
  (:use :cl :iterate :poker.card :poker.score))
(in-package :poker)

(defun make-deck ()
  (iterate
    (with deck = (make-array 52
                             :element-type '(or null card)
                             :initial-element nil
                             :fill-pointer 0))
    (for suit in '(:heart :diamond :spade :club))
    (iterate (for face from 2 to 14)
             (after-each (vector-push (make-card suit face) deck)))
    (finally (return deck))))
