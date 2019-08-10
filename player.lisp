(cl:in-package :poker)

(deftype money ()
  'fixnum)

(declaim (ftype (function (player) hand) player-hand))
(declaim (ftype (function (player) money) player-current-bet))
(declaim (ftype (function (player) money) player-cash-on-hand))

(defclass player ()
  ((hand :type hand
         :initarg :player-hand
         :initform (make-hand)
         :accessor player-hand)
   (current-bet :type money
                :initform 0
                :accessor player-current-bet)
   (cash-on-hand :type money
                 :initform #x1000
                 :initarg :cash-on-hand
                 :accessor player-cash-on-hand)))

(declaim (ftype (function (player) money) bet))
(defgeneric bet (player))

(defclass stdio-player (player)
  ((iostream :type stream
             :initform *terminal-io*
             :initarg :iostream
             :accessor player-iostream)))

(defmethod print-object ((it player) stream)
  (print-unreadable-object (it stream :type t :identity t)
    (format stream "hand ~s" (player-hand it))))

(deftype player-array (&optional (length '*))
  `(array player (,length)))

(defun make-player-array (&optional (count 2))
  (make-array (list count)
              :element-type 'player
              :initial-contents (iter (repeat count)
                                      (collect (make-instance 'stdio-player)))))

(defmethod bet ((player stdio-player))
    (with-slots (iostream current-bet cash-on-hand) player
      (format iostream
              "~&~a has ~a on hand and has already bet ~a.~%"
              player cash-on-hand current-bet)
      (format iostream "~&enter an amount for ~a to bet: " player)
      (let ((amount (read iostream)))
        (check-type amount money)
        (decf cash-on-hand amount)
        (incf current-bet amount))))

(defgeneric discard-cards (player))

(defmethod discard-cards ((player stdio-player))
  (with-slots (iostream) player
    (flet ((replace-card-p (card)
             (let ((*query-io* iostream))
               (y-or-n-p "discard ~a?" card))))
      (setf (player-hand player)
            (delete-if #'replace-card-p (player-hand player))))))

