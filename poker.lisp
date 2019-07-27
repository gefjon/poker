(in-package :poker)

(defvar *poker-output* *standard-output*)
(defvar *poker-input* *standard-input*)

(deftype money ()
  'fixnum)

(defclass player ()
  ((hand :type hand
         :initarg :player-hand
         :initform (make-hand)
         :accessor player-hand)
   (current-bet :type money
                :initform 0
                :accessor player-current-bet)
   (cash-on-hand :type fixnum
                 :initform #x1000
                 :initarg :cash-on-hand
                 :accessor player-cash-on-hand)))

(defclass stdio-player (player) ())

(defmethod print-object ((it player) stream)
  (print-unreadable-object (it stream :type t :identity t)
    (format stream "hand ~s" (player-hand it))))

(deftype player-array (&optional (length '*))
  `(array player (,length)))

(defun make-player-array (&optional (count 2))
  (make-array (list count)
              :element-type 'player
              :initial-contents (iter (repeat count)
                                      (collect (make-instance 'player)))))

(defclass poker-game ()
  ((current-bet :type money
                :initarg :current-bet
                :initform 0
                :accessor poker-game-current-bet)
   (deck :type deck
         :initarg :deck
         :initform (make-deck)
         :accessor poker-game-deck)
   (players :type (array player)
            :initform (make-player-array)
            :initarg :players
            :accessor poker-game-players)))

(defun draw-hand (player game)
  (with-slots (hand) player
    (with-slots (deck) game
      (iter (repeat (- (length hand)
                       (fill-pointer hand)))
            (vector-push (vector-pop deck)
                         hand)))))

(defmacro do-players ((binding game) &body body)
  `(iter (for ,binding in-vector (poker-game-players ,game))
         (progn ,@body)))

(defmacro subsequent-do-players ((binding game) &body forms-to-sequence)
  (cons 'progn
        (mapcar #'(lambda (body-form)
                    `(do-players (,binding ,game)
                       ,body-form))
                forms-to-sequence)))

(defgeneric bet (player game))

(defmethod bet ((player stdio-player) game)
    (with-slots (current-bet cash-on-hand) player
      (format *poker-output*
              "~&~a has ~a on hand and has already bet ~a.~%"
              player cash-on-hand current-bet)
      (format *poker-output* "~&enter an amount for ~a to bet: " player)
      (let ((amount (read *poker-input*)))
        (check-type amount money)
        (decf cash-on-hand amount)
        (incf current-bet amount))))

(defmethod bet :around ((player player) (game poker-game))
    (let* ((amount (call-next-method))
           (cash-on-hand (player-cash-on-hand player))
           (total-bet-this-round (+ amount (player-current-bet player)))
           (need-to-match (poker-game-current-bet game)))
      
      (when (> amount cash-on-hand)
        (error "can't afford to bet ~a when you have only ~a" amount cash-on-hand))
      
      (when (< total-bet-this-round need-to-match)
        (error "you must at least match ~a but you have only bet ~a"
               need-to-match total-bet-this-round))
      
      (when (> total-bet-this-round need-to-match)
        (setf (poker-game-current-bet game) total-bet-this-round))
      
      (decf (player-cash-on-hand player) amount)
      (setf (player-current-bet player) total-bet-this-round)
      
      amount))

(defgeneric replace-cards (player game))

(defmethod replace-cards ((player stdio-player) (game poker-game))
  (flet ((replace-card-p (card)
           (y-or-n-p "discard ~a?" card)))
    (setf (player-hand player)
          (delete-if #'replace-card-p (player-hand player))))
  (draw-hand player game))

(defun calculate-scores-and-choose-winner (game)
  (declare (ignorable game))
  (error "write some stupid code, idiot."))

(defun reset-game (game)
  (reset-deck (poker-game-deck game))
  (do-players (player game)
    (make-vector-empty (player-hand player))))

(defun play-hand (game)
  (reset-game game)
  (shuffle-deck (poker-game-deck game))
  (subsequent-do-players (player game)
    (draw-hand player game)
    (bet player game)
    (replace-cards player game)
    (bet player game))
  (calculate-scores-and-choose-winner game))
