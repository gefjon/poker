(cl:in-package :poker)

(declaim (ftype (function (poker-game) money) poker-game-current-bet))
(declaim (ftype (function (poker-game) deck) poker-game-deck))
(declaim (ftype (function (poker-game) (array player)) poker-game-players))

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

(defun vector-size (vector)
  "like CL:LENGTH, but ignores fill pointer"
  (array-dimension vector 0))

(defun draw-hand (player game)
  (with-slots (hand) player
    (with-slots (deck) game
      (iter (repeat (- (vector-size hand)
                       (fill-pointer hand)))
            (vector-push (vector-pop deck)
                         hand)))))

(defmacro do-players ((binding game) &body body)
  `(iter (for ,binding in-vector (poker-game-players ,game))
         ,@body))
