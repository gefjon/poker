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

(defun realize-bet (bet player)
  (with-slots (current-bet cash-on-hand) player
    (let ((delta (- bet current-bet)))
      (when (> delta cash-on-hand)
        (error "can't afford to bet ~s with only ~s" delta cash-on-hand))
      (setf current-bet bet)
      (decf cash-on-hand delta))))

(defun handle-player-bet-and-raise-p (player bet to-match)
  "returns T if this player raised or NIL if they did not"
  (cond
    ((> bet to-match)
     (realize-bet bet player)
     t)
    ((= bet to-match)
     (realize-bet bet player)
     nil)
    (t
     (setf (player-still-playing-p player) nil)
     nil)))

(defun modular-1+ (n base)
  (rem (1+ n) base))

(defun round-of-betting (game)
  (with-slots (current-bet
               deck
               players)
      game
    (let ((number-of-players (length players)))
      (iter (for players-to-bet
                 downfrom number-of-players)

            (until (= players-to-bet 0))
            
            (for current-betting-player
                 first 0
                 then (modular-1+ current-betting-player number-of-players))

            (for player = (aref players current-betting-player))
            
            (unless (player-still-playing-p player)
              (next-iteration))
            
            (for their-bet = (bet player))
            
            (when (handle-player-bet-and-raise-p player
                                                 their-bet
                                                 current-bet)
              (setf current-bet their-bet
                    players-to-bet number-of-players))))))

(defun discard-round (game)
  (do-players (player game)
    (discard-cards player)
    (draw-hand player game)))

(defun choose-winner-and-prize (game)
  (iter (for player in-vector (poker-game-players game))
        (finding player
                 maximizing (hand-score (player-hand player))
                 into winner)
        (sum (player-current-bet player)
             into prize)
        (finally (return (values winner prize)))))

(defun reset-game-for-new-hand (game)
  (with-slots (deck players current-bet) game
    (setf current-bet 0)
    (reset-deck deck)
    (shuffle-deck deck))
  (do-players (player game)
    (with-slots (current-bet hand still-playing-p) player
      (setf current-bet 0)
      (setf still-playing-p t)
      (make-vector-empty hand))))

(defun decide-hand (game)
  (multiple-value-bind (winner prize) (choose-winner-and-prize game)
    (incf (player-cash-on-hand winner) prize)
    (values winner prize)))

(defun play-hand (game)
  (reset-game-for-new-hand game)

  (do-players (player game)
    (draw-hand player game))

  (round-of-betting game)

  (discard-round game)

  (round-of-betting game)

  (decide-hand game))
