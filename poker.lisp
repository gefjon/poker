(in-package :poker)

(defmacro subsequent-do-players ((binding game) &body forms-to-sequence)
  (cons 'progn
        (mapcar #'(lambda (body-form)
                    `(do-players (,binding ,game)
                       ,body-form))
                forms-to-sequence)))

(defmethod bet :around ((player player) (game poker-game))
  (let* ((amount (call-next-method))
         (cash-on-hand (player-cash-on-hand player))
         (total-bet-this-round (+ amount (player-current-bet player)))
         (need-to-match (poker-game-current-bet game)))
    (declare (type money
                   amount
                   cash-on-hand
                   total-bet-this-round
                   need-to-match))
    
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
