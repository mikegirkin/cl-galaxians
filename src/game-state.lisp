(in-package :galaxians)

(defstruct game-state
  (player-position (make-player-position :x 50 :y 50) :type player-position))

(defmethod move-player* ((game-state game-state)
                         (dx integer)
                         (dy integer))
  (incf (player-position-x (game-state-player-position game-state)) dx)
  (incf (player-position-y (game-state-player-position game-state)) dy))
