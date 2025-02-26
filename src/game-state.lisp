(in-package :galaxians)

(defconstant +scale+ 3)

(defmethod world-to-gfx ((single-coord integer))
  (* single-coord +scale+))

(defmethod world-to-gfx ((rect rectangle))
  (make-rectangle :x1 (world-to-gfx (rectangle-x1 rect))
                  :y1 (world-to-gfx (rectangle-y1 rect))
                  :x2 (world-to-gfx (rectangle-x2 rect))
                  :y2 (world-to-gfx (rectangle-y2 rect))))

(defstruct player-position
  (x 0 :type integer)
  (y 0 :type integer))

(defstruct rectangle x1 y1 x2 y2)

(defun player-to-rectangle (player-position)
  (world-to-gfx
   (make-rectangle :x1 (- (player-position-x player-position) 5)
                   :y1 (- (player-position-y player-position) 5)
                   :x2 (+ (player-position-x player-position) 5)
                   :y2 (+ (player-position-y player-position) 5))))

(defstruct game-state
  (player-position (make-player-position :x 160 :y 120) :type player-position))

(defmethod move-player* ((game-state game-state)
                         (dx integer)
                         (dy integer))
  (incf (player-position-x (game-state-player-position game-state)) dx)
  (incf (player-position-y (game-state-player-position game-state)) dy))

