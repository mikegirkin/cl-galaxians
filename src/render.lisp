(in-package :galaxians)

(defun render-player (game-state)
  (let* ((player-position (player-position game-state))
         (half-player-size (/ +player-width+ 2)))
    (al:draw-scaled-bitmap (sprites-main-ship (sprites game-state))
                           8 8
                           32 32
                           (world-to-gfx (- (player-position-x player-position) half-player-size))
                           (world-to-gfx (- (player-position-y player-position) half-player-size))
                           (world-to-gfx +player-width+)
                           (world-to-gfx +player-width+)
                           nil)))

(defmethod world-to-gfx ((single-coord integer))
  (* single-coord +scale+))

(defmethod world-to-gfx ((rect rectangle))
  (make-rectangle :x1 (world-to-gfx (rectangle-x1 rect))
                  :y1 (world-to-gfx (rectangle-y1 rect))
                  :x2 (world-to-gfx (rectangle-x2 rect))
                  :y2 (world-to-gfx (rectangle-y2 rect))))

(defun render (game-state)
  (al:clear-to-color (al:map-rgb 0 0 0))
  (render-player game-state)
  (al:draw-line (world-to-gfx 16)
                (world-to-gfx 0)
                (world-to-gfx 16)
                (world-to-gfx 200)
                (al:map-rgb 20 20 20)
                1)
  (al:flip-display))

