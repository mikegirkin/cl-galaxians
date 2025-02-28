(in-package :galaxians)

(defun player-to-rectangle (player-position)
  (world-to-gfx
   (make-rectangle :x1 (- (player-position-x player-position) 5)
                   :y1 (- (player-position-y player-position) 5)
                   :x2 (+ (player-position-x player-position) 5)
                   :y2 (+ (player-position-y player-position) 5))))

(defmethod world-to-gfx ((single-coord integer))
  (* single-coord +scale+))

(defmethod world-to-gfx ((rect rectangle))
  (make-rectangle :x1 (world-to-gfx (rectangle-x1 rect))
                  :y1 (world-to-gfx (rectangle-y1 rect))
                  :x2 (world-to-gfx (rectangle-x2 rect))
                  :y2 (world-to-gfx (rectangle-y2 rect))))

(defun render (game-state)
  (al:clear-to-color (al:map-rgb 0 0 0))
  (let ((player-rect (player-to-rectangle (player-position game-state))))
    (al:draw-filled-rectangle (rectangle-x1 player-rect)
                              (rectangle-y1 player-rect)
                              (rectangle-x2 player-rect)
                              (rectangle-y2 player-rect)
                              (al:map-rgb 155 255 255)))
  (al:flip-display))

