(in-package :galaxians)

(defparameter +main-ship-sprite-path+ "images/main_ship.png")
(defparameter +enemy-ship-drone+ "images/Ship_1.png")
(defparameter +enemy-ship-sentry+ "images/Ship_2.png")
(defparameter +enemy-ship-guardian+ "images/Ship_3.png")

(defun asset (path)
  (namestring (asdf:system-relative-pathname "galaxians" (concatenate 'string "resources/" path))))

(defun load-bitmap (path)
  (let ((bitmap (al:load-bitmap (asset path))))
    (unless bitmap
      (error "Error loading ~a" path))
    bitmap))

(defmethod load-sprites! (game-state)
  (setf (sprites-main-ship (sprites game-state)) (load-bitmap +main-ship-sprite-path+))
  (setf (sprites-drone-ship (sprites game-state)) (load-bitmap +enemy-ship-drone+))
  (setf (sprites-sentry-ship (sprites game-state)) (load-bitmap +enemy-ship-sentry+))
  (setf (sprites-guardian-ship (sprites game-state)) (load-bitmap +enemy-ship-guardian+)))

(defun world-to-gfx-x (x-coord)
  (* x-coord +scale+))

(defun world-to-gfx-y (y-coord)
  (* (- +game-screen-height+ y-coord) +scale+))

(defmethod world-to-gfx ((single-point point2d))
  (make-point2d (world-to-gfx-x (point-x single-point))
                (world-to-gfx-y (point-y single-point))))

(defmethod world-to-gfx ((rect rectangle))
  (make-rectangle-by-coords (world-to-gfx-x (x1 rect))
                            (world-to-gfx-y (y1 rect))
                            (world-to-gfx-x (x2 rect))
                            (world-to-gfx-y (y2 rect))))

(defun render-player (game-state)
  (let* ((player-state (player-state game-state))
         (player-rect-gfx (-> player-state
                            position-rect
                            world-to-gfx))
         (half-player-size (/ +player-width+ 2)))
    (al:draw-scaled-bitmap (sprites-main-ship (sprites game-state))
                           8 8
                           32 32
                           (left player-rect-gfx)
                           (bottom player-rect-gfx) ;GFX Y coords are opposite way
                           (rectangle-width player-rect-gfx)
                           (rectangle-height player-rect-gfx)
                           nil)))

(defun enemy-sprite-for (enemy-type game-state)
  (let ((sprites (sprites game-state)))
    (case enemy-type
      (:drone (sprites-drone-ship sprites))
      (:sentry (sprites-sentry-ship sprites))
      (:guardian (sprites-guardian-ship sprites)))))

(defun render-enemies (game-state)
  (loop :with enemies-state = (enemies game-state)
        :for enemy-ship :across (enemy-ship-states enemies-state)
        :for sprite = (enemy-sprite-for (ship-type enemy-ship) game-state)
        :for enemy-ship-rect-gfx = (world-to-gfx (position-rect enemy-ship))
        :do (let* ((cx-gfx (/ (+ (left enemy-ship-rect-gfx) (right enemy-ship-rect-gfx)) 2f0))
                   (cy-gfx (/ (+ (top enemy-ship-rect-gfx) (bottom enemy-ship-rect-gfx)) 2f0))
                   (scale-x (/ (rectangle-width enemy-ship-rect-gfx) 32f0))
                   (scale-y (/ (rectangle-height enemy-ship-rect-gfx) 32f0)))
               (al:draw-scaled-rotated-bitmap sprite
                                              16f0 16f0
                                              cx-gfx cy-gfx
                                              scale-x scale-y
                                              (rotation-angle enemy-ship)
                                              nil))))

(defun render-projectiles (game-state)
  (loop :for projectile :across (projectiles game-state)
        :for projectile-gfx-rect = (world-to-gfx (position-rect projectile))
        :do (al:draw-filled-rectangle (x1 projectile-gfx-rect)
                                      (y1 projectile-gfx-rect)
                                      (x2 projectile-gfx-rect)
                                      (y2 projectile-gfx-rect)
                                      (al:map-rgb 255 255 255))))

(defun render (game-state)
  (al:clear-to-color (al:map-rgb 0 0 0))
  (render-player game-state)
  (render-projectiles game-state)
  (render-enemies game-state)
  (al:draw-line (world-to-gfx-x 16)
                (world-to-gfx-y 0)
                (world-to-gfx-x 16)
                (world-to-gfx-y 200)
                (al:map-rgb 20 20 20)
                1)
  (al:flip-display))

