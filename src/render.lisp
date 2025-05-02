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
      (error (format t "Error loading ~a" path)))
    bitmap))

(defmethod load-sprites! (game-state)
  (setf (sprites-main-ship (sprites game-state)) (load-bitmap +main-ship-sprite-path+))
  (setf (sprites-drone-ship (sprites game-state)) (load-bitmap +enemy-ship-drone+))
  (setf (sprites-sentry-ship (sprites game-state)) (load-bitmap +enemy-ship-sentry+))
  (setf (sprites-guardian-ship (sprites game-state)) (load-bitmap +enemy-ship-guardian+)))

(defun render-player (game-state)
  (let* ((player-state (player-state game-state))
         (half-player-size (/ +player-width+ 2)))
    (al:draw-scaled-bitmap (sprites-main-ship (sprites game-state))
                           8 8
                           32 32
                           (world-to-gfx (- (x player-state) half-player-size))
                           (world-to-gfx (- (y player-state) half-player-size))
                           (world-to-gfx +player-width+)
                           (world-to-gfx +player-width+)
                           nil)))

(defun enemy-sprite-for (enemy-type game-state)
  (let ((sprites (sprites game-state)))
    (case enemy-type
      (:drone (sprites-drone-ship sprites))
      (:sentry (sprites-sentry-ship sprites))
      (:guardian (sprites-guardian-ship sprites)))))

(defun render-enemies (game-state)
  (loop :for enemy-ship :across (enemies game-state)
        :for sprite = (enemy-sprite-for (ship-type enemy-ship) game-state)
        :do (al:draw-scaled-bitmap sprite
                                   0 0
                                   32 32
                                   (world-to-gfx (x1 (ship-position enemy-ship)))
                                   (world-to-gfx (y1 (ship-position enemy-ship)))
                                   (world-to-gfx (rectangle-width (ship-position enemy-ship)))
                                   (world-to-gfx (rectangle-height (ship-position enemy-ship)))
                                   nil
                                   )))

(defmethod world-to-gfx ((single-coord integer))
  (* single-coord +scale+))

(defmethod world-to-gfx ((rect geometry:rectangle))
  (make-rectangle-by-coords (world-to-gfx (x1 rect))
                            (world-to-gfx (y1 rect))
                            (world-to-gfx (x2 rect))
                            (world-to-gfx (y2 rect))))

(defun render (game-state)
  (al:clear-to-color (al:map-rgb 0 0 0))
  (render-player game-state)
  (render-enemies game-state)
  (al:draw-line (world-to-gfx 16)
                (world-to-gfx 0)
                (world-to-gfx 16)
                (world-to-gfx 200)
                (al:map-rgb 20 20 20)
                1)
  (al:flip-display))

