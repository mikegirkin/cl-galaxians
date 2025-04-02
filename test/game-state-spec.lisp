(in-package :galaxians-spec)

(def-suite* game-state-spec)
(in-suite game-state-spec)

(test make-rectangle-by-coords
  (let ((rect (g::make-rectangle-by-coords 10 20 30 40)))
    (is (= (g::x1 rect) 10))
    (is (= (g::y1 rect) 20))
    (is (= (g::x2 rect) 30))
    (is (= (g::y2 rect) 40))))

(test make-rectangle-by-size
  (let ((rect (g::make-rectangle-by-size 10 20 20 30)))
    (is (= (g::x1 rect) 10))
    (is (= (g::y1 rect) 20))
    (is (= (g::x2 rect) 30))
    (is (= (g::y2 rect) 50))))

(test new-player-projectile
  (let* ((player-state (g::make-player-state 20 20))
         (movement-vector (g::make-vector2d 0 10))
         (projectile (g::new-player-projectile player-state movement-vector)))
    (is (g::is-player-owned projectile))
    (is (g::vector2d= (g::movement-vector projectile) movement-vector))
    (is (g::rectangle= (g::position-rect projectile)
                       (g::make-rectangle-by-size 28 20 1 3)))))

(test player-fires-if-reload-time-passed
  (let* ((game-state (g::make-initial-game-state))
         (_1 (setf (g::fire (g::requested-player-actions game-state)) t))
         (_1 (g::player-fire* game-state))
         (added-projectile (elt (g::projectiles game-state) 0)))
    (is (g::rectangle= (g::position-rect added-projectile)
                       (g::make-rectangle-by-coords 108 150 109 153)))))

(test player-cant-fire-when-reloading
  (let* ((game-state (g::make-initial-game-state))
         (_ (setf (g::reload-time-left game-state) 10))
         (_ (setf (g::fire (g::requested-player-actions game-state)) t))
         (_ (g::player-fire* game-state))
         (projectiles-count (length (g::projectiles game-state))))
    (is (= projectiles-count 0))))
