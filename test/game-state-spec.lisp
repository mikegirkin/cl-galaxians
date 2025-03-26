(in-package :galaxians-spec)

(def-suite* game-state-spec)
(in-suite game-state-spec)

(test make-rectangle-by-coords
  (let ((rect (g::make-rectangle-by-coords 10 20 30 40)))
    (is (g::x1 rect) 10)
    (is (g::y1 rect) 20)
    (is (g::x2 rect) 30)
    (is (g::y2 rect) 40)))

(test make-rectangle-by-size
  (let ((rect (g::make-rectangle-by-coords 10 20 20 30)))
    (is (g::x1 rect) 10)
    (is (g::y1 rect) 20)
    (is (g::x2 rect) 30)
    (is (g::y2 rect) 50)))

(test new-player-projectile
  (let* ((player-state (g::make-player-state 20 20))
         (movement-vector (g::make-vector2d 0 10))
         (projectile (g::new-player-projectile player-state movement-vector)))
    (is (g::is-player-owned projectile) t)
    (is (g::movement-vector projectile) movement-vector)
    (is (g::position-rect projectile) (make-rectangle-by-size 28 28 1 3))))
