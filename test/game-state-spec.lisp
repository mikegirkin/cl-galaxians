(in-package :galaxians-spec)

(def-suite* game-state-spec)
(in-suite game-state-spec)

(defparameter test-game-config
  (g::make-game-config :player-speed 3
                       :player-projectile-speed 10))

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
    (is (g::vector2d= (g::speed-vector projectile) movement-vector))
    (is (g::rectangle= (g::position-rect projectile)
                       (g::make-rectangle-by-size 20 28 1 3)))))

(test player-fires-if-reload-time-passed
  (let* ((game-state (g::make-initial-game-state test-game-config))
         (_1 (setf (g::fire (g::requested-player-actions game-state)) t))
         (_1 (g::player-fire! game-state 10f0))
         (added-projectile (elt (g::projectiles game-state) 0)))
    (is (g::rectangle= (g::position-rect added-projectile)
                       (g::make-rectangle-by-size 150 8 1 3)))))

(test player-cant-fire-when-reloading
  (let* ((game-state (g::make-initial-game-state test-game-config))
         (_ (setf (g::last-time-fired (g::player-state game-state)) 9.8f0))
         (_ (setf (g::fire (g::requested-player-actions game-state)) t))
         (_ (g::update! game-state 10f0))
         (projectiles-count (length (g::projectiles game-state))))
    (is (= projectiles-count 0))))

(test projectiles-are-moved-on-update
  (let* ((game-state (g::make-initial-game-state test-game-config))
         (_ (setf (g::fire (g::requested-player-actions game-state)) t))
         (_ (g::update! game-state 0f0))
         (projectile (elt (g::projectiles game-state) 0))
         (old-x1 (-> projectile (g::position-rect) (g::x1)))
         (old-x2 (-> projectile (g::position-rect) (g::x2)))
         (old-y1 (-> projectile (g::position-rect) (g::y1)))
         (old-y2 (-> projectile (g::position-rect) (g::y2)))
         (_ (g::update! game-state 0.1f0))
         (new-projectile (elt (g::projectiles game-state) 0))
         (new-x1 (-> new-projectile (g::position-rect) (g::x1)))
         (new-x2 (-> new-projectile (g::position-rect) (g::x2)))
         (new-y1 (-> new-projectile (g::position-rect) (g::y1)))
         (new-y2 (-> new-projectile (g::position-rect) (g::y2))))
    (is (= old-x1 new-x1))
    (is (= old-x2 new-x2))
    (is (g::float-eql new-y1
                      (+ old-y1 1f0)
                      :epsilon 1f-6))
    (is (g::float-eql new-y2
                      (+ old-y2 1f0)
                      :epsilon 1f-6))))

(test projectiles-disappear-when-leaving-window
  (let* ((game-state (g::make-initial-game-state test-game-config))
         (_ (setf (g::fire (g::requested-player-actions game-state)) t))
         (_ (g::update! game-state 0f0))
         (_ (setf (g::fire (g::requested-player-actions game-state)) nil))
         (_ (g::update! game-state 18f0))
         (projectile (elt (g::projectiles game-state) 0))
         (on-boundary (g::copy (g::position-rect projectile)))
         (_ (g::update! game-state 20f0)))
    (is (g::rectangle= (g::make-rectangle-by-size 150 188 1 3)
                       on-boundary))
    (is (= (length (g::projectiles game-state)) 0))))

(test projectiles-kill-enemies-on-contact
  (let+ ((game-state (g::make-initial-game-state test-game-config))
         (projectile (g::new-player-projectile (g::player-state game-state)
                                               (g::make-vector2d 0 10)))
         (_ (setf (g::position-rect projectile)
                  (make-rectangle-by-size 20 130 1 3)))
         (_ (vector-push-extend projectile (g::projectiles game-state)))
         (_ (g::update! game-state 1f0)))
    (is (= 0 (length (g::projectiles game-state))))
    (is (= (length (g::enemies game-state)) 43))))
