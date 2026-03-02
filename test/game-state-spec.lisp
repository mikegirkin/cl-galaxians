(in-package :galaxians-spec)

(def-suite game-state-spec)
(in-suite game-state-spec)

(defparameter test-game-config
  (g::make-game-config :player-speed 3
                       :player-projectile-speed 10
                       :initial-wait-between-attacks 30))

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
                       (g::make-rectangle-by-size 142 16 1 3)))))

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
    (is (g::rectangle= (g::make-rectangle-by-size 142 196 1 3)
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
    (is (= 43 (length (-> game-state g::enemies g::enemy-ship-states)) 43))))


(def-suite enemy-state-spec)
(in-suite enemy-state-spec)
(setf g::+log-level+ :info)

(test eligible-attacker-indices-returns-all-non-moving-enemies
  (let+ ((game-config (g::make-game-config))
         (enemies-state (g::make-initial-enemies-state game-config))
         (eligible (g::eligible-attacker-indices enemies-state game-config)))
    ;; At attacks-completed-count=0 only drones are eligible (30 of them)
    (is (= 30 (length eligible)))
    ;; All returned indices should be drones with no movement-descriptor
    (is (every (lambda (idx)
                 (let ((enemy (elt (g::enemy-ship-states enemies-state) idx)))
                   (and (eq (g::ship-type enemy) :drone)
                        (null (g::movement-descriptor enemy)))))
               eligible))))

(test should-start-enemy-movement-is-correct
  (let+ ((game-state (g::make-initial-game-state test-game-config)))
    (is-false (g::should-start-enemy-movement? (g::enemies game-state) 29f0))
    (is-true (g::should-start-enemy-movement? (g::enemies game-state) 33f0))))

(test move-enemies-stops-enemy-movement
  (let+ ((game-state (g::make-initial-game-state test-game-config))
         (enemies (g::enemies game-state))
         (enemy-ship-states (g::enemy-ship-states enemies))
         (first-enemy-state (elt enemy-ship-states 0))
         (movement-trajectory (g::attack-trajectory-for-enemy-index test-game-config enemies 0))
         (movement-descriptor (g::make-movement-descriptor movement-trajectory 0f0 0f0)))

    (setf (g::movement-descriptor first-enemy-state) movement-descriptor)
    (g::update! game-state 30f0)
    (is (null (g::movement-descriptor first-enemy-state)))))

(test move-enemies-sets-rotation-angle
  (let+ ((game-state (g::make-initial-game-state test-game-config))
         (enemies (g::enemies game-state))
         (enemy-ship-states (g::enemy-ship-states enemies))
         (first-enemy (elt enemy-ship-states 0))
         (movement-trajectory (g::attack-trajectory-for-enemy-index test-game-config enemies 0))
         (movement-descriptor (g::make-movement-descriptor movement-trajectory 0f0 0f0)))

    ;; Slot default is 0 before any movement
    (is (g::float-eql (g::rotation-angle first-enemy) 0f0))

    (setf (g::movement-descriptor first-enemy) movement-descriptor)

    ;; At t=2.0 the trajectory tangent is purely leftward (dx≈-8, dy=0) because
    ;; enemy 0 is on the left side of the field and attacks toward the left boundary.
    ;; Expected angle is atan(dx, dy) = atan(-8, 0) = -pi/2.
    ;; The 1/3 control-point offsets produce dx=-7.999992 rather than -8.0 exactly,
    ;; so a small epsilon is needed.
    (g::update! game-state 2f0)
    (is (g::float-eql (g::rotation-angle first-enemy)
                      (/ pi 2f0)
                      :epsilon 1f-4))))

(defun make-enemies-state-with-single-enemy-at (x y game-config)
  "Create an enemies-state with one enemy centered at (X, Y)."
  (let* ((enemy (g::make-enemy-ship-state
                 (make-rectangle-from-center-size (make-point2d x y)
                                                  g::+enemy-ship-size+
                                                  g::+enemy-ship-size+)
                 :drone))
         (enemies-vector (make-array 1 :initial-contents (list enemy))))
    (g::make-enemies-state game-config enemies-vector)))

(defun make-game-state-with-attacking-enemy (fire-at)
  "Create a game-state with the first enemy already on an attack trajectory.
FIRE-AT is the relative trajectory time at which the enemy should fire."
  (let* ((game-state (g::make-initial-game-state test-game-config))
         (enemies (g::enemies game-state))
         (first-enemy (elt (g::enemy-ship-states enemies) 0))
         (trajectory (g::attack-trajectory-for-enemy-index test-game-config enemies 0))
         (md (g::make-movement-descriptor trajectory 0f0 fire-at)))
    (setf (g::movement-descriptor first-enemy) md)
    game-state))

(test enemy-fires-during-attack
  ;; fire-at=0.1 means the enemy should fire once relative-time passes 0.1
  (let* ((game-state (make-game-state-with-attacking-enemy 0.1f0))
         (_ (g::update! game-state 1f0))
         (enemy-projectiles (remove-if #'g::is-player-owned
                                       (coerce (g::projectiles game-state) 'list))))
    (is (= 1 (length enemy-projectiles)))))

(test enemy-fires-only-once
  ;; Even after multiple updates, only one enemy projectile should exist
  (let* ((game-state (make-game-state-with-attacking-enemy 0.1f0))
         (_ (g::update! game-state 1f0))
         (_ (g::update! game-state 2f0))
         (enemy-projectiles (remove-if #'g::is-player-owned
                                       (coerce (g::projectiles game-state) 'list))))
    (is (= 1 (length enemy-projectiles)))))

(test enemy-projectile-kills-player
  ;; Place an enemy projectile directly on the player and verify game-over is set.
  ;; Use seconds-now=0 so move-projectiles! applies zero displacement before the hit check.
  (let* ((game-state (g::make-initial-game-state test-game-config))
         (player-center (g::get-center (g::player-state game-state)))
         (projectile (g::mk-projectile-state
                      (make-rectangle-from-center-size player-center 1 3)
                      (make-vector2d 0f0 -10f0)
                      nil))
         (_ (vector-push-extend projectile (g::projectiles game-state)))
         (_ (g::update! game-state 0f0)))
    (is-true (g::game-over game-state))))

(test no-error-when-last-enemy-is-already-moving
  ;; With only one enemy present and already attacking, should-start-enemy-movement?
  ;; returns T (wait=0) but there are no eligible targets. This must not error.
  (let* ((game-config (g::make-game-config :initial-wait-between-attacks 0))
         (game-state (g::make-initial-game-state game-config))
         ;; Replace all enemies with a single one
         (single-enemy (g::make-enemy-ship-state
                        (make-rectangle-from-center-size (make-point2d 50f0 150f0)
                                                         g::+enemy-ship-size+
                                                         g::+enemy-ship-size+)
                        :drone))
         (enemies-vector (make-array 1 :initial-contents (list single-enemy)))
         (enemies-state (g::make-enemies-state game-config enemies-vector))
         (trajectory (g::attack-trajectory-for-enemy-index game-config enemies-state 0))
         (md (g::make-movement-descriptor trajectory 0f0 0.5f0)))
    (setf (g::enemies game-state) enemies-state)
    (setf (g::movement-descriptor single-enemy) md)
    ;; should-start-enemy-movement? is true immediately (wait=0),
    ;; but the only enemy is already moving -- this must not signal an error
    (finishes (g::update! game-state 1f0))))

(test left-side-enemy-attacks-toward-left-boundary
  ;; Enemy at x=50, gamefield center x=100 => should dive toward left boundary (x=0)
  (let+ ((game-config (g::make-game-config))
         (field-left (left (g::gamefield-rect game-config)))
         (enemies-state (make-enemies-state-with-single-enemy-at 50f0 150f0 game-config))
         (trajectory (g::attack-trajectory-for-enemy-index game-config enemies-state 0))
         ;; At t=0.5 the enemy should be near the near boundary waypoint
         (pos-at-first-waypoint (point-x (position-at trajectory 0.5f0))))
    (is (< pos-at-first-waypoint (/ (g::rectangle-width (g::gamefield-rect game-config)) 2f0)))
    (is (g::float-eql pos-at-first-waypoint (+ field-left 8f0) :epsilon 5f0))))

(test right-side-enemy-attacks-toward-right-boundary
  ;; Enemy at x=150, gamefield center x=100 => should dive toward right boundary (x=200)
  (let+ ((game-config (g::make-game-config))
         (field-right (right (g::gamefield-rect game-config)))
         (enemies-state (make-enemies-state-with-single-enemy-at 150f0 150f0 game-config))
         (trajectory (g::attack-trajectory-for-enemy-index game-config enemies-state 0))
         ;; At t=0.5 the enemy should be near the near boundary waypoint
         (pos-at-first-waypoint (point-x (position-at trajectory 0.5f0))))
    (is (> pos-at-first-waypoint (/ (g::rectangle-width (g::gamefield-rect game-config)) 2f0)))
    (is (g::float-eql pos-at-first-waypoint (- field-right 8f0) :epsilon 5f0))))
