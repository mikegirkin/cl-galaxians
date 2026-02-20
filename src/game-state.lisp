(in-package :galaxians)

(defparameter +scale+ 6)
(defparameter +min-left-pos+ 8)
(defparameter +max-right-pos+ 200)
(defparameter +player-reload-time-seconds+ 1f0)
(defparameter +game-screen-width+ 320)
(defparameter +game-screen-height+ 200)
(defparameter +player-width+ 16)
(defparameter +enemy-ship-size+ 12)
(defparameter +general-cell-size+ 16)

(defun float-eql (a b &key epsilon)
  (if epsilon
      (< (abs (- a b)) epsilon)
      (eql a b)))

(defclass player-state ()
  ((position-rect :type rectangle
                  :initarg :position-rect
                  :accessor position-rect)
   (last-time-fired :type single-float
                    :initarg :last-time-fired
                    :accessor last-time-fired)))

(defmethod print-object ((obj player-state) out)
  (with-slots (position-rect last-time-fired) obj
    (print-unreadable-object (obj out :type t)
      (format out "position:~A last-fired:~A" position-rect last-time-fired))))

(defun make-player-state (x-center y-center)
  (let ((new-player-state (make-instance 'player-state
                                         :position-rect nil
                                         :last-time-fired -10f0)))
    (set-center new-player-state (make-point2d x-center y-center))
    new-player-state))

(defmethod get-width ((player-state player-state))
  +player-width+)

(defmethod get-center ((player-state player-state))
  (let+ (((&accessors position-rect) player-state))
    (make-point2d (/ (+ (left position-rect) (right position-rect)) 2f0)
                  (/ (+ (top position-rect) (bottom position-rect)) 2f0))))

(defmethod set-center ((state player-state)
                       (new-center point2d))
  (let+ (((&accessors (x-center point-x)
                      (y-center point-y)) new-center)
         (new-position-rect (make-rectangle-by-size (- x-center (/ +player-width+ 2f0))
                                                    (- y-center (/ +player-width+ 2f0))
                                                    +player-width+
                                                    +player-width+)))
    (setf (position-rect state) new-position-rect)
    nil))

(defstruct sprites
  main-ship
  drone-ship
  sentry-ship
  guardian-ship)

(defclass animation ()
  ((sprites :initform nil :accessor sprites)
   (delay :initform nil :accessor delay) ;Time delay between switching
   ))

(defclass requested-player-actions ()
  ((move-up :initform nil :accessor move-up)
   (move-down :initform nil :accessor move-down)
   (move-left :initform nil :accessor move-left)
   (move-right :initform nil :accessor move-right)
   (fire :initform nil :accessor fire)))

(defmethod print-object ((obj requested-player-actions) out)
  (with-slots (move-left move-right fire) obj
    (print-unreadable-object (obj out :type t)
      (format out "move-left:~A move-right:~A fire:~A" move-left move-right fire))))

(defclass movement-descriptor ()
  ((trajectory :type trajectory
               :initarg :trajectory
               :accessor trajectory)
   (started-at :type single-float
               :initarg :started-at
               :accessor started-at)))

(defun make-movement-descriptor (trajectory started-at)
  "Create a movement-descriptor instance holding TRAJECTORY and STARTED-AT.
STARTED-AT is expected to be a single-float timestamp (seconds)."
  (make-instance 'movement-descriptor
                 :trajectory trajectory
                 :started-at started-at))

(defmethod print-object ((obj movement-descriptor) out)
  (with-slots (trajectory started-at) obj
    (print-unreadable-object (obj out :type t)
      (format out "trajectory:~A started-at:~A" trajectory started-at))))

(defparameter +enemy-types+ '(:drone :sentry :guardian))
(deftype enemy-type () `(member ,@+enemy-types+))
(defun enemy-type-members ()
  +enemy-types+)

(defclass enemy-ship-state ()
  ((position-rect :initarg :position-rect
                  :type rectangle
                  :accessor position-rect)
   (ship-type :initarg :ship-type
              :type enemy-type
              :accessor ship-type)
   (movement-descriptor :initarg :movement-descriptor
                        :initform nil
                        :type enemy-movement-state
                        :accessor movement-descriptor)))

(defun make-enemy-ship-state (position ship-type)
  (make-instance 'enemy-ship-state
                 :position-rect position
                 :ship-type ship-type
                 :movement-descriptor nil))

(defun make-enemy-ship-state-from-row-col (row col ship-type)
  (make-enemy-ship-state (make-rectangle-by-size
                          (* (+ col 1) +general-cell-size+)
                          (+ (* row +general-cell-size+) 116)
                          +enemy-ship-size+
                          +enemy-ship-size+)
                         ship-type))

(defmethod print-object ((obj enemy-ship-state) out)
  (with-slots (position-rect ship-type) obj
    (print-unreadable-object (obj out :type t)
      (format out "position:~A type:~A" position-rect ship-type))))

(defclass enemies-state ()
  ((enemy-ship-states :type (vector enemy-ship-state)
                      :initarg :enemies
                      :accessor enemy-ship-states)
   (attacks-completed-count :type integer
                            :initarg :attacks-completed-count
                            :accessor attacks-completed-count)
   (last-attack-started-at :type single-float
                           :initarg :last-attack-started-at
                           :accessor last-attack-started-at)
   (wait-between-attacks :type single-float
                         :initarg :wait-between-attacks
                         :accessor wait-between-attacks)))

(defun make-enemies-state (game-config enemy-ships-state)
  (make-instance 'enemies-state
                 :enemies enemy-ships-state
                 :attacks-completed-count 0
                 :last-attack-started-at 0f0
                 :wait-between-attacks (-> game-config initial-wait-between-attacks)))

(defun make-initial-enemies-state (game-config)
  (let+ ((enemy-ships
          (append
           (loop :for col :from 0 :to 9
                 :append (loop
                           :for row :from 0 :to 2
                           :collect (make-enemy-ship-state-from-row-col row col :drone)))
           (loop :for col :from 1 :to 8
                 :with row := 3
                 :collect (make-enemy-ship-state-from-row-col row col :sentry))
           (loop :for col :from 2 :to 7
                 :with row := 4
                 :collect (make-enemy-ship-state-from-row-col row col :guardian))))
         (enemy-ships-vector (coerce enemy-ships 'vector)))
    (make-enemies-state game-config enemy-ships-vector)))

(defmethod pick-next-attacker ((enemies-state enemies-state)
                               (game-config game-config)
                               random-fn)
  "Picks next enemy ship to execute attack on the player. Returns enemy index.
   random-fn - 1 arg function, takes max number to return"
  (let+ (((&accessors attacks-completed-count enemy-ship-states) enemies-state)
         (progression-index (position-if (lambda (x) (>= x attacks-completed-count))
                                         (attack-progression game-config)))
         (allowed-types (subseq +enemy-types+ 0 (+ progression-index 1)))
         (allowed-indices (all-positions-if (lambda (enemy) (and (member (ship-type enemy)
                                                                         allowed-types)
                                                                 (null (movement-descriptor enemy))))
                                            enemy-ship-states)))
    (funcall random-fn (length allowed-indices))))

(defclass projectile-state ()
  ((position-rect :initarg :rect
                  :type rectangle
                  :accessor position-rect)
   (speed-vector :initarg :speed-vector
                 :type vector2d
                 :accessor speed-vector)
   (is-player-owned :initarg :is-player-owned
                    :type boolean
                    :accessor is-player-owned)))

(defun mk-projectile-state (position-rect speed-vector is-player-owned)
  (make-instance 'projectile-state
                 :rect position-rect
                 :speed-vector speed-vector
                 :is-player-owned is-player-owned))

(defmethod print-object ((obj projectile-state) out)
  (with-slots (position-rect speed-vector is-player-owned) obj
    (print-unreadable-object (obj out :type t)
      (format out "position-rect:~A speed-vector:~A is-player-owned:~A" position-rect speed-vector is-player-owned))))

(defmethod move! ((projectile projectile-state)
                  (seconds-since-last-update single-float))
  (let* ((movement-vector (mul-scalar (speed-vector projectile) seconds-since-last-update)))
    (move-rect! (position-rect projectile) movement-vector)))

(defmethod new-player-projectile ((player-state player-state)
                                  (speed-vector vector2d))
  (let* ((player-center-point (get-center player-state))
         (projectile-x (point-x player-center-point))
         (projectile-y (top (position-rect player-state)))
         (projectile-position-rect (make-rectangle-by-size projectile-x projectile-y 1 3)))
    (mk-projectile-state projectile-position-rect speed-vector t)))

(defclass game-state ()
  ((player-state :initarg :player-state
                 :type player-state
                 :accessor player-state)
   (enemies :type enemies-state
            :accessor enemies
            :initarg :enemies)
   (projectiles :initform (make-array 5 :fill-pointer 0 :adjustable t)
                :type (vector projectile-state)
                :accessor projectiles)
   (requested-player-actions :initform (make-instance 'requested-player-actions)
                             :type requested-player-actions
                             :accessor requested-player-actions)
   (screen-rect :initform (make-rectangle-by-size 0 0 +game-screen-width+ +game-screen-height+)
                :type rectangle
                :accessor screen-rect)
   (sprites :initform (make-sprites)
            :type sprites
            :accessor sprites)
   (last-update-seconds :initform 0f0
                        :type single-float
                        :accessor last-update-seconds)
   (game-config :type game-config
                :initarg :game-config
                :reader game-config)
   (paused :initform nil
           :accessor paused)
   (quit :initform nil
         :type boolean
         :accessor game-state-quit)))

(defmethod print-object ((obj game-state) out)
  (with-slots (player-state enemies projectiles) obj
    (print-unreadable-object (obj out :type t)
      (format out "player-state:~A~%  enemies:~A~%  projectiles:~A" player-state enemies projectiles))))

(defun make-initial-game-state (game-config)
  (make-instance 'game-state
                 :game-config game-config
                 :player-state (make-player-state 150 0)
                 :enemies (make-initial-enemies-state game-config)))

(defmethod initialize-enemies! ((game-state game-state))
  (let+ (((&accessors game-config) game-state)
         (enemies (make-initial-enemies-state game-config)))
    (setf (enemies game-state) (coerce enemies 'vector))))

(defun limit-by (min-value max-value value)
  (min max-value
       (max min-value value)))

(defmethod within-screen? ((game-state game-state)
                           item)
  (let* ((screen-rect (screen-rect game-state))
         (item-rect (position-rect item)))
    (has-common-area? screen-rect item-rect)))

(defmethod get-enemy-hit ((game-state game-state)
                          (projectile projectile-state))
  (find-if (lambda (enemy) (has-common-area? (position-rect projectile)
                                             (position-rect enemy)))
           (-> game-state enemies enemy-ship-states)))

(defmethod process-enemy-hits! ((game-state game-state))
  (log-debug "process-enemy-hits!~%")
  (let* ((new-projectiles-vector (make-array (length (projectiles game-state))
                                             :fill-pointer 0
                                             :adjustable t)))
    (loop :for projectile :across (projectiles game-state)
          :if (is-player-owned projectile)
            :do (a:if-let ((enemy (get-enemy-hit game-state projectile)))
                  (setf (enemy-ship-states (enemies game-state))
                        (remove enemy (enemy-ship-states (enemies game-state))))
                                        ;TODO: add enemy killed animation
                  (vector-push-extend projectile new-projectiles-vector))
          :else :do
            (vector-push-extend projectile new-projectiles-vector))
    (setf (projectiles game-state) new-projectiles-vector)))

(defmethod move-projectiles! ((game-state game-state)
                              (seconds-since-last-update single-float))
  (log-debug "move-projectiles!~%")
  (let* ((new-projectiles-vector (make-array (length (projectiles game-state))
                                             :fill-pointer 0
                                             :adjustable t)))
    (loop :for projectile :across (projectiles game-state)
          :do (move! projectile seconds-since-last-update))
    (process-enemy-hits! game-state)
    (loop :for projectile :across (projectiles game-state)
          :do (cond
                ((within-screen? game-state projectile)
                 (vector-push-extend projectile new-projectiles-vector))))
    (setf (projectiles game-state) new-projectiles-vector)))

(defmethod move-player! ((game-state game-state)
                         (seconds-since-last-update single-float))
  (log-debug "move-player!~%")
  (let* ((player-speed (-> game-state game-config player-speed))
         (left (if (move-left (requested-player-actions game-state)) player-speed 0))
         (right (if (move-right (requested-player-actions game-state)) player-speed 0))
         (total-speed (- right left))
         (dx (* total-speed seconds-since-last-update))
         (player-center (get-center (player-state game-state)))
         (new-x (limit-by +min-left-pos+ +max-right-pos+
                          (+ (point-x player-center) dx)))
         (new-center (make-point2d new-x
                                   (point-y player-center))))
    (set-center (player-state game-state) new-center)))

(defmethod player-fire! ((game-state game-state)
                         (seconds-now single-float))
  (log-debug "player-fire!~%")
  (let+ ((player-state (player-state game-state))
         (seconds-since-last-fired (- seconds-now
                                      (-> game-state player-state last-time-fired)))
         (reload-time-left (- +player-reload-time-seconds+
                              seconds-since-last-fired)))
    (if (and (fire (requested-player-actions game-state))
             (<= reload-time-left 0))
        (let* ((projectile-vector (make-vector2d 0 (-> game-state game-config player-projectile-speed)))
               (new-projectile (new-player-projectile (player-state game-state) projectile-vector)))
          (vector-push-extend new-projectile (projectiles game-state))
          (setf (last-time-fired player-state) seconds-now)))))

(defmethod attack-trajectory-for-enemy-index ((enemies-state enemies-state)
                                              enemy-index)
  (let+ (((&accessors enemy-ship-states) enemies-state)
         (enemy (elt enemy-ship-states enemy-index))
         ((&accessors position-rect) enemy)
         (initial-point (-> position-rect center)))
    (make-spline-trajectory (spline-vertex initial-point
                                           (make-vector2d 0 1)
                                           0f0)
                            (spline-vertex (make-point2d 20 (- +game-screen-height+ 20f0))
                                           (make-vector2d 0 -20)
                                           1f0)
                            (spline-vertex (make-point2d (/ +game-screen-width+ 2f0)
                                                         (/ +game-screen-height+ 2f0))
                                           (make-vector2d 20 0)
                                           4f0)
                            (spline-vertex (make-point2d (- +game-screen-width+ -20f0)
                                                         (- +game-screen-height+ 20f0))
                                           (make-vector2d 0 20)
                                           7f0)
                            (spline-vertex initial-point
                                           (make-vector2d 0 1)
                                           8f0))))

(defmethod should-start-enemy-movement? ((enemies-state enemies-state)
                                         (seconds-now single-float))
  (let+ (((&accessors last-attack-started-at wait-between-attacks) enemies-state)
         (time-since-last-movement (- seconds-now last-attack-started-at)))
    (>= time-since-last-movement wait-between-attacks)))

(defmethod start-enemy-movement! ((game-config game-config)
                                  (enemies-state enemies-state)
                                  (seconds-now single-float))
  ;decide what enemy is going to move
  (let+ ((random-fn (lambda (x) (random x)))
         (enemy-index (pick-next-attacker enemies-state game-config random-fn))
         ((&accessors enemy-ship-states) enemies-state)
         (enemy (elt enemy-ship-states enemy-index))
         (trajectory (attack-trajectory-for-enemy-index enemies-state enemy-index))
         (attack-movement-descriptor (make-movement-descriptor trajectory
                                                               seconds-now)))
    (setf (movement-descriptor enemy) attack-movement-descriptor)
    (setf (last-attack-started-at enemies-state) seconds-now)))

(defmethod move-enemies! ((game-state game-state)
                          (seconds-now single-float))
  ;; Set movement-descriptor to nil for enemies that finished their attack moves
  (loop :for enemy :across (-> game-state enemies enemy-ship-states)
        :as md := (movement-descriptor enemy)
        :when md
        :do
           (let+ (((&accessors trajectory started-at) md)
                  (trajectory-end-time (time-end trajectory)))
             (if (>= seconds-now
                     (+ started-at trajectory-end-time))
                 (setf (movement-descriptor enemy) nil)
                 (incf (attacks-completed-count (enemies game-state)) ))))
  ;; Move existing enemies
  (loop :for enemy :across (-> game-state enemies enemy-ship-states)
        :as md := (movement-descriptor enemy)
        :when md
        :do
           (let+ ((old-position (position-rect enemy))
                  ((&accessors trajectory started-at) md)
                  (new-center (position-at trajectory (- seconds-now started-at)))
                  (new-position (make-rectangle-from-center-size new-center
                                                                 (rectangle-width old-position)
                                                                 (rectangle-height old-position))))
             (setf (position-rect enemy) new-position)))
  ;; If time for next attack came - start it
  (if (should-start-enemy-movement? (enemies game-state) seconds-now)
      (start-enemy-movement! (game-config game-state)
                             (enemies game-state)
                             seconds-now)))

(defmethod update! ((game-state game-state)
                    (seconds-now single-float))
  (let* ((seconds-since-last-update (- seconds-now (last-update-seconds game-state))))
    (move-projectiles! game-state seconds-since-last-update)
    (player-fire! game-state seconds-now)
    (move-player! game-state seconds-since-last-update)
    (move-enemies! game-state seconds-now)
    (setf (last-update-seconds game-state) seconds-now)))
