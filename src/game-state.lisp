(in-package :galaxians)

(defun float-eql (a b &key epsilon)
  (if epsilon
      (< (abs (- a b)) epsilon)
      (eql a b)))

(defstruct sprites
  main-ship
  drone-ship
  sentry-ship
  guardian-ship)

(defclass animation ()
  ((sprites :initform nil :accessor sprites)
   (delay :initform nil :accessor delay) ;Time delay between switching
   ))

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
                 :player-state (make-player-state 142 8)
                 :enemies (make-initial-enemies-state game-config)))

(defmethod initialize-enemies! ((game-state game-state))
  (let+ (((&accessors game-config) game-state)
         (enemies (make-initial-enemies-state game-config)))
    (setf (enemies game-state) enemies)))

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
         (min-left-pos (-> game-state game-config gamefield-rect left))
         (max-right-pos (-> game-state game-config gamefield-rect right))
         (new-x (limit-by min-left-pos
                          max-right-pos
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
