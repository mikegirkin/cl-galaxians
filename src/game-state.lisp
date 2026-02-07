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
  ((x :initform 0f0
      :initarg :x
      :type float
      :accessor x)
   (y :initform 0f0
      :initarg :y
      :type float
      :accessor y)))

(defmethod print-object ((obj player-state) out)
  (with-slots (x y) obj
    (print-unreadable-object (obj out :type t)
      (format out "x:~A y:~A" x y))))

(defun make-player-state (x y)
  (make-instance 'player-state
                 :x x
                 :y y))

(defmethod get-width ((player-state player-state))
  +player-width+)

(defmethod get-center ((player-state player-state))
  (make-point2d (+ (x player-state) (/ (get-width player-state) 2f0))
                (+ (y player-state) (/ (get-width player-state) 2f0))))

(defmethod as-rectangle ((player-state player-state))
  (make-rectangle-by-size (x player-state)
                          (y player-state)
                          (get-width player-state)
                          (get-width player-state)))

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
  ((trajectory)
   (time-elapsed)))

(deftype enemy-type () '(member :drone :sentry :guardian))

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
                        :accessor :movement-descriptor)))

(defmethod print-object ((obj enemy-ship-state) out)
  (with-slots (position-rect ship-type) obj
    (print-unreadable-object (obj out :type t)
      (format out "position:~A type:~A" position-rect ship-type))))

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

(defun new-player-projectile (player-state speed-vector)
  (let* ((player-center-point (get-center player-state))
         (projectile-x (point-x player-center-point))
         (projectile-y (top (as-rectangle player-state)))
         (projectile-position-rect (make-rectangle-by-size projectile-x projectile-y 1 3)))
    (mk-projectile-state projectile-position-rect speed-vector t)))

(defclass game-config ()
  ((player-speed :type integer
                 :initarg :player-speed
                 :reader player-speed)
   (player-projectile-speed :type integer
                            :initarg :player-projectile-speed
                            :reader player-projectile-speed)))

(defun make-game-config (&key (player-speed 3)
                              (player-projectile-speed 50))
  (make-instance 'game-config
                 :player-speed player-speed
                 :player-projectile-speed player-projectile-speed))

(defclass game-state ()
  ((player-state :initform (make-player-state 150 0)
                 :type player-state
                 :accessor player-state)
   (enemies :initform (mk-initial-enemy-state)
            :type (vector enemy-ship-state)
            :accessor enemies)
   (projectiles :initform (make-array 5 :fill-pointer 0 :adjustable t)
                :type (vector projectile-state)
                :accessor projectiles)
   (reload-time-left :initform 0f0
                     :type single-float
                     :accessor reload-time-left)
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
  (make-instance 'game-state :game-config game-config))

(defun limit-by (min-value max-value value)
  (min max-value
       (max min-value value)))

(defun mk-initial-ship-state (row col ship-type)
  (make-instance 'enemy-ship-state
                 :position-rect (make-rectangle-by-size
                                 (* (+ col 1) +general-cell-size+)
                                 (+ (* row +general-cell-size+) 116)
                                 +enemy-ship-size+
                                 +enemy-ship-size+)
                 :ship-type ship-type))

(defun mk-initial-enemy-state ()
  (append
   (loop :for col :from 0 :to 9
         :append (loop
                   :for row :from 0 :to 2
                   :collect (mk-initial-ship-state row col :drone)))
   (loop :for col :from 1 :to 8
         :with row := 3
         :collect (mk-initial-ship-state row col :sentry))
   (loop :for col :from 2 :to 7
         :with row := 4
         :collect (mk-initial-ship-state row col :guardian))))

(defmethod initialize-enemies! ((game-state game-state))
  (let ((enemies (mk-initial-enemy-state)))
    (setf (enemies game-state) (coerce enemies 'vector))))

(defmethod within-screen? ((game-state game-state)
                            item)
  (let* ((screen-rect (screen-rect game-state))
         (item-rect (position-rect item)))
    (has-common-area? screen-rect item-rect)))

(defmethod get-enemy-hit ((game-state game-state)
                          (projectile projectile-state))
  (find-if (lambda (enemy) (has-common-area? (position-rect projectile)
                                             (position-rect enemy)))
           (enemies game-state)))

(defmethod process-enemy-hits! ((game-state game-state))
  (let* ((new-projectiles-vector (make-array (length (projectiles game-state))
                                             :fill-pointer 0
                                             :adjustable t)))
    (loop :for projectile :across (projectiles game-state)
          :if (is-player-owned projectile)
            :do (a:if-let ((enemy (get-enemy-hit game-state projectile)))
                  (progn
                    (force-output)
                    (setf (enemies game-state)
                          (remove enemy (enemies game-state)))
                                        ;TODO: add enemy killed animation
                    )
                  (vector-push-extend projectile new-projectiles-vector))
          :else :do
            (vector-push-extend projectile new-projectiles-vector))
    (setf (projectiles game-state) new-projectiles-vector)))

(defmethod move-projectiles! ((game-state game-state)
                              (seconds-since-last-update single-float))
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
  (let* ((player-speed (-> game-state game-config player-speed))
         (left (if (move-left (requested-player-actions game-state)) player-speed 0))
         (right (if (move-right (requested-player-actions game-state)) player-speed 0))
         (total-speed (- right left))
         (dx (* total-speed seconds-since-last-update))
         (new-x (limit-by +min-left-pos+ +max-right-pos+
                          (+ (x (player-state game-state)) dx))))
    (setf (x (player-state game-state)) new-x)))

(defmethod player-fire! ((game-state game-state)
                         (seconds-since-last-update single-float))
  (if (> (reload-time-left game-state) 0)
      (setf (reload-time-left game-state)
            (max (- (reload-time-left game-state) seconds-since-last-update)
                 0)))
  (if (and (fire (requested-player-actions game-state))
           (= (reload-time-left game-state) 0))
      (let* ((projectile-vector (make-vector2d 0 (-> game-state game-config player-projectile-speed)))
             (new-projectile (new-player-projectile (player-state game-state) projectile-vector)))
        (vector-push-extend new-projectile (projectiles game-state))
        (setf (reload-time-left game-state) +player-reload-time-seconds+))))

(defmethod update! ((game-state game-state)
                    (seconds-now single-float))
  (let* ((seconds-since-last-update (- seconds-now (last-update-seconds game-state))))
    (move-projectiles! game-state seconds-since-last-update)
    (player-fire! game-state seconds-since-last-update)
    (move-player! game-state seconds-since-last-update)
    (setf (last-update-seconds game-state) seconds-now)))
