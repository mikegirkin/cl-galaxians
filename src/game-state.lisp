(in-package :galaxians)

(defparameter +scale+ 6)
(defparameter +player-movement-speed+ 3)
(defparameter +player-width+ 16)
(defparameter +min-left-pos+ 8)
(defparameter +max-right-pos+ 200)
(defparameter +player-projectile-speed+ 10)
(defparameter +player-reload-time-seconds+ 0.25d0)
(defparameter +game-screen-width+ 320)
(defparameter +game-screen-height+ 200)

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
  16)

(defmethod get-center ((player-state player-state))
  (make-instance 'point2d
                 :x (+ (x player-state) (/ (get-width player-state) 2.0))
                 :y (+ (y player-state) (/ (get-width player-state) 2.0))))

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

(defclass requested-player-actions ()
  ((move-up :initform nil :accessor move-up)
   (move-down :initform nil :accessor move-down)
   (move-left :initform nil :accessor move-left)
   (move-right :initform nil :accessor move-right)
   (fire :initform nil :accessor fire)))

(deftype enemy-type () '(member :drone :sentry :guardian))

(defclass enemy-ship-state ()
  ((ship-position :initarg :ship-position
                  :type rectangle
                  :accessor ship-position)
   (ship-type :initarg :ship-type
              :type enemy-type
              :accessor ship-type)))

(defmethod print-object ((obj enemy-ship-state) out)
  (with-slots (ship-position ship-type) obj
    (print-unreadable-object (obj out :type t)
      (format out "position:~A type:~A" ship-position ship-type))))

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
                 :is-player-owned t))

(defmethod print-object ((obj projectile-state) out)
  (with-slots (position-rect speed-vector is-player-owned) obj
    (print-unreadable-object (obj out :type t)
      (format out "position-rect:~A speed-vector:~A is-player-owned:~A" position-rect speed-vector is-player-owned))))

(defmethod move! ((projectile projectile-state)
                  (ms-since-last-update double-float))
  (let* ((movement-vector (mul-scalar (speed-vector projectile) ms-since-last-update)))
    (move-rect! (position-rect projectile) movement-vector)))

(defun new-player-projectile (player-state speed-vector)
  (let* ((player-center-point (get-center player-state))
         (player-center-x (point-x player-center-point))
         (player-center-y (y player-state))
         (projectile-position-rect (make-rectangle-by-size player-center-x player-center-y 1 3)))
    (mk-projectile-state projectile-position-rect speed-vector t)))

(defclass game-state ()
  ((player-state :initform (make-player-state 150 20)
                 :type player-state
                 :accessor player-state)
   (enemies :initform (mk-initial-enemy-state)
            :type (vector enemy-ship-state)
            :accessor enemies)
   (projectiles :initform (make-array 5 :fill-pointer 0 :adjustable t)
                :type (vector projectile-state)
                :accessor projectiles)
   (reload-time-left :initform 0
                     :type integer
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
   (last-update-seconds :initform 0d0
                        :type double-float
                        :accessor last-update-seconds)
   (quit :initform nil
         :type boolean
         :accessor game-state-quit)))

(defmethod print-object ((obj game-state) out)
  (with-slots (player-state enemies) obj
    (print-unreadable-object (obj out :type t)
      (format out "player-state:~A enemies:~A" player-state enemies))))

(defun make-initial-game-state ()
  (make-instance 'game-state))

(defun limit-by (min-value max-value value)
  (min max-value
       (max min-value value)))

(defun mk-initial-ship-state (row col ship-type)
  (make-instance 'enemy-ship-state
                 :ship-position (make-rectangle-by-size
                                 (* (+ col 1) 16)
                                 (+ (* row 16) 132)
                                 15
                                 15)
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

(defmethod move-projectiles! ((game-state game-state)
                              (seconds-since-last-update double-float))
  (let* ((new-projectiles-vector (make-array (length (projectiles game-state))
                                             :fill-pointer 0
                                             :adjustable t))
         (screen-rect (screen-rect game-state)))
    (loop :for projectile :across (projectiles game-state)
          :do (move! projectile seconds-since-last-update)
          :when (has-common-area? screen-rect
                                  (position-rect projectile))
            :do (vector-push-extend projectile new-projectiles-vector))
    (setf (projectiles game-state) new-projectiles-vector)))

(defmethod move-player! ((game-state game-state))
  (let* ((left (if (move-left (requested-player-actions game-state)) +player-movement-speed+ 0))
         (right (if (move-right (requested-player-actions game-state)) +player-movement-speed+ 0))
         (dx (- right left))
         (new-x (limit-by +min-left-pos+ +max-right-pos+
                          (+ (x (player-state game-state)) dx))))
    (setf (x (player-state game-state)) new-x)))

(defmethod player-fire! ((game-state game-state)
                         (ms-since-last-update double-float))
  (if (and (fire (requested-player-actions game-state))
           (= (reload-time-left game-state) 0))
      (let* ((projectile-vector (make-vector2d 0 +player-projectile-speed+))
             (new-projectile (new-player-projectile (player-state game-state) projectile-vector)))
        (vector-push-extend new-projectile (projectiles game-state))
        (setf (reload-time-left game-state) +player-reload-time-seconds+))))

(defmethod update! ((game-state game-state)
                    (seconds-now double-float))
  (let* ((ms-since-last-update (- seconds-now (last-update-seconds game-state))))
    (move-projectiles! game-state ms-since-last-update)
    (player-fire! game-state ms-since-last-update)
    (move-player! game-state)
    (setf (last-update-seconds game-state) seconds-now)))
