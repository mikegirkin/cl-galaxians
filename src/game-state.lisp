(in-package :galaxians)

(defparameter +scale+ 6)
(defparameter +player-movement-speed+ 3)
(defparameter +min-left-pos+ 8)
(defparameter +max-right-pos+ 200)
(defparameter +player-projectile-speed+ 10)
(defparameter +player-reload-time-ms+ 250)

(defclass player-state ()
  ((x :initform 0f0
      :initarg :x
      :type float
      :accessor x)
   (y :initform 0f0
      :initarg :y
      :type float
      :accessor y)))

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

(defclass rectangle ()
  ((x1 :initform 0
       :initarg :x1
       :accessor x1)
   (y1 :initform 0
       :initarg :y1
       :accessor y1)
   (x2 :initform 0
       :initarg :x2
       :accessor x2)
   (y2 :initform 0
       :initarg :y2
       :accessor y2)))

(defun make-rectangle-by-coords (x1 y1 x2 y2)
  (make-instance 'rectangle
                 :x1 x1
                 :y1 y1
                 :x2 x2
                 :y2 y2))

(defun make-rectangle-by-size (x1 y1 width height)
  (make-rectangle-by-coords x1 y1 (+ x1 width) (+ y1 height)))

(defmethod rectangle= (r1 r2)
  (and (= (x1 r1) (x1 r2))
       (= (y1 r1) (y1 r2))
       (= (x2 r1) (x2 r2))
       (= (y2 r1) (y2 r2))))

(defmethod print-object ((obj rectangle) out)
  (with-slots (x1 y1 x2 y2) obj
    (print-unreadable-object (obj out :type t)
      (format out "x1: ~A y1: ~A x2: ~A y2: ~A" x1 y1 x2 y2))))

(defmethod rectangle-width ((rectangle rectangle))
  (abs (- (x1 rectangle)
          (x2 rectangle))))

(defmethod rectangle-height ((rectangle rectangle))
  (abs (- (y1 rectangle)
          (y2 rectangle))))

(defclass vector2d ()
  ((dx :initform 0f0
       :initarg :dx
       :type float
       :accessor dx)
   (dy :initform 0f0
       :initarg :dy
       :type float
       :accessor dy)))

(defun make-vector2d (dx dy)
  (make-instance 'vector2d :dx dx :dy dy))

(defmethod print-object ((obj vector2d) out)
  (with-slots (dx dy) obj
    (print-unreadable-object (obj out :type t)
      (format out "dx: ~A dy: ~A" dx dy))))

(defmethod vector2d= (v1 v2)
  (and (= (dx v1) (dx v2))
       (= (dy v1) (dy v2))))


(defclass point2d ()
  ((x :initform 0f0
      :initarg :x
      :type float
      :accessor point-x)
   (y :initform 0f0
      :initarg :y
      :type float
      :accessor point-y)))

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

(defclass projectile-state ()
  ((position-rect :initarg :rect
                  :type rectangle
                  :accessor position-rect)
   (movement-vector :initarg :movement-vector
                    :type vector2d
                    :accessor movement-vector)
   (is-player-owned :initarg :is-player-owned
                    :type boolean
                    :accessor is-player-owned)))

(defun new-player-projectile (player-state movement-vector)
  (let* ((player-center-point (get-center player-state))
         (player-center-x (point-x player-center-point))
         (player-center-y (y player-state)))
    (make-instance 'projectile-state
                   :rect (make-rectangle-by-size player-center-x player-center-y 1 3)
                   :movement-vector movement-vector
                   :is-player-owned t)))

(defclass game-state ()
  ((player-state :initform (make-instance 'player-state :x 100 :y 150)
                 :type player-state
                 :accessor player-state)
   (enemies :initform (vector)
            :type (vector enemy-ship-state)
            :accessor enemies)
   (projectiles :initform (make-array 0 :fill-pointer t :adjustable t)
                :type (vector projectile-state)
                :accessor projectiles)
   (reload-time-left :initform 0
                     :type integer
                     :accessor reload-time-left)
   (requested-player-actions :initform (make-instance 'requested-player-actions)
                             :type requested-player-actions
                             :accessor requested-player-actions)
   (sprites :initform (make-sprites)
            :type sprites
            :accessor sprites)
   (quit :initform nil
         :type boolean
         :accessor game-state-quit)))

(defun make-initial-game-state ()
  (make-instance 'game-state))

(defun limit-by (min-value max-value value)
  (min max-value
       (max min-value value)))

(defun mk-initial-ship-state (ship-type row col)
  (make-instance 'enemy-ship-state
                 :ship-position (make-rectangle-by-size
                                 (* (+ col 1) 16)
                                 (* row 16)
                                 15
                                 15)
                 :ship-type ship-type))

(defun mk-initial-enemy-state ()
  (append
   (loop :for col :from 0 :to 9
         :append (loop
                   :for row :from 4 :downto 2
                   :collect (mk-initial-ship-state row col :drone)))
   (loop :for col :from 1 :to 8
         :with row := 1
         :collect (mk-initial-ship-state row col :sentry))
   (loop :for col :from 2 :to 7
         :with row := 0
         :collect (mk-initial-ship-state row col :guardian))))

(defmethod initialize-enemies! ((game-state game-state))
  (let ((enemies (mk-initial-enemy-state)))
    (setf (enemies game-state) (coerce enemies 'vector))))

(defmethod move-player* ((game-state game-state))
  (let* ((left (if (move-left (requested-player-actions game-state)) +player-movement-speed+ 0))
         (right (if (move-right (requested-player-actions game-state)) +player-movement-speed+ 0))
         (dx (- right left))
         (new-x (limit-by +min-left-pos+ +max-right-pos+
                          (+ (x (player-state game-state)) dx))))
    (setf (x (player-state game-state)) new-x)))

(defmethod player-fire* ((game-state game-state))
  (if (and (fire (requested-player-actions game-state))
           (= (reload-time-left game-state) 0))
      (let* ((projectile-vector (make-vector2d 0 +player-projectile-speed+))
             (new-projectile (new-player-projectile (player-state game-state) projectile-vector)))
        (vector-push-extend new-projectile (projectiles game-state))
        (setf (reload-time-left game-state) +player-reload-time-ms+))))

(defmethod update* ((game-state game-state))
  (player-fire* game-state)
  (move-player* game-state))
