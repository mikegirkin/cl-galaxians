(in-package :galaxians)

(defparameter +scale+ 6)
(defparameter +player-movement-speed+ 3)
(defparameter +min-left-pos+ 8)
(defparameter +max-right-pos+ 200)
(defparameter +player-width+ 16)

(defstruct player-position
  (x 0 :type integer)
  (y 0 :type integer))

(defstruct rectangle
  (x1 0 :type integer)
  (y1 0 :type integer)
  (x2 0 :type integer)
  (y2 0 :type integer))

(defmethod rectangle-width ((rectangle rectangle))
  (abs (-
        (rectangle-x1 rectangle)
        (rectangle-x2 rectangle))))

(defmethod rectangle-height ((rectangle rectangle))
  (abs (-
        (rectangle-y1 rectangle)
        (rectangle-y2 rectangle))))

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

(defclass game-state ()
  ((player-position :initform (make-player-position :x 100 :y 180)
                    :type player-position
                    :accessor player-position)
   (enemies :initform (vector)
            :type (vector enemy-ship-state)
            :accessor enemies)
   (reload-time-left :initform 0
                     :type integer)
   (requested-player-actions :initform (make-instance 'requested-player-actions)
                             :type requested-player-actions
                             :accessor requested-player-actions)
   (sprites :initform (make-sprites)
            :type sprites
            :accessor sprites)
   (quit :initform nil
         :type boolean
         :accessor game-state-quit)))

(defun limit-by (min-value max-value value)
  (min max-value
       (max min-value value)))

(defun mk-initial-enemy-state ()
  (append
   (loop :for i :from 0 :to 9
         :append (loop
                   :for row :from 4 :downto 2
                   :collect (make-instance 'enemy-ship-state
                                           :ship-position (make-rectangle :x1 (* (+ i 1) 16)
                                                                          :y1 (* row 16)
                                                                          :x2 (+ (* (+ i 1) 16) 15)
                                                                          :y2 (+ (* row 16) 15))
                                           :ship-type :drone)))
   (loop :for i :from 1 :to 8
         :with row := 1
         :collect (make-instance 'enemy-ship-state
                                 :ship-position (make-rectangle :x1 (* (+ i 1) 16)
                                                                :y1 (* row 16)
                                                                :x2 (+ (* (+ i 1) 16) 15)
                                                                :y2 (+ (* row 16) 15))
                                 :ship-type :sentry))
   (loop :for i :from 2 :to 7
         :with row := 0
         :collect (make-instance 'enemy-ship-state
                                 :ship-position (make-rectangle :x1 (* (+ i 1) 16)
                                                                :y1 (* row 16)
                                                                :x2 (+ (* (+ i 1) 16) 13)
                                                                :y2 (+ (* row 16) 13))
                                 :ship-type :guardian))))

(defmethod initialize-enemies! ((game-state game-state))
  (let ((enemies (mk-initial-enemy-state)))
    (setf (enemies game-state) (coerce enemies 'vector))))

(defmethod move-player* ((game-state game-state))
  (let* ((left (if (move-left (requested-player-actions game-state)) +player-movement-speed+ 0))
         (right (if (move-right (requested-player-actions game-state)) +player-movement-speed+ 0))
         (dx (- right left))
         (new-x (limit-by +min-left-pos+ +max-right-pos+
                          (+ (player-position-x (player-position game-state)) dx))))
    (setf (player-position-x (player-position game-state)) new-x)))

(defmethod update* ((game-state game-state))
  (move-player* game-state))
