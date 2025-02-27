(in-package :galaxians)

(defconstant +scale+ 4)
(defconstant +player-movement-speed+ 3)

(defmethod world-to-gfx ((single-coord integer))
  (* single-coord +scale+))

(defmethod world-to-gfx ((rect rectangle))
  (make-rectangle :x1 (world-to-gfx (rectangle-x1 rect))
                  :y1 (world-to-gfx (rectangle-y1 rect))
                  :x2 (world-to-gfx (rectangle-x2 rect))
                  :y2 (world-to-gfx (rectangle-y2 rect))))

(defstruct player-position
  (x 0 :type integer)
  (y 0 :type integer))

(defstruct rectangle x1 y1 x2 y2)

(defun player-to-rectangle (player-position)
  (world-to-gfx
   (make-rectangle :x1 (- (player-position-x player-position) 5)
                   :y1 (- (player-position-y player-position) 5)
                   :x2 (+ (player-position-x player-position) 5)
                   :y2 (+ (player-position-y player-position) 5))))

(defclass requested-player-actions ()
  ((move-up :initform nil :accessor move-up)
   (move-down :initform nil :accessor move-down)
   (move-left :initform nil :accessor move-left)
   (move-right :initform nil :accessor move-right)
   (fire :initform nil :accessor fire)))

(defclass game-state ()
  ((player-position :initform (make-player-position :x 160 :y 120)
                    :type player-position
                    :accessor player-position)
   (reload-time-left :initform 0
                     :type integer)
   (requested-player-actions :initform (make-instance 'requested-player-actions)
                             :type requested-player-actions
                             :accessor requested-player-actions)
   (quit :initform nil
         :type boolean
         :accessor game-state-quit)))

(defmethod move-player* ((game-state game-state))
  (let* ((up (if (move-up (requested-player-actions game-state)) +player-movement-speed+ 0))
         (down (if (move-down (requested-player-actions game-state)) +player-movement-speed+ 0))
         (left (if (move-left (requested-player-actions game-state)) +player-movement-speed+ 0))
         (right (if (move-right (requested-player-actions game-state)) +player-movement-speed+ 0))
         (dy (- down up))
         (dx (- right left)))
    (incf (player-position-x (player-position game-state)) dx)
    (incf (player-position-y (player-position game-state)) dy)))
