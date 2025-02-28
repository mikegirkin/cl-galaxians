(in-package :galaxians)

(defun asset (path)
  (namestring (asdf:system-relative-pathname "galaxians" (concatenate 'string "resources/" path))))

(defconstant +scale+ 6)
(defconstant +player-movement-speed+ 3)
(defconstant +min-left-pos+ 8)
(defconstant +max-right-pos+ 200)
(defconstant +player-width+ 16)

(defvar +main-ship-asset+ (asset "images/main_ship.png"))

(defstruct player-position
  (x 0 :type integer)
  (y 0 :type integer))

(defstruct rectangle x1 y1 x2 y2)

(defstruct sprites main-ship)

(defclass requested-player-actions ()
  ((move-up :initform nil :accessor move-up)
   (move-down :initform nil :accessor move-down)
   (move-left :initform nil :accessor move-left)
   (move-right :initform nil :accessor move-right)
   (fire :initform nil :accessor fire)))

(defun limit-by (min-value max-value value)
  (min max-value
       (max min-value value)))

(defclass game-state ()
  ((player-position :initform (make-player-position :x 100 :y 150)
                    :type player-position
                    :accessor player-position)
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

(defmethod move-player* ((game-state game-state))
  (let* (;;(up (if (move-up (requested-player-actions game-state)) +player-movement-speed+ 0))
         ;;(down (if (move-down (requested-player-actions game-state)) +player-movement-speed+ 0))
         (left (if (move-left (requested-player-actions game-state)) +player-movement-speed+ 0))
         (right (if (move-right (requested-player-actions game-state)) +player-movement-speed+ 0))
         ;;(dy (- down up))
         (dx (- right left))
         (new-x (limit-by +min-left-pos+ +max-right-pos+
                          (+ (player-position-x (player-position game-state)) dx))))
    (setf (player-position-x (player-position game-state)) new-x)))

(defmethod update* ((game-state game-state))
  (move-player* game-state))

(defun load-bitmap (path)
  (let ((bitmap (al:load-bitmap path)))
    (unless bitmap
      (error (format t "Error loading ~a" path)))
    bitmap))

(defmethod load-sprites! (game-state)
  (setf (sprites-main-ship (sprites game-state)) (load-bitmap +main-ship-asset+)))
