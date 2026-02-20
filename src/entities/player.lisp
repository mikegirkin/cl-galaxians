(in-package :galaxians)

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

(defmethod new-player-projectile ((player-state player-state)
                                  (speed-vector vector2d))
  (let* ((player-center-point (get-center player-state))
         (projectile-x (point-x player-center-point))
         (projectile-y (top (position-rect player-state)))
         (projectile-position-rect (make-rectangle-by-size projectile-x projectile-y 1 3)))
    (mk-projectile-state projectile-position-rect speed-vector t)))
