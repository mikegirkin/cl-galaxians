(in-package :galaxians)

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
