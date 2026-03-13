(in-package :galaxians)

(defclass explosion-state ()
  ((position-rect :initarg :position-rect
                  :type rectangle
                  :accessor position-rect)
   (started-at :initarg :started-at
               :type single-float
               :accessor started-at)))

(defun make-explosion-state (position-rect started-at)
  "Create an explosion at POSITION-RECT beginning at STARTED-AT (absolute seconds)."
  (make-instance 'explosion-state
                 :position-rect position-rect
                 :started-at started-at))

(defmethod print-object ((obj explosion-state) out)
  (with-slots (position-rect started-at) obj
    (print-unreadable-object (obj out :type t)
      (format out "position:~A started-at:~A" position-rect started-at))))

(defun explosion-current-frame (explosion seconds-now game-config)
  "Return the current frame index (0-based) for EXPLOSION at SECONDS-NOW."
  (let* ((elapsed (- seconds-now (started-at explosion)))
         (frame-duration (/ (explosion-duration game-config)
                            (explosion-frame-count game-config)))
         (frame (floor (/ elapsed frame-duration))))
    (min frame (- (explosion-frame-count game-config) 1))))

(defun explosion-finished? (explosion seconds-now game-config)
  "Return T if the explosion animation has completed."
  (>= (- seconds-now (started-at explosion))
      (explosion-duration game-config)))
