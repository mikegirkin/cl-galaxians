(in-package :geometry)

(defclass abstract-curve () ())

(defclass cubic-bezier-curve (abstract-curve)
  ((p0 :initarg :p0
       :type point2d
       :accessor cubic-bezier-p0)
   (p1 :initarg :p1
       :type point2d
       :accessor cubic-bezier-p1)
   (p2 :initarg :p2
       :type point2d
       :accessor cubic-bezier-p2)
   (p3 :initarg :p3
       :type point2d
       :accessor cubic-bezier-p3)))

(defmethod print-object ((curve cubic-bezier-curve) stream)
  (print-unreadable-object (curve stream :type t :identity t)
    (with-slots (p0 p1 p2 p3) curve
      (format stream "p0=~a p1=~a p2=~a p3=~a" p0 p1 p2 p3))))

(defun make-cubic-bezier-curve-by-points (p0 p1 p2 p3)
  (make-instance 'cubic-bezier-curve :p0 p0
                                     :p1 p1
                                     :p2 p2
                                     :p3 p3))

(defun make-cubic-bezier-curve-by-end-vectors (p-start v-start p-end v-end)
  (let* ((p1 (make-point2d (/ (+ (point-x p-start) (dx v-start)) 3f0)
                           (/ (+ (point-y p-start) (dy v-start)) 3f0)))
         (p2 (make-point2d (/ (- (point-x p-end) (dx v-end)))
                           (/ (- (point-y p-end) (dy v-end))))))
    (make-cubic-bezier-curve-by-points p-start p1 p2 p-end)))

(defmethod position-at ((curve cubic-bezier-curve)
                         (time single-float))
  "Returns position at the bezier curve for time in [0, 1]"
  (with-slots (p0 p1 p2 p3) curve
    (let* ((u (- 1f0 time))
           (uu (* u u))
           (uuu (* uu u))
           (tt (* time time))
           (ttt (* tt time))
           (x (+ (* uuu (point-x p0))
                 (* 3f0 uu time (point-x p1))
                 (* 3f0 u tt (point-x p2))
                 (* ttt (point-x p3))))
           (y (+ (* uuu (point-y p0))
                 (* 3f0 uu time (point-y p1))
                 (* 3f0 u tt (point-y p2))
                 (* ttt (point-y p3)))))
      (make-point2d x y))))

(defmethod speed_at ((curve cubic-bezier-curve)
                      (time single-float))
  "Returns speed vector (derivative) at the bezier curve for time in [0, 1]. Formula: B'(time) = 3(1-t)²(P₁-P₀) + 6(1-t)time(P₂-P₁) + 3t²(P₃-P₂)"
  (with-slots (p0 p1 p2 p3) curve
    (let* ((u (- 1f0 time))
           (uu (* u u))
           (tt (* time time))
           (dx (+ (* 3f0 uu (- (point-x p1) (point-x p0)))
                  (* 6f0 u time (- (point-x p2) (point-x p1)))
                  (* 3f0 tt (- (point-x p3) (point-x p2)))))
           (dy (+ (* 3f0 uu (- (point-y p1) (point-y p0)))
                  (* 6f0 u time (- (point-y p2) (point-y p1)))
                  (* 3f0 tt (- (point-y p3) (point-y p2))))))
      (make-vector2d dx dy))))

(defclass trajectory-fragment ()
  ((curve :initarg :curve
          :type abstract-curve
          :accessor curve)
   (period :initarg :period
           :type single-float
           :accessor period)))

(defun make-trajectory-fragment (curve period)
  (make-instance 'trajectory-fragment :curve curve
                                      :period period))

(defmethod position-at ((fragment trajectory-fragment)
                        (time single-float))
  (let* ((relative-time (/ time (period fragment))))
    (position-at (curve fragment)
                 relative-time)))

(defmethod print-object ((fragment trajectory-fragment) stream)
  (print-unreadable-object (fragment stream :type t :identity t)
    (with-slots (curve period) fragment
      (format stream "curve=~a time=~,4f" curve period))))

(defclass trajectory ()
  ((fragments :initarg :fragments
              :type array
              :accessor fragments)))

(defun make-trajectory (&rest fragments)
  (make-instance 'trajectory :fragments (apply #'vector fragments)))

(defmethod print-object ((traj trajectory) stream)
  (print-unreadable-object (traj stream :type t :identity t)
    (with-slots (fragments) traj
      (let ((descs (map 'list
                        (lambda (frag)
                          (with-slots (curve period) frag
                            (format nil "(~a @ ~,4f)" (class-of curve) period)))
                        (coerce fragments 'list))))
        (format stream "fragments=~a" descs)))))

(defmethod position-at ((trajectory trajectory)
                        (time single-float))
  "Return a point2d for trajectory at time time."
  (loop :for fragment :across (fragments trajectory)
        :for time-from-beginning = 0 :then (+ time-from-beginning (period fragment))
        :for fragment-end-time = (+ time-from-beginning (period fragment))
        :when (and (>= time time-from-beginning)
                   (<= time fragment-end-time))
          :return (position-at fragment
                               (- time time-from-beginning))
        :finally (return nil)))

(defgeneric velocity-at (trajectory time)
  (:documentation "Return a vector2d for trajectory at time time."))



