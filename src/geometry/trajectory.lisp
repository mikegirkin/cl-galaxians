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
  (let* ((p1 (plus p-start
                   (mul-scalar v-start (/ 1f0 3f0))))
         (p2 (minus p-end
                    (mul-scalar v-end (/ 1f0 3f0)))))
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
          :reader curve)
   (time-start :initarg :time-start
               :type single-float
               :reader time-start)
   (time-end :initarg :time-end
             :type single-float
             :reader time-end)))

(defmethod print-object ((fragment trajectory-fragment) stream)
  (print-unreadable-object (fragment stream :type t :identity t)
    (with-slots (curve time-start time-end) fragment
      (format stream "curve=~a start=~,3f end=~,3f" curve time-start time-end))))

(defun make-trajectory-fragment (curve time-start time-end)
  (when (< time-end time-start)
    (error 'simple-error :format-control "end (~a) must be >= start (~a)"
                         :format-arguments (list time-end time-start)))
  (make-instance 'trajectory-fragment :curve curve
                                      :time-start time-start
                                      :time-end time-end))

(defmethod position-at ((fragment trajectory-fragment)
                        (time single-float))
  (with-slots (time-start time-end) fragment
      (let* ((fragment-duration (- time-end time-start))
             (relative-time (/ (- time time-start) fragment-duration)))
        (position-at (curve fragment)
                     relative-time))))

(defclass trajectory-spline-vertex ()
  ((point :type point-2d
          :initarg :point)
   (direction :type vector-2d
              :initarg :direction)
   (time :type single-float
         :initarg :time)))

(defun make-trajectory-spline-vertex (point direction time)
  "Create a vertex coupling position, direction, and moment for spline construction."
  (make-instance 'trajectory-spline-vertex
                 :point point
                 :direction direction
                 :time time))

(defun spline-vertex (&key p v time)
  (let+ ((#(px py) p)
         (#(vx vy) v))
        (make-trajectory-spline-vertex (make-point2d px py)
                                       (make-vector2d vx vy)
                                       time)))

(defclass trajectory ()
  ((fragments :initarg :fragments
              :type array
              :reader fragments)))

(defmethod print-object ((traj trajectory) stream)
  (print-unreadable-object (traj stream :type t :identity t)
    (with-slots (fragments) traj
      (let ((descs (map 'list
                        (lambda (frag)
                          (with-slots (curve time-start time-end) frag
                            (format nil "(~a @ ~,3f ~,3f)" curve time-start time-end)))
                        (coerce fragments 'list))))
        (format stream "fragments=~a" descs)))))

(defun make-trajectory (first-fragment &rest other-fragments)
  (let* ((fragments-v (apply #'vector (list* first-fragment other-fragments)))
         (starts-at-zero (= 0f0 (time-start (elt fragments-v 0))))
         (breaks-after-index (loop :for i :from 1 :below (length fragments-v)
                                   :when (/= (time-end (elt fragments-v (- i 1)))
                                             (time-start (elt fragments-v i)))
                                     :return (- i 1)
                                   :finally (return nil))))
    (cond ((not starts-at-zero) (error 'simple-error
                                       :format-control "first fragment must start at time 0"
                                       :format-arguments nil))
          (breaks-after-index (error 'simple-error
                                     :format-control "fragments must be continuous; break between fragment ~a and ~a"
                                     :format-arguments (list breaks-after-index (+ breaks-after-index 1))))
          (t (make-instance 'trajectory :fragments fragments-v)))))

(defun make-spline-trajectory (first-vertex second-vertex &rest vertexes)
  "Creates a trajectory instance as a series of connected bezier curves defined by two or more (point, vector, time) vertexes"
  (let* ((all-vertexes (list* first-vertex second-vertex vertexes))
         (sorted-vertexes (sort all-vertexes #'< :key (lambda (v) (slot-value v 'time))))
         (fragments (loop :for left-v :in sorted-vertexes
                          :for right-v :in (rest sorted-vertexes)
                          :while right-v
                          :collect (make-trajectory-fragment
                                    (make-cubic-bezier-curve-by-end-vectors
                                     (slot-value left-v 'point)
                                     (slot-value left-v 'direction)
                                     (slot-value right-v 'point)
                                     (slot-value right-v 'direction))
                                    (slot-value left-v 'time)
                                    (slot-value right-v 'time)))))
    (apply #'make-trajectory (first fragments) (rest fragments))))

(defmethod position-at ((trajectory trajectory)
                        (time single-float))
  "Return a point2d for trajectory at time time."
  (loop :for fragment :across (fragments trajectory)
        :when (and (>= time (time-start fragment))
                   (<= time (time-end fragment)))
          :return (position-at fragment time)
        :finally (return nil)))

(defgeneric velocity-at (trajectory time)
  (:documentation "Return a vector2d for trajectory at time time."))



