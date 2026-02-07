(in-package :geometry-spec)

(defun make-curves ()
  (vector (make-cubic-bezier-curve-by-points
           (make-point2d 0f0 0f0)
           (make-point2d 1f0 1f0)
           (make-point2d 5f0 1f0)
           (make-point2d 8f0 0f0))
          (make-cubic-bezier-curve-by-end-vectors
           (make-point2d 8f0 0f0)
           (make-vector2d 0f0 -3f0)
           (make-point2d 13f0 -5f0)
           (make-vector2d 5f0 0f0))
          (make-cubic-bezier-curve-by-end-vectors
           (make-point2d 13f0 -5f0)
           (make-vector2d 5f0 0f0)
           (make-point2d 18f0 0f0)
           (make-vector2d 0f0 3f0))))

(def-suite* trajectory-spec)
(in-suite trajectory-spec)

(test make-trajectory-rejects-non-zero-start
  (let* ((curve-1 (elt (make-curves) 1))
         (fragment-1 (make-trajectory-fragment curve-1 10f0 15f0)))
    (signals simple-error
      (make-trajectory fragment-1))))

(test make-trajectory-rejects-trajectories-with-time-interruptions
  (let* ((curves (make-curves))
         (fragment-0 (make-trajectory-fragment (elt curves 0) 0f0 10f0))
         (fragment-1 (make-trajectory-fragment (elt curves 1) 12f0 15f0)))
    (signals simple-error
      (make-trajectory fragment-0 fragment-1))))

(test make-trajectory-manually
  (let* ((curves (make-curves))
         (fragment-0 (make-trajectory-fragment (elt curves 0) 0f0 10f0))
         (fragment-1 (make-trajectory-fragment (elt curves 1) 10f0 15f0))
         (trajectory (make-trajectory fragment-0
                                      fragment-1)))
    (is (point2d= (position-at trajectory 0f0)
                  (make-point2d 0 0)))
    (is (point2d= (position-at trajectory 10f0)
                  (make-point2d 8 0)))))

(test make-spline-trajectory
  (let* ((trajectory (make-spline-trajectory
                      (spline-vertex :p #(0f0 0f0)
                                     :v #(0f0 1f1)
                                     :time 0f0)
                      (spline-vertex :p #(5f0 0f0)
                                     :v #(0f0 -1f0)
                                     :time 5f0))))
    (is (point2d= (position-at trajectory 2.5f0)
                  (make-point2d 2.5f0 1.375f0)))))
