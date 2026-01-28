(in-package :geometry-spec)

(def-suite* trajectory-spec)
(in-suite trajectory-spec)

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

(test make-curve-with-points
  (let* ((curves (make-curves))
         (fragment-0 (make-trajectory-fragment (elt curves 0) 0f0 10f0))
         (fragment-1 (make-trajectory-fragment (elt curves 1) 10f0 15f0))
         (trajectory (make-trajectory fragment-0
                                      fragment-1)))
    (is (point2d= (position-at trajectory 0f0)
                  (make-point2d 0 0)))
    (is (point2d= (position-at trajectory 10f0)
                  (make-point2d 8 0)))))
