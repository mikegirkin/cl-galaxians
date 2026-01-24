(in-package :geometry-spec)

(def-suite* trajectory-spec)
(in-suite trajectory-spec)

(test make-curve-with-points
  (let* ((curve-0 (make-cubic-bezier-curve-by-points
                   (make-point2d 0f0 0f0)
                   (make-point2d 1f0 1f0)
                   (make-point2d 5f0 1f0)
                   (make-point2d 8f0 0f0)))
         (curve-1 (make-cubic-bezier-curve-by-end-vectors
                   (make-point2d 8f0 0f0)
                   (make-vector2d 11f0 -3f0)
                   (make-point2d 13f0 -5f0)
                   (make-vector2d 16f0 0f0)))
         (fragment-0 (make-trajectory-fragment curve-0 10))
         (fragment-1 (make-trajectory-fragment curve-1 5))
         (trajectory (make-trajectory fragment-0
                                      fragment-1)))
    (is (point2d= (position-at trajectory 0f0)
                  (make-point2d 0 0)))
    (is (point2d= (position-at trajectory 10f0)
                  (make-point2d 8 0)))))
