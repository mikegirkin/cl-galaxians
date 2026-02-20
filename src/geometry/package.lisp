(defpackage :geometry
  (:use :cl :binding-arrows :let-plus)
  (:local-nicknames (:a :alexandria))
  (:export #:point2d
           #:point2d=
           #:point-x #:point-y
           #:print-object
           #:as-array
           #:make-point2d
           #:vector2d
           #:dx #:dy
           #:make-vector2d
           #:vector2d=
           #:mul-scalar
           #:rectangle
           #:x1 #:y1 #:x2 #:y2
           #:left #:top #:right #:bottom
           #:make-rectangle-by-coords
           #:make-rectangle-by-size
           #:make-rectangle-from-center-size
           #:rectangle=
           #:copy
           #:rectangle-width
           #:rectangle-height
           #:topleft
           #:bottomright
           #:center
           #:move-rect!
           #:has-common-area?
           #:within-rectangle?

           #:abstract-curve
           #:cubic-bezier-curve
           #:cubic-bezier-p0
           #:cubic-bezier-p1
           #:cubic-bezier-p2
           #:cubic-bezier-p3
           #:make-cubic-bezier-curve-by-points
           #:make-cubic-bezier-curve-by-end-vectors
           #:get-position
           #:get-speed
           #:trajectory-fragment
           #:make-trajectory-fragment
           #:trajectory
           #:make-trajectory
           #:position-at
           #:velocity-at

           #:spline-vertex
           #:make-spline-trajectory
           #:time-end
           ))
