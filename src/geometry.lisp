(defpackage :geometry
  (:use :cl :binding-arrows :let-plus)
  (:export #:point2d
           #:point-x #:point-y
           #:print-object
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
           #:rectangle=
           #:copy
           #:rectangle-width
           #:rectangle-height
           #:topleft
           #:bottomright
           #:move-rect!
           #:has-common-area?
           #:within-rectangle?))

(in-package :geometry)

(defclass point2d ()
  ((x :initform 0f0
      :initarg :x
      :type float
      :accessor point-x)
   (y :initform 0f0
      :initarg :y
      :type float
      :accessor point-y)))

(defmethod print-object ((obj point2d) out)
  (with-slots (x y) obj
    (print-unreadable-object (obj out :type t)
      (format out "x:~A y:~A" x y))))

(defun make-point2d (x y)
  (make-instance 'point2d :x x
                          :y y))

(defclass vector2d ()
  ((dx :initform 0f0
       :initarg :dx
       :type float
       :accessor dx)
   (dy :initform 0f0
       :initarg :dy
       :type float
       :accessor dy)))

(defmethod print-object ((obj vector2d) out)
  (with-slots (dx dy) obj
    (print-unreadable-object (obj out :type t)
      (format out "dx:~A dy:~A" dx dy))))

(defun make-vector2d (dx dy)
  (make-instance 'vector2d :dx dx :dy dy))

(defmethod vector2d= (v1 v2)
  (and (= (dx v1) (dx v2))
       (= (dy v1) (dy v2))))

(defmethod mul-scalar (vector scalar)
  (make-vector2d (* (dx vector) scalar)
                 (* (dy vector) scalar)))

(defclass rectangle ()
  ((x1 :initform 0
       :initarg :x1
       :accessor x1)
   (y1 :initform 0
       :initarg :y1
       :accessor y1)
   (x2 :initform 0
       :initarg :x2
       :accessor x2)
   (y2 :initform 0
       :initarg :y2
       :accessor y2)))

(defmethod print-object ((obj rectangle) out)
  (with-slots (x1 y1 x2 y2) obj
    (print-unreadable-object (obj out :type t)
      (format out "x1:~A y1:~A x2:~A y2:~A" x1 y1 x2 y2))))

(defun make-rectangle-by-coords (x1 y1 x2 y2)
  (make-instance 'rectangle
                 :x1 x1
                 :y1 y1
                 :x2 x2
                 :y2 y2))

(defun make-rectangle-by-size (x1 y1 width height)
  (make-rectangle-by-coords x1 y1 (+ x1 width) (+ y1 height)))

(defmethod rectangle= ((r1 rectangle)
                       (r2 rectangle))
  (and (= (x1 r1) (x1 r2))
       (= (y1 r1) (y1 r2))
       (= (x2 r1) (x2 r2))
       (= (y2 r1) (y2 r2))))

(defmethod copy ((r rectangle))
  (make-rectangle-by-coords (x1 r)
                            (y1 r)
                            (x2 r)
                            (y2 r)))

(defmethod rectangle-width ((rectangle rectangle))
  (abs (- (x1 rectangle)
          (x2 rectangle))))

(defmethod rectangle-height ((rectangle rectangle))
  (abs (- (y1 rectangle)
          (y2 rectangle))))

(defmethod top ((rectangle rectangle))
  (max (y1 rectangle) (y2 rectangle)))

(defmethod bottom ((rectangle rectangle))
  (min (y1 rectangle) (y2 rectangle)))

(defmethod left ((rectangle rectangle))
  (min (x1 rectangle) (x2 rectangle)))

(defmethod right ((rectangle rectangle))
  (max (x1 rectangle) (x2 rectangle)))

(defmethod topleft ((rectangle rectangle))
  (make-point2d (left rectangle)
                (top rectangle)))

(defmethod bottomright ((rectangle rectangle))
  (make-point2d (right rectangle)
                (bottom rectangle)))

(defmethod move-rect! ((rect rectangle)
                       (vector vector2d))
  (setf (x1 rect) (+ (x1 rect) (dx vector)))
  (setf (y1 rect) (+ (y1 rect) (dy vector)))
  (setf (x2 rect) (+ (x2 rect) (dx vector)))
  (setf (y2 rect) (+ (y2 rect) (dy vector))))

(defmethod has-common-area? ((boundary-rect rectangle)
                             (rect-to-check rectangle))
  (or
   (within-rectangle? boundary-rect
                      (make-point2d (x1 rect-to-check) (y1 rect-to-check)))
   (within-rectangle? boundary-rect
                      (make-point2d (x1 rect-to-check) (y2 rect-to-check)))
   (within-rectangle? boundary-rect
                      (make-point2d (x2 rect-to-check) (y1 rect-to-check)))
   (within-rectangle? boundary-rect
                      (make-point2d (x2 rect-to-check) (y2 rect-to-check)))
   (within-rectangle? rect-to-check
                      (make-point2d (x1 boundary-rect) (y1 boundary-rect)))
   (within-rectangle? rect-to-check
                      (make-point2d (x1 boundary-rect) (y2 boundary-rect)))
   (within-rectangle? rect-to-check
                      (make-point2d (x2 boundary-rect) (y1 boundary-rect)))
   (within-rectangle? rect-to-check
                      (make-point2d (x2 boundary-rect) (y2 boundary-rect)))))

(defmethod within-rectangle? ((boundary-rect rectangle)
                              (point point2d))
  (let+ (((&accessors (x point-x)
                      (y point-y)) point)
         ((&accessors (tl topleft)
                      (br bottomright)) boundary-rect))
    (and
     (>= x (point-x tl))
     (<= x (point-x br))
     (>= y (point-y br))
     (<= y (point-y tl)))))

