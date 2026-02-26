(in-package :geometry)

(defclass point2d ()
  ((x :initform 0f0
      :initarg :x
      :type single-float
      :accessor point-x)
   (y :initform 0f0
      :initarg :y
      :type single-float
      :accessor point-y)))

(defmethod print-object ((obj point2d) out)
  (with-slots (x y) obj
    (print-unreadable-object (obj out :type t)
      (format out "x:~A y:~A" x y))))

(defun make-point2d (x y)
  (make-instance 'point2d :x x
                          :y y))

(defmethod point2d= ((p1 point2d)
                     (p2 point2d))
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(defmethod as-array ((p point2d))
  (vector (point-x p) (point-y p)))

(defclass vector2d ()
  ((dx :initform 0f0
       :initarg :dx
       :type single-float
       :accessor dx)
   (dy :initform 0f0
       :initarg :dy
       :type single-float
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

(defmethod neg ((v vector2d))
  (make-vector2d (- (dx v))
                 (- (dy v))))

(defmethod plus ((p point2d)
                 (v vector2d))
  (make-point2d (+ (point-x p) (dx v))
                (+ (point-y p) (dy v))))

(defmethod minus ((p point2d)
                  (v vector2d))
  (plus p (neg v)))

(defclass rectangle ()
  ((x1 :initform 0f0
       :initarg :x1
       :accessor x1)
   (y1 :initform 0f0
       :initarg :y1
       :accessor y1)
   (x2 :initform 0f0
       :initarg :x2
       :accessor x2)
   (y2 :initform 0f0
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

(defun make-rectangle-from-center-size (center-point width height)
  (let+ (((&accessors (xc point-x)
                      (yc point-y)) center-point))
    (make-rectangle-by-coords (- xc (/ width 2f0))
                              (- yc (/ height 2f0))
                              (+ xc (/ width 2f0))
                              (+ yc (/ height 2f0)))))

(defmethod rectangle= ((r1 rectangle)
                       (r2 rectangle))
  (and (= (left r1) (left r2))
       (= (right r1) (right r2))
       (= (top r1) (top r2))
       (= (bottom r1) (bottom r2))))

(defmethod copy ((rect rectangle))
  (make-rectangle-by-coords (x1 rect)
                            (y1 rect)
                            (x2 rect)
                            (y2 rect)))

(defmethod rectangle-width ((rectangle rectangle))
  (- (right rectangle)
     (left rectangle)))

(defmethod rectangle-height ((rectangle rectangle))
  (- (top rectangle)
     (bottom rectangle)))

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

(defmethod topright ((rectangle rectangle))
  (make-point2d (right rectangle)
                (top rectangle)))

(defmethod bottomright ((rectangle rectangle))
  (make-point2d (right rectangle)
                (bottom rectangle)))

(defmethod bottomleft ((rectangle rectangle))
  (make-point2d (left rectangle)
                (bottom rectangle)))

(defmethod center ((rectangle rectangle))
  (make-point2d (/ (+ (left rectangle) (right rectangle)) 2)
                (/ (+ (top rectangle) (bottom rectangle)) 2)))

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
                      (topleft rect-to-check))
   (within-rectangle? boundary-rect
                      (topright rect-to-check))
   (within-rectangle? boundary-rect
                      (bottomleft rect-to-check))
   (within-rectangle? boundary-rect
                      (bottomright rect-to-check))
   (within-rectangle? rect-to-check
                      (topleft boundary-rect))
   (within-rectangle? rect-to-check
                      (topright boundary-rect))
   (within-rectangle? rect-to-check
                      (bottomleft boundary-rect))
   (within-rectangle? rect-to-check
                      (bottomright boundary-rect))))

(defmethod within-rectangle? ((boundary-rect rectangle)
                              (point point2d))
  (let+ (((&accessors (x point-x)
                      (y point-y)) point)
         ((&accessors (rect-top top)
                      (rect-bottom bottom)
                      (rect-left left)
                      (rect-right right)) boundary-rect))
    (and
     (>= x rect-left)
     (<= x rect-right)
     (>= y rect-bottom)
     (<= y rect-top))))
