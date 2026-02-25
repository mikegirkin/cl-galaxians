(in-package :galaxians)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +enemy-types+ '(:drone :sentry :guardian)))

(deftype enemy-type () `(member ,@+enemy-types+))

(defun enemy-type-members ()
  +enemy-types+)

(defclass enemy-ship-state ()
  ((position-rect :initarg :position-rect
                  :type rectangle
                  :accessor position-rect)
   (ship-type :initarg :ship-type
              :type enemy-type
              :accessor ship-type)
   (movement-descriptor :initarg :movement-descriptor
                        :initform nil
                        :type enemy-movement-state
                        :accessor movement-descriptor)))

(defun make-enemy-ship-state (position ship-type)
  (make-instance 'enemy-ship-state
                 :position-rect position
                 :ship-type ship-type
                 :movement-descriptor nil))

(defun make-enemy-ship-state-from-row-col (row col ship-type)
  (make-enemy-ship-state (make-rectangle-by-size
                          (* (+ col 1) +general-cell-size+)
                          (+ (* row +general-cell-size+) 116)
                          +enemy-ship-size+
                          +enemy-ship-size+)
                         ship-type))

(defmethod print-object ((obj enemy-ship-state) out)
  (with-slots (position-rect ship-type) obj
    (print-unreadable-object (obj out :type t)
      (format out "position:~A type:~A" position-rect ship-type))))

(defclass enemies-state ()
  ((enemy-ship-states :type (vector enemy-ship-state)
                      :initarg :enemies
                      :accessor enemy-ship-states)
   (attacks-completed-count :type integer
                            :initarg :attacks-completed-count
                            :accessor attacks-completed-count)
   (last-attack-started-at :type single-float
                           :initarg :last-attack-started-at
                           :accessor last-attack-started-at)
   (wait-between-attacks :type single-float
                         :initarg :wait-between-attacks
                         :accessor wait-between-attacks)))

(defun make-enemies-state (game-config enemy-ships-state)
  (make-instance 'enemies-state
                 :enemies enemy-ships-state
                 :attacks-completed-count 0
                 :last-attack-started-at 0f0
                 :wait-between-attacks (-> game-config initial-wait-between-attacks)))

(defun make-initial-enemies-state (game-config)
  (let+ ((enemy-ships
          (append
           (loop :for col :from 0 :to 9
                 :append (loop
                           :for row :from 0 :to 2
                           :collect (make-enemy-ship-state-from-row-col row col :drone)))
           (loop :for col :from 1 :to 8
                 :with row := 3
                 :collect (make-enemy-ship-state-from-row-col row col :sentry))
           (loop :for col :from 2 :to 7
                 :with row := 4
                 :collect (make-enemy-ship-state-from-row-col row col :guardian))))
         (enemy-ships-vector (coerce enemy-ships 'vector)))
    (make-enemies-state game-config enemy-ships-vector)))

(defmethod pick-next-attacker ((enemies-state enemies-state)
                               (game-config game-config)
                               random-fn)
  "Picks next enemy ship to execute attack on the player. Returns enemy index.
   random-fn - 1 arg function, takes max number to return"
  (let+ (((&accessors attacks-completed-count enemy-ship-states) enemies-state)
         (progression-index (or (position-if (lambda (x) (> x attacks-completed-count))
                                             (attack-progression game-config))
                                (- (length +enemy-types+) 1)))
         (allowed-types (subseq +enemy-types+ 0 (+ progression-index 1)))
         (allowed-indices (all-positions-if (lambda (enemy) (and (member (ship-type enemy)
                                                                         allowed-types)
                                                                 (null (movement-descriptor enemy))))
                                            enemy-ship-states)))
    (funcall random-fn (length allowed-indices))))

(defmethod attack-trajectory-for-enemy-index ((game-config game-config)
                                              (enemies-state enemies-state)
                                              enemy-index)
  (let+ (((&accessors enemy-ship-states) enemies-state)
         (enemy (elt enemy-ship-states enemy-index))
         ((&accessors position-rect) enemy)
         (gamefield-rect (gamefield-rect game-config))
         ((&accessors (field-left left)
                      (field-top top)
                      (field-right right)
                      (field-bottom bottom)
                      (field-width rectangle-width)
                      (field-height rectangle-height)) gamefield-rect)
         (initial-point (-> position-rect center)))
    (make-spline-trajectory (spline-vertex initial-point
                                           (make-vector2d 0 1)
                                           0f0)
                            (spline-vertex (make-point2d 8 (- field-top 8f0))
                                           (make-vector2d 0 -20)
                                           0.5f0)
                            (spline-vertex (make-point2d (/ field-width 2f0)
                                                         (/ field-height 2f0))
                                           (make-vector2d 8 0)
                                           2f0)
                            (spline-vertex (make-point2d (- field-right 8f0)
                                                         (- field-top 8f0))
                                           (make-vector2d 0 8)
                                           3.5f0)
                            (spline-vertex initial-point
                                           (make-vector2d 0 1)
                                           4f0))))

(defmethod should-start-enemy-movement? ((enemies-state enemies-state)
                                         (seconds-now single-float))
  (let+ (((&accessors last-attack-started-at wait-between-attacks) enemies-state)
         (time-since-last-movement (- seconds-now last-attack-started-at)))
    (>= time-since-last-movement wait-between-attacks)))

(defmethod start-enemy-movement! ((game-config game-config)
                                  (enemies-state enemies-state)
                                  (seconds-now single-float))
  (format t "Starting enemy movement")
  ;decide what enemy is going to move
  (let+ ((random-fn (lambda (x) (random x)))
         (enemy-index (pick-next-attacker enemies-state game-config random-fn))
         ((&accessors enemy-ship-states) enemies-state)
         (enemy (elt enemy-ship-states enemy-index))
         (trajectory (attack-trajectory-for-enemy-index game-config enemies-state enemy-index))
         (attack-movement-descriptor (make-movement-descriptor trajectory
                                                               seconds-now)))
    (setf (movement-descriptor enemy) attack-movement-descriptor)
    (setf (last-attack-started-at enemies-state) seconds-now)))
