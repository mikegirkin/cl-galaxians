(in-package :galaxians)

(defparameter +log-level+ :info)

;; Global game constants
(defparameter +scale+ 6)
(defparameter +player-reload-time-seconds+ 1f0)
(defparameter +game-screen-width+ 320)
(defparameter +game-screen-height+ 200)
(defparameter +player-width+ 16)
(defparameter +enemy-ship-size+ 12)
(defparameter +general-cell-size+ 16)

(defclass game-config ()
  ((player-speed :type integer
                 :initarg :player-speed
                 :reader player-speed)
   (gamefield-rect :type rectangle
                   :reader gamefield-rect
                   :initarg :gamefield-rect)
   (player-projectile-speed :type integer
                            :initarg :player-projectile-speed
                            :reader player-projectile-speed)
   (initial-wait-between-attacks :type single-float
                                 :initarg :initial-wait-between-attacks
                                 :reader initial-wait-between-attacks)
   (attack-progression :type list ;; List of integers, defines after which number of attacks the game will progress to the next enemy type
                       :initarg :attack-progression
                       :reader attack-progression)))

(defun make-game-config (&key (player-speed 3)
                           (gamefield-rect (make-rectangle-by-size 0 0 200 192))
                           (player-projectile-speed 50)
                           (initial-wait-between-attacks 3)
                           (attack-progression (list 2 4)))
  (make-instance 'game-config
                 :player-speed player-speed
                 :gamefield-rect gamefield-rect
                 :player-projectile-speed player-projectile-speed
                 :initial-wait-between-attacks initial-wait-between-attacks
                 :attack-progression attack-progression))

(defun log-debug (line)
  (if (eq +log-level+ :debug)
      (format t "DEBUG: ~a~%" line)))
