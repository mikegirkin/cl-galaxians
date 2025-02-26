(in-package #:galaxians)

(define-constant +window-width+ 800)
(define-constant +window-height+ 600)

(defstruct player-position
  (x 0 :type integer)
  (y 0 :type integer))

(defstruct game-state
  (player-position (make-player-position :x 50 :y 50) :type player-position))

(defstruct rectangle x1 y1 x2 y2)

(defun player-to-rectangle (player-position)
  (make-rectangle :x1 (- (player-position-x player-position) 10)
                  :y1 (- (player-position-y player-position) 10)
                  :x2 (+ (player-position-x player-position) 10)
                  :y2 (+ (player-position-y player-position) 10)))

(defun initialize ()
  (unless (al:init)
    (error "Initializing liballegro failed"))
  (unless (al:init-primitives-addon)
    (error "Initializing primitives addon failed"))
  ;; (unless (al:init-image-addon)
    ;; (error "Initializing image addon failed"))
  ;; (unless (al:init-font-addon)
  ;;   (error "Initializing liballegro font addon failed"))
  ;; (unless (al:init-ttf-addon)
  ;;   (error "Initializing liballegro TTF addon failed"))
  ;; (unless (al:install-audio)
  ;;   (error "Intializing audio addon failed"))
  ;; (unless (al:init-acodec-addon)
  ;;   (error "Initializing audio codec addon failed"))
  ;; (unless (al:restore-default-mixer)
  ;;   (error "Initializing default audio mixer failed"))
  )

(defun shutdown (display event-queue)
  (al:inhibit-screensaver nil)
  (al:destroy-display display)
  (al:destroy-event-queue event-queue)
  (al:stop-samples)
  (al:uninstall-system)
  (al:uninstall-audio)
  (al:shutdown-ttf-addon)
  (al:shutdown-font-addon)
  (al:shutdown-image-addon)
  (al:shutdown-primitives-addon))

(defun render (game-state)
  (al:clear-to-color (al:map-rgb 0 0 0))
  (let ((player-rect (player-to-rectangle (game-state-player-position game-state))))
    (al:draw-filled-rectangle (rectangle-x1 player-rect)
                              (rectangle-y1 player-rect)
                              (rectangle-x2 player-rect)
                              (rectangle-y2 player-rect)
                              (al:map-rgb 155 255 255)))
  (al:flip-display))

(defun process-events (keyboard-state game-state)
  (let ((quit nil))
    (when (al:key-down keyboard-state :escape)
      (setf quit t))
    (when (al:key-down keyboard-state :w)
      (format t "W pressed~%"))
    (when (al:key-down keyboard-state :s)
      (format t "S pressed~%"))
    (when (al:key-down keyboard-state :a)
      (format t "A pressed~%"))
    (when (al:key-down keyboard-state :d)
      (format t "D pressed~%"))
    (when (al:key-down keyboard-state :p)
      (format t "P: (~a, ~a)~%"
              (player-position-x (game-state-player-position game-state))
              (player-position-y (game-state-player-position game-state))))
    quit))

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                              (al:show-native-message-box
                               (cffi:null-pointer) "Hey guys"
                               "We got a big error here :("
                               (with-output-to-string (s)
                                 (uiop:print-condition-backtrace e :stream s))
                               (cffi:null-pointer) :error)))))
    (al:set-app-name "galaxians")
    ;;     (let ((config (al:load-config-file +config-path+)))
    ;;       (unless (cffi:null-pointer-p config)
    ;;         (al:merge-config-into (al:get-system-config) config)))
    ;; ;    (setf *fpsp* (al:get-config-value (al:get-system-config)
                                        ;                                      "game" "show-fps"))
    (initialize)
    (let* ((display (al:create-display +window-width+ +window-height+))
           (event-queue (al:create-event-queue))
           (game-state (make-game-state)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Galaxians")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      ;; (al:install-mouse)
      ;; (al:register-event-source event-queue
      ;;                           (al:get-mouse-event-source))
      (unwind-protect
           (progn
             (livesupport:setup-lisp-repl)
             (trivial-garbage:gc :full t)
             (al:with-current-keyboard-state state
               (loop :named event-loop
                     :with ticks :of-type double-float := (al:get-time)
                     :with last-repl-update :of-type double-float := ticks
                     :with dt :of-type double-float := 0d0
                     :with restart := nil
                     :with quit := nil
                     :while (not quit)
                     :do (al:get-keyboard-state state)
                         (setf quit
                               (process-events state game-state))
                         (render game-state)
                         (sleep 0.01))))
        (shutdown display event-queue))))
  0)

(defun main ()
  (float-features:with-float-traps-masked
      (:divide-by-zero :invalid :inexact :overflow :underflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %main))))
