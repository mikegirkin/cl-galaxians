(in-package #:galaxians)

(define-constant +window-width+ 800)
(define-constant +window-height+ 600)

(defun initialize ()
  (unless (al:init)
    (error "Initializing liballegro failed"))
  (unless (al:init-primitives-addon)
    (error "Initializing primitives addon failed"))
  (unless (al:init-image-addon)
    (error "Initializing image addon failed"))
  (unless (al:init-font-addon)
    (error "Initializing liballegro font addon failed"))
  (unless (al:init-ttf-addon)
    (error "Initializing liballegro TTF addon failed"))
  (unless (al:install-audio)
    (error "Intializing audio addon failed"))
  (unless (al:init-acodec-addon)
    (error "Initializing audio codec addon failed"))
  (unless (al:restore-default-mixer)
    (error "Initializing default audio mixer failed")))

(defun shutdown (display event-queue)
  (al:inhibit-screensaver nil)
  (al:destroy-display display)
  (al:destroy-event-queue event-queue)
  (al:stop-samples)
  (al:uninstall-system)
  (al:uninstall-audio)
  (al:shutdown-ttf-addon)
  (al:shutdown-font-addon)
  (al:shutdown-image-addon))

(defun render ()
  (al:clear-to-color (al:map-rgb 0 0 0))
  (al:draw-filled-rectangle
   100 110 400 450
   (al:map-rgb 255 255 255))
  (al:flip-display))

(defun process-events (keyboard-state)
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
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Galaxians")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
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
                               (process-events state))
                         (sleep 0.01))))
        (shutdown display event-queue))))
  0)

(defun main ()
  (float-features:with-float-traps-masked
      (:divide-by-zero :invalid :inexact :overflow :underflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %main))))
