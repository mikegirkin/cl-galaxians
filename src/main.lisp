(in-package :galaxians)

(defparameter +window-width+ (* 320 +scale+))
(defparameter +window-height+ (* 200 +scale+))

(defun initialize ()
  (unless (al:init)
    (error "Initializing liballegro failed"))
  (unless (al:init-primitives-addon)
    (error "Initializing primitives addon failed"))
  (unless (al:init-image-addon)
   (error "Initializing image addon failed"))
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

(defun shutdown (display event-queue frame-timer)
  (al:inhibit-screensaver nil)
  (al:stop-timer frame-timer)
  (al:destroy-timer frame-timer)
  (al:destroy-display display)
  (al:destroy-event-queue event-queue)
  (al:stop-samples)
  (al:uninstall-system)
  (al:uninstall-audio)
  (al:shutdown-ttf-addon)
  (al:shutdown-font-addon)
  (al:shutdown-image-addon)
  (al:shutdown-primitives-addon))

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
           (frame-timer (al:create-timer))
           (game-state (make-initial-game-state)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Galaxians")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:register-event-source event-queue
                                (al:get-timer-event-source timer))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      ;; (al:install-mouse)
      ;; (al:register-event-source event-queue
      ;;                           (al:get-mouse-event-source))
      (load-sprites! game-state)
      (initialize-enemies! game-state)
      (unwind-protect
           (progn
             (livesupport:setup-lisp-repl)
             (trivial-garbage:gc :full t)
             (al:with-current-keyboard-state state
               (loop :named event-loop
                     :with ticks :of-type double-float := (al:get-time)
                     :with last-repl-update :of-type double-float := ticks
                     :with dt :of-type double-float := 0d0
                     :while (not (game-state-quit game-state))
                     :do (process-event-queue event-queue game-state)
                         (al:wait-for-event-timed event-queue (cffi:null-pointer) 0.005))))
        (shutdown display event-queue frame-timer))))
  0)

(defun handle-key-down-event (keycode game-state)
  (case keycode
    (:escape (setf (game-state-quit game-state) t))
    (:p (pprint game-state))
    (:w (setf (move-up (requested-player-actions game-state)) t))
    (:s (setf (move-down (requested-player-actions game-state)) t))
    (:a (setf (move-left (requested-player-actions game-state)) t))
    (:d (setf (move-right (requested-player-actions game-state)) t))))

(defun handle-key-up-event (keycode game-state)
  (case keycode
    (:w (setf (move-up (requested-player-actions game-state)) nil))
    (:s (setf (move-down (requested-player-actions game-state)) nil))
    (:a (setf (move-left (requested-player-actions game-state)) nil))
    (:d (setf (move-right (requested-player-actions game-state)) nil))))

(defun handle-timer-event (timer-count game-state)
  (update* game-state)
  (render game-state))

(defun process-event-queue (event-queue game-state)
  (cffi:with-foreign-object (event '(:union al:event))
    (loop :while (al:get-next-event event-queue event)
          :do (let ((event-type (cffi:foreign-slot-value event '(:union al:event) 'al::type)))
                (case event-type
                  (:key-down (handle-key-down-event (cffi:foreign-slot-value event
                                                                             '(:struct al:keyboard-event)
                                                                             'al::keycode)
                                                    game-state))
                  (:key-up (handle-key-up-event (cffi:foreign-slot-value event
                                                                         '(:struct al:keyboard-event)
                                                                         'al::keycode)
                                                game-state))
                  (:timer (handle-timer-event (cffi:foreign-slot-value event
                                                                       '(:struct al:timer-event)
                                                                       'al::count)
                                              game-state))
                  (:display-close (setf (game-state-quit game-state) t)))))))

(defun main ()
  (float-features:with-float-traps-masked
      (:divide-by-zero :invalid :inexact :overflow :underflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %main))))
