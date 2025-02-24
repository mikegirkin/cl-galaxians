(in-package #:galaxians)

(defvar *display*)

(defun main ()
  (al:init)                                    ; al_init();
  (al:install-keyboard)
  (al:init-primitives-addon)                   ; al_init_primitives_addon()
  (al:set-new-display-flags '(:windowed :resizable :opengl)) ; al_set_new_display_flags(ALLEGRO_WINDOWED | ALLEGRO_RESIZABLE);
  (al:set-new-display-option :vsync 0 :require) ; al_set_new_display_option(ALLEGRO_VSYNC, 1, ALLEGRO_REQUIRE);
  (setf *display* (al:create-display 800 600))   ; display = al_create_display(800, 600);
  (al:clear-to-color (al:map-rgb 128 128 128)) ; al_clear_to_color(al_map_rgb(128 128 128);
  (al:draw-filled-rectangle
   100 110 400 450
   (al:map-rgb 255 255 255))                   ; al_draw_filled_rectangle(100, 110, 400, 450, al_map_rgb(128 128 128));
  (al:flip-display)                            ; al_flip_display();
  (al:rest-time 2)                             ; al_rest(2);
  (al:destroy-display *display*)                 ; al_destroy_display(display);
  (al:uninstall-system))                       ; al_uninstall_system();
