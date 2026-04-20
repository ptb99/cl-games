(ql:quickload '(:sdl2) :silent t)

(defpackage :hello-sdl2
  (:use :cl))

(in-package :hello-sdl2)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "Hello SDL2!"
                           :w *screen-width* :h *screen-height*
                           :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))

        ;; Main event loop
        (sdl2:with-event-loop (:method :poll)

          ;; Window close button or Alt-F4
          (:quit () t)

          ;; Key handler
          (:keydown (:keysym keysym)
	    (let ((scancode (sdl2:scancode-value keysym)))
	      (when (or (sdl2:scancode= scancode :scancode-escape)
			(sdl2:scancode= scancode :scancode-q))
		(sdl2:push-event :quit))))

          ;; Draw every frame
          (:idle ()
           ;; Dark blue background
           (sdl2:set-render-draw-color renderer 30 30 80 255)
           (sdl2:render-clear renderer)

           ;; Draw a white rectangle in the center
           (sdl2:set-render-draw-color renderer 255 255 255 255)
           (sdl2:render-fill-rect renderer
             (sdl2:make-rect 270 190 100 100))

           ;; Draw a red diagonal line
           (sdl2:set-render-draw-color renderer 220 60 60 255)
           (sdl2:render-draw-line renderer 0 0 640 480)

           (sdl2:render-present renderer)))))))

(main)
