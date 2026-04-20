(ql:quickload '(:sdl2 :sdl2-image) :silent t)

(defpackage :ball-demo
  (:use :cl)
  (:export :main))

(in-package :ball-demo)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *image-dir* "./images/")
(defparameter *image-file* "ball.gif")
(defparameter *initial-speed* #(2 2))
(defparameter *delay* 10)


(defun load-image (filename)
  (let ((image (sdl2-image:load-image
                (merge-pathnames filename *image-dir*))))
    (if (autowrap:wrapper-null-p image)
        (error "Cannot load image ~a" filename)
        image)))

(defun rect-move (rect vel)
  (let ((dx (aref vel 0))
        (dy (aref vel 1)))

    (incf (sdl2:rect-x rect) dx)
    (when (or (< (sdl2:rect-x rect) 0)
              (> (+ (sdl2:rect-x rect) (sdl2:rect-width rect)) *screen-width*))
      (setf (aref vel 0) (- dx))
      (incf (sdl2:rect-x rect) (* -2 dx)))   ; push back inside

    (incf (sdl2:rect-y rect) dy)
    (when (or (< (sdl2:rect-y rect) 0)
              (> (+ (sdl2:rect-y rect) (sdl2:rect-height rect)) *screen-height*))
      (setf (aref vel 1) (- dy))
      (incf (sdl2:rect-y rect) (* -2 dy)))

    rect))


(defun main ()
  (sdl2:with-init (:video)
    (sdl2-image:init '(:jpg :png))
    (unwind-protect
	 (sdl2:with-window (win :title "SDL2 Ball Demo"
				:w *screen-width* :h *screen-height*
				:flags '(:shown))
	   (sdl2:with-renderer (renderer win :flags '(:accelerated))

             ;; Create a texture from the image file
             (let* ((image-surface (load-image *image-file*))
		    (texture (sdl2:create-texture-from-surface
                              renderer image-surface))
		    (img-w (sdl2:surface-width image-surface))
		    (img-h (sdl2:surface-height image-surface))
		    (current-velocity (copy-seq *initial-speed*))
		    (dest-rect (sdl2:make-rect 0 0 img-w img-h)))

               ;; surface no longer needed afterwards
               (sdl2:free-surface image-surface)

               ;; Main event loop
	       (unwind-protect
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

			     ;; Update position in place
			     (rect-move dest-rect current-velocity)

			     ;; Draw image
			     (sdl2:render-copy renderer texture
					       :source-rect nil
					       :dest-rect dest-rect)

			     (sdl2:render-present renderer)

			     ;; pause between updates
			     (sdl2:delay *delay*) ))

		 ;; cleanup even if with-event-loop exits uncleanly:
		 (sdl2:destroy-texture texture)) )))

      ;; explicit cleanup for sdl2-image even if error within with-window:
      (sdl2-image:quit) )))

(main)
