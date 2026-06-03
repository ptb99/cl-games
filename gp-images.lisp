(ql:quickload '(:sdl2 :sdl2-image) :silent t)

(defpackage :gp-images
  (:use :cl)
  (:export :main))

(in-package :gp-images)


;;; ── Constants ──────────────────────────────────────────────────────────────
(defparameter *tile-w*        100)
(defparameter *tile-h*        100)
(defparameter *border*        5)
(defparameter *thumb-cols*    4)
(defparameter *thumb-rows*    3)
(defparameter *max-depth*     6)    ; max initial tree depth
(defparameter *min-depth*     2)    ; min initial tree depth


;;; ── GP Function and Terminal Sets ──────────────────────────────────────────
;;
;; Each genome is an s-expression tree that maps (x y) -> value in [-1, 1].
;; Three separate trees produce R, G, B channels.
;; x and y are normalized to [-1, 1] across the image.
;;
;; Function set: (name arity)
;; Terminal set: x, y, random constants in [-1,1]
(defparameter *functions*
  '((+        2)
    (-        2)
    (*        2)
    (div-safe 2)    ; protected division
    (sin-pi   1)    ; sin(pi * x) -- more interesting than plain sin
    (cos-pi   1)
    (abs-val  1)
    (negate   1)
    (mod-safe 2)    ; protected mod
    (expt-safe 2)   ; protected expt
    (min-val  2)
    (max-val  2)
    (if-pos   3)    ; (if-pos condition then else)
    (mix      3)    ; linear interp: mix(a,b,t) = a*(1-t) + b*t
    (warp     3)))  ; domain warp: eval subtree at (x+dx, y+dy)

(defparameter *terminals*
  '(x y))

(defparameter *const-probability* 0.3)   ; probability a terminal is a constant


;;; ── Safe math primitives ───────────────────────────────────────────────────
;;
;; All functions clamp output to [-1, 1] to prevent runaway values.
(declaim (inline clamp))
(defun clamp (v)
  (max -1.0 (min 1.0 (float v 1.0))))

(defun div-safe (a b)
  (if (< (abs b) 1e-6) 0.0 (clamp (/ a b))))

(defun mod-safe (a b)
  (if (< (abs b) 1e-6) 0.0 (clamp (mod a (+ (abs b) 1e-6)))))

(defun expt-safe (base exp)
  (clamp (if (and (< base 0) (/= (round exp) exp))
             0.0
             (expt (clamp base) (clamp exp)))))

(defun sin-pi (x)  (sin  (* pi x)))
(defun cos-pi (x)  (cos  (* pi x)))
(defun abs-val (x) (abs x))
(defun negate  (x) (- x))
(defun min-val (a b) (min a b))
(defun max-val (a b) (max a b))

(defun if-pos (condition then else)
  (if (>= condition 0.0) then else))

(defun mix (a b tval)
  (let ((tval (clamp tval)))
    (+ (* a (- 1.0 tval)) (* b tval))))

;; warp is handled specially during eval since it modifies x/y


;;; ── Genome Generation ──────────────────────────────────────────────────────
(defun random-const ()
  "Return a random float constant in [-1, 1]."
  (- (random 2.0) 1.0))

(defun random-terminal ()
  "Return a terminal: x, y, or a random constant."
  (if (< (random 1.0) *const-probability*)
      (random-const)
      (nth (random (length *terminals*)) *terminals*)))

(defun random-function ()
  "Return a random (name arity) pair from the function set."
  (nth (random (length *functions*)) *functions*))

(defun random-expr (&optional (max-depth *max-depth*) (min-depth *min-depth*))
  "Generate a random expression tree.
   At max-depth or probabilistically, return a terminal.
   Below min-depth, always recurse to ensure minimum complexity."
  (cond
    ;; Must go deeper -- never return terminal yet
    ((> min-depth 0)
     (let* ((fn   (random-function))
            (name (first fn))
            (arity (second fn)))
       (cons name (loop repeat arity
                        collect (random-expr (1- max-depth)
                                             (1- min-depth))))))
    ;; At max depth -- must return terminal
    ((= max-depth 0)
     (random-terminal))
    ;; In between -- 50/50 terminal or recurse
    (t
     (if (< (random 1.0) 0.5)
         (random-terminal)
         (let* ((fn    (random-function))
                (name  (first fn))
                (arity (second fn)))
           (cons name (loop repeat arity
                            collect (random-expr (1- max-depth) 0))))))))

(defun make-rgb-genome ()
  "Create a genome: a list of three expression trees (R G B)."
  (list (random-expr)
        (random-expr)
        (random-expr)))


;;; ── Genome Evaluation ──────────────────────────────────────────────────────
(defun eval-expr (expr x y)
  "Evaluate an expression tree at normalized coordinates (x y) in [-1,1].
   Returns a float in [-1, 1]."
  (cond
    ;; Terminals
    ((eq  expr 'x)   x)
    ((eq  expr 'y)   y)
    ((numberp expr)  (clamp expr))

    ;; Compound expressions
    ((consp expr)
     (let ((fn   (car expr))
           (args (cdr expr)))
       (case fn
         ;; warp: evaluate first arg as dx, second as dy,
         ;; then evaluate third arg at warped coordinates
         (warp
          (let ((dx (eval-expr (first  args) x y))
                (dy (eval-expr (second args) x y)))
            (eval-expr (third args)
                       (clamp (+ x dx))
                       (clamp (+ y dy)))))
         ;; All other functions: evaluate args then apply
         (otherwise
          (let ((evaled (mapcar (lambda (a) (eval-expr a x y)) args)))
            (clamp (apply fn evaled)))))))

    (t 0.0)))  ; fallback for anything unexpected

(defun value-to-byte (v)
  "Convert a [-1,1] float to a [0,255] byte."
  (round (* 255 (/ (+ v 1.0) 2.0))))

(defun eval-genome-to-pixels (genome width height)
  "Evaluate an RGB genome across a width x height grid.
   Returns a flat (unsigned-byte 8) array in RGB format."
  (let ((pixels (make-array (* width height 3)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0))
        (r-expr (first  genome))
        (g-expr (second genome))
        (b-expr (third  genome)))
    (dotimes (py height)
      (dotimes (px width)
        ;; Normalize pixel coords to [-1, 1]
        (let* ((x  (- (* 2.0 (/ px (float (1- width))))  1.0))
               (y  (- (* 2.0 (/ py (float (1- height)))) 1.0))
               (r  (value-to-byte (eval-expr r-expr x y)))
               (g  (value-to-byte (eval-expr g-expr x y)))
               (b  (value-to-byte (eval-expr b-expr x y)))
               (idx (* 3 (+ (* py width) px))))
          (setf (aref pixels idx)       r
                (aref pixels (+ idx 1)) g
                (aref pixels (+ idx 2)) b))))
    pixels))


;;; ── SDL2 Rendering ─────────────────────────────────────────────────────────
(defun pixels-to-texture (renderer pixels width height)
  "Upload a flat RGB pixel array to an SDL2 texture."
  (let ((texture (sdl2:create-texture renderer
                                       sdl2-ffi:+sdl-pixelformat-rgb24+
                                       sdl2-ffi:+sdl-textureaccess-streaming+
                                       width height)))
    (cffi:with-pointer-to-vector-data (ptr pixels)
      (sdl2:update-texture texture nil ptr (* width 3)))
    texture))

(defun render-thumbnail (renderer texture ix iy tile-w tile-h border)
  "Blit a thumbnail texture to grid position (ix, iy)."
  (let ((dest (sdl2:make-rect (+ (* ix tile-w) border)
                              (+ (* iy tile-h) border)
                              (- tile-w (* 2 border))
			      (- tile-h (* 2 border)))))
    (sdl2:render-copy renderer texture :dest-rect dest)))


;;; ── Population ─────────────────────────────────────────────────────────────
(defstruct individual
  genome
  texture
  pixels)

(defun make-genome (renderer thumb-w thumb-h)
  "Generate a random genome, evaluate it, upload texture."
  (let* ((genome (make-rgb-genome))
         (pixels (eval-genome-to-pixels genome thumb-w thumb-h))
         (texture (pixels-to-texture renderer pixels thumb-w thumb-h)))
    (make-individual :genome genome :texture texture :pixels pixels)))

(defun free-individual (ind)
  (when (individual-texture ind)
    (sdl2:destroy-texture (individual-texture ind))))

(defun make-population (renderer n thumb-w thumb-h)
  "Create N random individuals."
  (format t "~&Generating ~A random genomes...~%" n)
  (loop repeat n collect (make-genome renderer thumb-w thumb-h)))

(defun free-population (pop)
  (mapc #'free-individual pop))


;;; ── Event loop functions  ──────────────────────────────────────────────────
(defun handle-keydown (state keysym)
  (let ((sc       (sdl2:scancode-value keysym))
        (pop-size (* *thumb-cols* *thumb-rows*))
	(renderer (cdr (assoc :renderer state)))
	(pop      (cdr (assoc :pop state))))
    (cond
      ;; Q or Escape: quit
      ((or (sdl2:scancode= sc :scancode-escape)
           (sdl2:scancode= sc :scancode-q))
       (sdl2:push-event :quit))

      ;; R: regenerate entire population
      ((sdl2:scancode= sc :scancode-r)
       (format t "~&Regenerating population...~%")
       (free-population pop)
       (setf (cdr (assoc :pop state))
	     (make-population renderer pop-size *tile-w* *tile-h*)
             (cdr (assoc :dirty state))
	     t))

      ;; Space: regenerate a single random individual
      ((sdl2:scancode= sc :scancode-space)
       (let ((idx (random pop-size)))
         (free-individual (nth idx pop))
         (setf (nth idx pop)
               (make-genome renderer *tile-w* *tile-h*))
         (setf (cdr (assoc :dirty state)) t))) )))

(defun handle-mouse (state x y button)
  (when (= button sdl2-ffi:+sdl-button-left+)
    (let* ((tile-w (+ *tile-w* (* 2 *border*)))
	   (tile-h (+ *tile-h* (* 2 *border*)))
	   (ix (truncate x tile-w))
	   (iy (truncate y tile-h)))
    (format t "Mouse click: ~a ~a ==> ~a ~a~%" x y ix iy))))

(defun handle-idle (state)
  (when (cdr (assoc :dirty state))
    (let ((renderer (cdr (assoc :renderer state)))
	  (pop      (cdr (assoc :pop state))))
    (sdl2:set-render-draw-color renderer 20 20 20 255)
    (sdl2:render-clear renderer)
    (loop for ind in pop
          for i from 0
          do (let ((ix (mod i *thumb-cols*))
                   (iy (truncate i *thumb-cols*)))
               (render-thumbnail renderer
                                 (individual-texture ind)
                                 ix iy
				 (+ *tile-w* (* 2 *border*))
				 (+ *tile-h* (* 2 *border*))
				 *border*)))
    (sdl2:render-present renderer)
    (setf (cdr (assoc :dirty state)) nil)))
  (sdl2:delay 16))


;;; ── Main ───────────────────────────────────────────────────────────────────
(defun main ()
  ;; calc screen-width / screen-height
  (let ((screen-width  (* *thumb-cols* (+ *tile-w* (* 2 *border*))))
	(screen-height (* *thumb-rows* (+ *tile-h* (* 2 *border*)))))
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title "GP Image Explorer"
                             :w screen-width :h screen-height
                             :flags '(:shown))
	(sdl2:with-renderer (renderer win :flags '(:accelerated))
          (let* ((pop-size (* *thumb-cols* *thumb-rows*))
		 (pop      (make-population renderer pop-size *tile-w* *tile-h*))
		 (state    (list (cons :renderer renderer)
				 (cons :pop pop)
				 (cons :dirty t))))

          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:keydown (:keysym keysym) (handle-keydown state keysym))
	    (:mousebuttondown (:x x :y y :button button)
			      (handle-mouse state x y button))
            (:idle () (handle-idle state)))

          (free-population (cdr (assoc :pop state)))
	  t))))))			;return t instead of pop list

;; Print a sample genome to the REPL for inspection
;;(format t "~&Sample genome:~%~S~%~%" (make-rgb-genome))

(main)
