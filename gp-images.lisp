(ql:quickload '(:sdl2 :sdl2-image) :silent t)

(defpackage :gp-images
  (:use :cl)
  (:export :main))

(in-package :gp-images)


;;; ── Constants ──────────────────────────────────────────────────────────────
(defparameter *thumb-cols*      4)
(defparameter *thumb-rows*      3)
(defparameter *thumb-w*         100)
(defparameter *thumb-h*         100)
(defparameter *border*          5)
(defparameter *border-color*    '(20 20 20))
(defparameter *selected-color*  '(255 255 255))
(defparameter *multi-sel-color* '(150 150 150))
(defparameter *tile-w*          (+ *thumb-w* (* 2 *border*)))
(defparameter *tile-h*          (+ *thumb-h* (* 2 *border*)))
(defparameter *idle-delay*      50)   ;pause in ms

;;; GP/Evolution params;
(defparameter *max-depth*       6)    ; max initial tree depth
(defparameter *min-depth*       2)    ; min initial tree depth
(defparameter *max-nodes*       150)   ; discard offspring larger than this
;;Mutation rate scales inversely with tree size (Sims' key insight)
(defparameter *base-mutation-rate*  0.08)
(defparameter *const-adjust-prob*   0.3)   ; prob of nudging a constant vs replacing
(defparameter *const-adjust-amount* 0.3)   ; max nudge magnitude


;;; ── GP Function and Terminal Sets ──────────────────────────────────────────
;;
;; Each genome is an s-expression tree that maps (x y) -> value in [-1, 1].
;; Three separate trees produce R, G, B channels.
;; x and y are normalized to [-1, 1] across the image.
;;
;; Function set: (name arity)
;; Terminal set: x, y, random constants in [-1,1]
(defparameter *functions*
  '((+         2)
    (-         2)
    (*         2)
    (div-safe  2)    ; protected division
    (sin-pi    1)    ; sin(pi * x) -- more interesting than plain sin
    (cos-pi    1)
    (abs-val   1)
    (negate    1)
    (mod-safe  2)    ; protected mod
    (expt-safe 2)    ; protected expt
    (min-val   2)
    (max-val   2)
    (if-pos    3)    ; (if-pos condition then else)
    (mix       3)    ; linear interp: mix(a,b,t) = a*(1-t) + b*t
    (warp      3)))  ; domain warp: eval subtree at (x+dx, y+dy)

(defparameter *terminals*         '(x y))
(defparameter *const-probability*  0.3)   ; probability a terminal is a constant



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

(defun expt-safe (base e)
  (clamp (if (and (< base 0) (/= (round e) e)) 0.0
             (expt (clamp base) (clamp e)))))

(defun sin-pi  (x)   (sin (* pi x)))
(defun cos-pi  (x)   (cos (* pi x)))
(defun abs-val (x)   (abs x))
(defun negate  (x)   (- x))
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
     (let* ((fn    (random-function))
            (name  (first fn))
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

(defun count-nodes (expr)
  (if (atom expr) 1
      (1+ (reduce #'+ (mapcar #'count-nodes (cdr expr))))))


;;; ── Tree utilities ─────────────────────────────────────────────────────────
(defun collect-nodes (expr)
  "Return a flat list of all subtrees (including atoms) in expr."
  (if (atom expr)
      (list expr)
      (cons expr (mapcan #'collect-nodes (cdr expr)))))

(defun random-subtree (expr)
  "Pick a uniformly random subtree from expr."
  (let ((nodes (collect-nodes expr)))
    (when nodes
      (nth (random (length nodes)) nodes))))

(defun function-arity (name)
  (let ((entry (assoc name *functions*)))
    (when entry
      (second entry))))

(defun random-other-function (current-name current-arity)
  "Return a random function name with same arity, different from current."
  (let ((candidates (loop for (name arity) in *functions*
                          when (and (= arity current-arity)
                                    (not (eq name current-name)))
                          collect name)))
    (when candidates
      (nth (random (length candidates)) candidates))))


;;; ── Mutation ───────────────────────────────────────────────────────────────
;;
;; Implements all 7 of Sims' mutation types.
;; Per-node rate scales inversely with parent tree size.

(defun mutate-expr (expr parent-size)
  "Mutate an expression, using a rate scaled by parent-size."
  (let ((rate (/ *base-mutation-rate* (max 1.0 (/ parent-size 10.0)))))
    (labels ((mut (e depth)
               (cond
                 ;; Type 1: replace entire node with new random subtree
                 ((< (random 1.0) rate)
                  (random-expr (max 1 (- *max-depth* depth)) 0))

                 ;; Atom (terminal)
                 ((atom e)
                  (cond
                    ;; Type 2: nudge a numeric constant
                    ((numberp e)
                     (if (< (random 1.0) *const-adjust-prob*)
                         (clamp (+ e (* *const-adjust-amount*
                                        (- (random 2.0) 1.0))))
                         e))
                    (t e)))

                 ;; Compound node
                 ((consp e)
                  (let* ((fname (car e))
                         (fargs (cdr e))
                         (arity (length fargs)))
                    (cond
                      ;; Type 4: change function to another with same arity
                      ((< (random 1.0) rate)
                       (let ((new-fn (random-other-function fname arity)))
                         (cons (or new-fn fname)
                               (mapcar (lambda (a) (mut a (1+ depth))) fargs))))

                      ;; Type 5: wrap node inside a new random function
                      ((< (random 1.0) (* rate 0.5))
                       (let* ((new-fn    (random-function))
                              (new-name  (first  new-fn))
                              (new-arity (second new-fn))
                              (pos       (random new-arity)))
                         (cons new-name
                               (loop for i below new-arity
                                     collect (if (= i pos) e (random-terminal))))))

                      ;; Type 6: one argument replaces the whole node (inverse of 5)
                      ((< (random 1.0) (* rate 0.5))
                       (mut (nth (random arity) fargs) depth))

                      ;; Type 7: copy a sibling subtree into this position
                      ((< (random 1.0) (* rate 0.3))
                       (let ((other (random-subtree e)))
                         (if other other
                             (cons fname (mapcar (lambda (a) (mut a (1+ depth)))
                                                 fargs)))))

                      ;; Default: recurse into children
                      (t
                       (cons fname (mapcar (lambda (a) (mut a (1+ depth)))
                                           fargs)))))))))
      (mut expr 0))))

(defun mutate-genome (genome)
  "Mutate all three channels; discard any channel that grows too large."
  (let ((size (reduce #'+ (mapcar #'count-nodes genome))))
    (loop for channel in genome
          collect (let ((m (mutate-expr channel size)))
                    (if (> (count-nodes m) *max-nodes*) channel m)))))


;;; ── Crossover ──────────────────────────────────────────────────────────────
;;
;; Sims: "A node in expr1 is chosen at random and replaced by a node
;; chosen at random from expr2."
(defun replace-one-node (expr replacement)
  "Replace exactly one randomly chosen node in expr with replacement.
   Returns (values new-expr did-replace?)."
  (cond
    ;; Atom: replace with some probability
    ((atom expr)
     (if (< (random 1.0) 0.3)
         (values replacement t)
         (values expr nil)))
    ;; Compound
    ((consp expr)
     (if (< (random 1.0) 0.15)
         (values replacement t)
         ;; Try to replace in exactly one child, left to right
         (let ((new-args nil) (replaced nil))
           (dolist (arg (cdr expr))
             (if replaced
                 (push arg new-args)
                 (multiple-value-bind (new-arg did-it)
                     (replace-one-node arg replacement)
                   (push new-arg new-args)
                   (setf replaced did-it))))
           (values (cons (car expr) (nreverse new-args)) replaced))))))

(defun crossover-expr (expr1 expr2)
  "Sims crossover: graft a random subtree of expr2 into expr1."
  (let* ((subtree (random-subtree expr2))
         (result  (replace-one-node expr1 subtree)))
    (if (> (count-nodes result) *max-nodes*) expr1 result)))

(defun crossover-genomes (genome1 genome2)
  "Crossover channel by channel."
  (loop for ch1 in genome1
        for ch2 in genome2
        collect (crossover-expr ch1 ch2)))


;;; ── Genetic Dissolve ───────────────────────────────────────────────────────
;;
;; Sims: copy identical nodes, wrap differing ones in (mix e1 e2 alpha).
;; Varying alpha 0->1 produces a smooth animation between two parent images.
(defun dissolve-expr (expr1 expr2 alpha)
  "Build a dissolved expression at blend alpha in [0.0, 1.0]."
  (cond
    ((equal expr1 expr2)
     expr1)
    ((and (atom expr1) (atom expr2))
     `(mix ,expr1 ,expr2 ,alpha))
    ((and (consp expr1) (consp expr2)
          (eq (car expr1) (car expr2))
          (= (length expr1) (length expr2)))
     (cons (car expr1)
           (mapcar (lambda (a b) (dissolve-expr a b alpha))
                   (cdr expr1) (cdr expr2))))
    (t
     `(mix ,expr1 ,expr2 ,alpha))))

(defun dissolve-genomes (genome1 genome2 alpha)
  (loop for ch1 in genome1
        for ch2 in genome2
        collect (dissolve-expr ch1 ch2 alpha)))


;;; ── Breeding ───────────────────────────────────────────────────────────────
(defun breed-offspring (parent-genomes)
  "Produce one child from 1 or more parents."
  (if (= (length parent-genomes) 1)
      (mutate-genome (first parent-genomes))
      (let* ((p1    (nth (random (length parent-genomes)) parent-genomes))
             (p2    (nth (random (length parent-genomes)) parent-genomes))
             (child (crossover-genomes p1 p2)))
        (mutate-genome child))))

(defun evolve-population (state)
  "Replace non-selected slots with offspring; selected individuals survive."
  (let* ((pop      (state-pop state))
         (renderer (state-renderer state))
         (selected (state-selected state)))
    (when (null selected)
      ;; or maybe just mutate each cell??
      (format t "~&No parents selected -- click images first, then press E.~%")
      (return-from evolve-population nil))
    (let ((parent-genomes
           (loop for pos in selected
                 collect (individual-genome
                          (nth (+ (* (cdr pos) *thumb-cols*) (car pos)) pop)))))
      (format t "~&Breeding ~A offspring from ~A parent(s)...~%"
              (- (* *thumb-cols* *thumb-rows*) (length selected))
              (length parent-genomes))
      (loop for ind in pop
            for i from 0
            do (unless (individual-selected ind)
                 (let* ((child   (breed-offspring parent-genomes))
                        (pixels  (eval-genome-to-pixels child *thumb-w* *thumb-h*))
                        (texture (pixels-to-texture renderer pixels *thumb-w* *thumb-h*)))
                   (free-individual ind)
                   (setf (nth i (state-pop state))
                         (make-individual :genome child :texture texture
                                          :pixels pixels)))))
      (clear-selected state)
      (setf (state-dirty state) t)
      (format t "~&Evolution complete.~%"))))

(defun dissolve-selected (state)
  "Fill non-selected slots with dissolve steps between exactly 2 parents."
  (let* ((selected (state-selected state))
         (pop      (state-pop state))
         (renderer (state-renderer state)))
    (unless (= (length selected) 2)
      (format t "~&Dissolve requires exactly 2 selected images (currently ~A selected).~%"
              (length selected))
      (return-from dissolve-selected nil))
    (let* ((pos1    (first  selected))
           (pos2    (second selected))
           (genome1 (individual-genome
                     (nth (+ (* (cdr pos1) *thumb-cols*) (car pos1)) pop)))
           (genome2 (individual-genome
                     (nth (+ (* (cdr pos2) *thumb-cols*) (car pos2)) pop)))
           (n-steps (- (* *thumb-cols* *thumb-rows*) 2))
           (step    0))
      (format t "~&Dissolving in ~A steps...~%" n-steps)
      (loop for ind in pop
            for i from 0
            do (unless (individual-selected ind)
                 (incf step)
                 (let* ((alpha   (/ (float step) (1+ n-steps)))
                        (child   (dissolve-genomes genome1 genome2 alpha))
                        (pixels  (eval-genome-to-pixels child *thumb-w* *thumb-h*))
                        (texture (pixels-to-texture renderer pixels *thumb-w* *thumb-h*)))
                   (free-individual ind)
                   (setf (nth i (state-pop state))
                         (make-individual :genome child :texture texture
                                          :pixels pixels)))))
      (clear-selected state)
      (setf (state-dirty state) t)
      (format t "~&Dissolve complete.~%"))))


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
          (clamp (apply fn (mapcar (lambda (a) (eval-expr a x y)) args)))))))

    ;; fallback for anything unexpected
    (t 0.0)))

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
        (let* ((x   (- (* 2.0 (/ px (float (1- width))))  1.0))
               (y   (- (* 2.0 (/ py (float (1- height)))) 1.0))
               (r   (value-to-byte (eval-expr r-expr x y)))
               (g   (value-to-byte (eval-expr g-expr x y)))
               (b   (value-to-byte (eval-expr b-expr x y)))
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

(defun render-thumbnail (renderer texture ix iy sel-color
                         tile-w tile-h border) ;these could be constants
  "Blit a thumbnail texture to grid position (ix, iy)."
  (let* ((x     (* ix tile-w))
         (y     (* iy tile-h))
         (outer (sdl2:make-rect x y tile-w tile-h))
         (inner (sdl2:make-rect (+ x border) (+ y border)
                                (- tile-w (* 2 border))
                                (- tile-h (* 2 border)))))
    ;; first render the border, depending on selected state
    (when sel-color
      (sdl2:set-render-draw-color renderer
        (first sel-color) (second sel-color) (third sel-color) 255)
      (sdl2:render-fill-rect renderer outer))
    (sdl2:render-copy renderer texture :dest-rect inner)))


;;; ── Population ─────────────────────────────────────────────────────────────
(defstruct individual
  genome
  texture
  pixels
  (selected nil))

(defun make-random-individual (renderer thumb-w thumb-h)
  (let* ((genome  (make-rgb-genome))
         (pixels  (eval-genome-to-pixels genome thumb-w thumb-h))
         (texture (pixels-to-texture renderer pixels thumb-w thumb-h)))
    (make-individual :genome genome :texture texture :pixels pixels)))

(defun free-individual (ind)
  (when (individual-texture ind)
    (sdl2:destroy-texture (individual-texture ind))))

(defun make-population (renderer n thumb-w thumb-h)
  "Create N random individuals."
  (format t "~&Generating ~A random genomes...~%" n)
  (loop repeat n collect (make-random-individual renderer thumb-w thumb-h)))

(defun free-population (pop)
  (mapc #'free-individual pop))


;;; ── State struct ───────────────────────────────────────────────────────────
(defstruct state
  pop
  renderer
  (selected nil)
  (dirty t))

;; implicit defun of (make-state :pop pop :renderer renderer :selected nil :dirty t)

(defun free-state (s)
  (free-population (state-pop s)))

(defun toggle-selected (state ix iy)
  (let* ((idx   (+ (* iy *thumb-cols*) ix))
         (indiv (nth idx (state-pop state)))
         (pos   (cons ix iy)))
    (if (individual-selected indiv)
        (progn (setf (individual-selected indiv) nil)
               ;; if cell was selected remove it
               (setf (state-selected state)
                     (remove pos (state-selected state) :test #'equal)))
        (progn (setf (individual-selected indiv) t)
               ;; else add it
               (push pos (state-selected state))))))

(defun clear-selected (state)
  (dolist (pos (state-selected state))
    (let ((indiv (nth (+ (* (cdr pos) *thumb-cols*) (car pos))
                      (state-pop state))))
      (setf (individual-selected indiv) nil)))
  (setf (state-selected state) nil))


;;; ── Event Handlers ─────────────────────────────────────────────────────────
(defun handle-keydown (state keysym)
  (let ((sc       (sdl2:scancode-value keysym))
        (renderer (state-renderer state))
        (pop-size (* *thumb-cols* *thumb-rows*)))
    (cond
      ;; Q or Escape: quit
      ((or (sdl2:scancode= sc :scancode-escape)
           (sdl2:scancode= sc :scancode-q))
       (sdl2:push-event :quit))

      ;; E: evolve -- breed offspring from selected parents into empty slots
      ((sdl2:scancode= sc :scancode-e)
       (evolve-population state))

      ;; D: dissolve -- fill grid with interpolations between 2 selected parents
      ((sdl2:scancode= sc :scancode-d)
       (dissolve-selected state))

      ;; M: mutate selected images in place
      ((sdl2:scancode= sc :scancode-m)
       (dolist (pos (state-selected state))
         (let* ((idx     (+ (* (cdr pos) *thumb-cols*) (car pos)))
                (ind     (nth idx (state-pop state)))
                (mutated (mutate-genome (individual-genome ind)))
                (pixels  (eval-genome-to-pixels mutated *thumb-w* *thumb-h*))
                (texture (pixels-to-texture renderer pixels *thumb-w* *thumb-h*)))
           (free-individual ind)
           (setf (nth idx (state-pop state))
                 (make-individual :genome mutated :texture texture
                                  :pixels pixels :selected t))))
       (setf (state-dirty state) t))

      ;; X: clear selection without doing anything
      ((sdl2:scancode= sc :scancode-x)
       (clear-selected state)
       (setf (state-dirty state) t))

      ;; R: full random reset
      ((sdl2:scancode= sc :scancode-r)
       (free-population (state-pop state))
       (clear-selected state)
       (setf (state-pop   state)
             (make-population renderer pop-size *thumb-w* *thumb-h*)
             (state-dirty state) t))

      ;; P: print selected genomes to REPL for inspection / saving
      ((sdl2:scancode= sc :scancode-p)
       (dolist (pos (state-selected state))
         (let* ((idx (+ (* (cdr pos) *thumb-cols*) (car pos)))
                (g   (individual-genome (nth idx (state-pop state)))))
           (format t "~&Genome at ~A:~%  R: ~S~%  G: ~S~%  B: ~S~%~%"
                   pos (first g) (second g) (third g))))))))

(defun handle-mouse (state x y button)
  (when (= button sdl2-ffi:+sdl-button-left+)
    (let ((ix       (truncate x *tile-w*))
          (iy       (truncate y *tile-h*)))
      ;;(format t "Mouse click: ~a ~a ==> ~a ~a~%" x y ix iy)
      (toggle-selected state ix iy)
      ;;(format t "State-selected = ~a~%" (state-selected state))
      (setf (state-dirty state) t))))

(defun handle-idle (state)
  (when (state-dirty state)
    (let ((renderer     (state-renderer state))
          (pop          (state-pop state))
          (num-selected (length (state-selected state))))
      (sdl2:set-render-draw-color renderer
        (first *border-color*) (second *border-color*) (third *border-color*) 255)
      (sdl2:render-clear renderer)
      (loop for ind in pop
            for i from 0
            do (let ((ix (mod i *thumb-cols*))
                     (iy (truncate i *thumb-cols*))
                     (selected (when (individual-selected ind)
                                 (if (> num-selected 1)
                                     *multi-sel-color*
                                     *selected-color*))))
                 (render-thumbnail renderer (individual-texture ind)
                                   ix iy selected
                                   *tile-w* *tile-h* *border*)))
      (sdl2:render-present renderer)
      (setf (state-dirty state) nil)))
  (sdl2:delay *idle-delay*))


;;; ── Main ───────────────────────────────────────────────────────────────────
(defun main ()
  ;; usage guide:
  (format t "~&
GP Image Evolver
  Click          select / deselect
  E              evolve: breed offspring from selected parent(s)
  D              dissolve: interpolate between exactly 2 selected
  M              mutate selected in place
  X              clear selection
  R              random reset
  P              print genome(s) to REPL
  Q / Escape     quit
~%")
  ;; calc screen-width / screen-height
  (let ((screen-w (* *thumb-cols* *tile-w*))
        (screen-h (* *thumb-rows* *tile-h*))
        (title "GP Image Evolver  E=evolve D=dissolve M=mutate R=reset"))
    (sdl2:with-init (:video)
      (sdl2:with-window (win :title title :w screen-w :h screen-h :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (let* ((pop-size (* *thumb-cols* *thumb-rows*))
                 (pop      (make-population renderer pop-size *thumb-w* *thumb-h*))
                 (state    (make-state :renderer renderer :pop pop)))
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:keydown (:keysym keysym) (handle-keydown state keysym))
              (:mousebuttondown (:x x :y y :button button)
                                (handle-mouse state x y button))
              (:idle () (handle-idle state)))
            (free-state state)
            t))))))                       ;return t instead of the pop list

(main)
