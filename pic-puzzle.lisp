(ql:quickload '(:sdl2 :sdl2-image :dexador :quri
		:cl-json :alexandria) :silent t)

(defpackage :pic-puzzle
  (:use :cl)
  (:export :main))

(in-package :pic-puzzle)


;;; ── Constants ─────────────────────────────────────────────────────────
(defparameter *max-size* 800)
(defparameter *num-x* 3)
(defparameter *num-y* 3)
;;(defparameter *keyword* "tiger")
(defparameter *keyword* "cat")
(defparameter *num-urls* 30)
(defparameter *debug-image* nil)   ; set to a path string to bypass Flickr

(defparameter *border-off*   5)
(defparameter *border-on*   10)
(defparameter *color-off* '(255 255 255))   ; white border when unselected
(defparameter *color-on*  '(  0   0   0))   ; black border when selected


;;; ── Flickr API ─────────────────────────────────────────────────────────
(defparameter *flickr-api-url* "https://api.flickr.com/services/rest/")

(defun read-flickr-keys (&optional (path "~/.flickr.keys"))
  (with-open-file (stream (merge-pathnames path (user-homedir-pathname))
                          :direction :input)
    (json:decode-json stream)))   ; return the full alist

;; Cached keys so we only read the file once
(defvar *flickr-keys* nil) 

(defun flickr-keys ()
  (or *flickr-keys*
      (setf *flickr-keys* (read-flickr-keys))))

(defun flickr-api-key ()
  ;; cl-json maps "api_key" -> :API--KEY
  (cdr (assoc :api--key (flickr-keys))))      

;; Not used in current code.  My be needed for future OAuth and write calls
(defun flickr-api-secret ()
  ;; cl-json maps "api_key" -> :API--KEY
  (cdr (assoc :api--secret (flickr-keys))))      

(defun flickr-call (method &rest params)
  "Call a Flickr API method and return the parsed JSON response."
  (let* ((query (list* (cons "method"         method)
                       (cons "api_key"        (flickr-api-key))
                       (cons "format"         "json")
                       (cons "nojsoncallback" "1")
                       (loop for (k v) on params by #'cddr
                             collect (cons k v))))
         (uri (quri:make-uri :defaults *flickr-api-url*
                             :query query))
         (response (dex:get uri)))
    (json:decode-json-from-string response)))

(defun form-photo-url (photo &optional (size "b"))
  "Construct a Flickr image URL from a photo alist."
  (format nil "https://farm~A.staticflickr.com/~A/~A_~A_~A.jpg"
          (cdr (assoc :farm   photo))
          (cdr (assoc :server photo))
          (cdr (assoc :id     photo))
          (cdr (assoc :secret photo))
          size))

(defun flickr-get-interesting (&optional (num-urls 10))
  "Return a random photo URL from Flickr interestingness."
  (let* ((result (flickr-call "flickr.interestingness.getList"
                              "per_page" (write-to-string num-urls)))
         (photos (cdr (assoc :photo
                       (cdr (assoc :photos result))))))
    (form-photo-url (nth (random (length photos)) photos))))


(defun flickr-get-keyword (keyword &optional (num-urls 10))
  "Return a random photo URL matching keyword, using url_l directly."
  (let* ((result (flickr-call "flickr.photos.search"
                              "text"     keyword
                              "tags"     keyword
                              "extras"   "url_l"
                              "per_page" (write-to-string num-urls)))
         (photos (cdr (assoc :photo
			     (cdr (assoc :photos result))))))
    ;; cl-json: "url_l" -> :URL--L
    (cdr (assoc :url--l
                (nth (random (length photos)) photos)))))


;;; ── Cell struct ───────────────────────────────────────────────────────
;;
;; Each tile knows:
;;   initial-pos  -- where it started (ix . iy), used to check if solved
;;   current-pos  -- where it is now
;;   src-rect     -- the sub-rectangle of the full texture to draw
;;   selected     -- boolean
;;
(defstruct cell
  initial-pos
  current-pos
  src-rect
  (selected nil))

(defun cell-right-place-p (cell)
  (equal (cell-initial-pos cell) (cell-current-pos cell)))

(defun cell-toggle-selected (cell)
  "Toggle selected state; return new state."
  (setf (cell-selected cell) (not (cell-selected cell))))


;;; ── Grid ──────────────────────────────────────────────────────────────
;;
;; The grid is a hash table mapping (ix . iy) -> cell.
;; All cells share the single full-image texture; src-rect selects the tile.
;;
(defun make-grid (num-x num-y tex-w tex-h)
  "Compute src-rects by mapping tile boundaries proportionally into texture space."
  (let ((grid (make-hash-table :test #'equal)))
    (dotimes (ix num-x grid)
      (dotimes (iy num-y)
        (let* ((pos  (cons ix iy))
               ;; Map tile pixel boundaries into texture space
               (sx   (round (* tex-w ix) num-x))
               (sy   (round (* tex-h iy) num-y))
               (ex   (round (* tex-w (1+ ix)) num-x))
               (ey   (round (* tex-h (1+ iy)) num-y)))
          (setf (gethash pos grid)
                (make-cell
                 :initial-pos pos
                 :current-pos pos
                 :src-rect (sdl2:make-rect sx sy
					   (- ex sx) (- ey sy)))))))))

(defun grid-get (grid ix iy)
  (gethash (cons ix iy) grid))

(defun grid-done-p (grid num-x num-y)
  (dotimes (ix num-x t)
    (dotimes (iy num-y)
      (unless (cell-right-place-p (grid-get grid ix iy))
        (return-from grid-done-p nil)))))

(defun grid-swap (grid ix1 iy1 ix2 iy2)
  "Exchange the two cells at (ix1,iy1) and (ix2,iy2)."
  (let ((cell1 (grid-get grid ix1 iy1))
        (cell2 (grid-get grid ix2 iy2)))
    (setf (cell-current-pos cell1) (cons ix2 iy2)
          (cell-current-pos cell2) (cons ix1 iy1))
    (setf (gethash (cons ix1 iy1) grid) cell2
          (gethash (cons ix2 iy2) grid) cell1)))

(defun grid-randomize (grid num-x num-y)
  "Shuffle using repeated random swaps (Fisher-Yates on the grid)."
  (let ((positions (loop for ix below num-x
                         nconc (loop for iy below num-y
                                     collect (cons ix iy)))))
    (loop for i from (1- (length positions)) downto 1
          for j = (random (1+ i))
          for posi = (nth i positions)
          for posj = (nth j positions)
          do (grid-swap grid (car posi) (cdr posi)
                             (car posj) (cdr posj)))))


;;; ── Drawing helpers ───────────────────────────────────────────────────

(defun fit-dimensions (img-w img-h max-size num-x num-y)
  "Window and source crop dimensions, both exact multiples of tile count."
  (let* ((scale    (/ (float max-size) (max img-w img-h)))
         ;; Snap source crop to tile boundaries first
         (crop-w   (* num-x (truncate (* img-w scale) num-x)))
         (crop-h   (* num-y (truncate (* img-h scale) num-y)))
         ;; Window matches crop exactly
         (win-w    crop-w)
         (win-h    crop-h))
    (values win-w win-h)))

(defun draw-cell (renderer texture cell tile-w tile-h)
  "Blit one tile from the texture, with a filled border underneath."
  (let* ((pos    (cell-current-pos cell))
         (ix     (car pos))
         (iy     (cdr pos))
         (px     (* ix tile-w))
         (py     (* iy tile-h))
         (bord   (if (cell-selected cell) *border-on* *border-off*))
         (color  (if (cell-selected cell) *color-on* *color-off*))
         ;; Inset dest rect by border thickness on all sides
         (dest   (sdl2:make-rect (+ px bord)
                                 (+ py bord)
                                 (- tile-w (* 2 bord))
                                 (- tile-h (* 2 bord)))))
    ;; 1. Fill the full tile area with border color
    (sdl2:set-render-draw-color renderer
                                (first color) (second color) (third color) 255)
    (sdl2:render-fill-rect renderer (sdl2:make-rect px py tile-w tile-h))

    ;; 2. Blit the inset portion of the image on top
    (sdl2:render-copy renderer texture
                      :source-rect (let ((sr (cell-src-rect cell)))
				     (sdl2:make-rect
                                      (+ (sdl2:rect-x sr) bord)
				      (+ (sdl2:rect-y sr) bord)
				      (- (sdl2:rect-width  sr) (* 2 bord))
				      (- (sdl2:rect-height sr) (* 2 bord))))
                      :dest-rect dest)))

(defun draw-grid (renderer texture grid num-x num-y tile-w tile-h)
  (dotimes (ix num-x)
    (dotimes (iy num-y)
      (draw-cell renderer texture
                 (grid-get grid ix iy)
                 tile-w tile-h))))


;;; ── Image loading ─────────────────────────────────────────────────────

(defun load-image-from-url (url)
  ;; make a flag to use a static image (images/cat.bmp)
  (let ((tmp "/tmp/puzzle-img.jpg"))
    (dex:fetch url tmp :if-exists :supersede) ; download directly to file
    (sdl2-image:load-image tmp)))


(defun fetch-puzzle-texture (renderer keyword num-urls)
  "Download a Flickr image, load it into an SDL2 texture, return (texture w h)."
  (let* ((surface (if *debug-image*
                      (sdl2-image:load-image *debug-image*)
                      (load-image-from-url
                       (flickr-get-keyword keyword num-urls))))
         (w       (sdl2:surface-width surface))
         (h       (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (sdl2:free-surface surface)
    (values texture w h)))


;;; ── Main ──────────────────────────────────────────────────────────────

(defstruct game-state
  win renderer texture
  win-w win-h
  tile-w tile-h
  grid
  (done     nil)
  (hinting  nil)
  (selected nil))

(defun load-new-image (gs)
  (when (game-state-texture gs)
    (sdl2:destroy-texture (game-state-texture gs)))
  (multiple-value-bind (texture img-w img-h)
      (fetch-puzzle-texture (game-state-renderer gs) *keyword* *num-urls*)
    (multiple-value-bind (win-w win-h)
        (fit-dimensions img-w img-h *max-size* *num-x* *num-y*)
      (sdl2:set-window-size (game-state-win gs) win-w win-h)
      (setf (game-state-texture  gs) texture
            (game-state-win-w    gs) win-w
            (game-state-win-h    gs) win-h
            (game-state-tile-w   gs) (truncate win-w *num-x*)
            (game-state-tile-h   gs) (truncate win-h *num-y*)
            (game-state-grid     gs) (make-grid *num-x* *num-y* img-w img-h)
            (game-state-done     gs) nil
            (game-state-hinting  gs) nil
            (game-state-selected gs) nil)
      (grid-randomize (game-state-grid gs) *num-x* *num-y*))))

(defun handle-keydown (gs keysym)
  (let ((sc (sdl2:scancode-value keysym)))
    (cond
      ((or (sdl2:scancode= sc :scancode-escape)
           (sdl2:scancode= sc :scancode-q))
       (sdl2:push-event :quit))

      ((sdl2:scancode= sc :scancode-r)
       (load-new-image gs))

      ((sdl2:scancode= sc :scancode-h)
       (setf (game-state-hinting gs) t)))))

(defun handle-keyup (gs keysym)
  (let ((sc (sdl2:scancode-value keysym)))
    (when (sdl2:scancode= sc :scancode-h)
      (setf (game-state-hinting gs) nil))))

(defun handle-mouse (gs x y button)
  (when (and (= button sdl2-ffi:+sdl-button-left+)
             (not (game-state-done gs)))
    (let* ((tile-w (game-state-tile-w gs))
           (tile-h (game-state-tile-h gs))
           (grid   (game-state-grid gs))
           (ix     (truncate x tile-w))
           (iy     (truncate y tile-h)))
      (if (game-state-selected gs)
          (let ((ox (car (game-state-selected gs)))
                (oy (cdr (game-state-selected gs))))
            (cell-toggle-selected (grid-get grid ox oy))
            (unless (equal (cons ix iy) (game-state-selected gs))
              (grid-swap grid ix iy ox oy)
              (when (grid-done-p grid *num-x* *num-y*)
                (setf (game-state-done gs) t)))
            (setf (game-state-selected gs) nil))
          (progn
            (cell-toggle-selected (grid-get grid ix iy))
            (setf (game-state-selected gs) (cons ix iy)))))))

(defun handle-idle (gs)
  (let ((renderer (game-state-renderer gs))
        (texture  (game-state-texture  gs)))
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)
    (if (or (game-state-done gs) (game-state-hinting gs))
        (sdl2:render-copy renderer texture
                          :dest-rect (sdl2:make-rect 0 0
                                       (game-state-win-w gs)
                                       (game-state-win-h gs)))
        (draw-grid renderer texture
                   (game-state-grid gs)
                   *num-x* *num-y*
                   (game-state-tile-w gs)
                   (game-state-tile-h gs)))
    (sdl2:render-present renderer)))

;; SDL boilerplate
(defmacro with-sdl2-game ((win renderer title w h) &body body)
  `(sdl2:with-init (:video)
     (sdl2-image:init '(:jpg :png))
     (unwind-protect
         (sdl2:with-window (,win :title ,title :w ,w :h ,h :flags '(:shown))
           (sdl2:with-renderer (,renderer ,win :flags '(:accelerated))
             ,@body))
       (sdl2-image:quit))))

(defun main ()
  (with-sdl2-game (win renderer "Image Puzzle" *max-size* *max-size*)
    (let ((gs (make-game-state :win win :renderer renderer)))
      (load-new-image gs)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym) (handle-keydown gs keysym))
        (:keyup   (:keysym keysym) (handle-keyup   gs keysym))
        (:mousebuttondown (:x x :y y :button button)
			  (handle-mouse gs x y button))
        (:idle () (handle-idle gs))))))

;; run the program
(main)
