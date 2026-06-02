(ql:quickload '(:sdl2 :sdl2-image :dexador :quri
                :cl-json :alexandria) :silent t)

(defpackage :puzzle
  (:use :cl)
  (:export :main))

(in-package :puzzle)


;;; ── Constants ─────────────────────────────────────────────────────────
(defparameter *max-size* 800)
(defparameter *num-x* 5)
(defparameter *num-y* 5)
(defparameter *idle-delay* 50)          ;pause in ms

;;(defparameter *keyword* "tiger")
;;(defparameter *keyword* "cat")
(defparameter *keyword* "kitty")
;;(defparameter *keyword* nil)          ;use interestingness query

(defparameter *num-urls* 20)
(defparameter *image-size* "b")         ; z=640, c=800, b=1024, h=1600
(defparameter *debug-image* nil)        ; set to a path string to bypass Flickr

(defparameter *border-off*   5)
(defparameter *border-on*   10)
(defparameter *color-off* '(255 255 255))   ; white border when unselected
(defparameter *color-on*  '(  0   0   0))   ; black border when selected

;; use for debug-only printouts
(defparameter *debug* nil)
;;(defparameter *debug* t)


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

(defun flickr-get-interesting (&optional (num-urls 10) (page 1))
  "Return a list of photo assocsd from Flickr interestingness."
  (let* ((result (flickr-call "flickr.interestingness.getList"
                              "per_page" (write-to-string num-urls)
                              "page"     (write-to-string page)))
         (photos (cdr (assoc :photo
                             (cdr (assoc :photos result))))))
    photos))

(defun flickr-get-by-keyword (keyword &optional (num-urls 10) (page 1))
  "Return a list of photo URLs matching keyword."
  (let* ((result (flickr-call "flickr.photos.search"
                              "tags"     keyword
                              "sort"     "relevance"
                              "content_types" "0" ; photos only, no screens/art
                              "per_page" (write-to-string num-urls)
                              "page"     (write-to-string page)))
         (photos (cdr (assoc :photo
                             (cdr (assoc :photos result))))))
    photos))

(defun form-photo-url (photo &optional (size *image-size*))
  "Construct a Flickr image URL from a photo alist."
  (when photo
    (format nil "https://farm~A.staticflickr.com/~A/~A_~A_~A.jpg"
            (cdr (assoc :farm   photo))
            (cdr (assoc :server photo))
            (cdr (assoc :id     photo))
            (cdr (assoc :secret photo))
            size)))

(defun get-random-url (photos &optional (size *image-size*))
  "Choose a random pic from list of returned photo prop-lists"
  (form-photo-url (nth (random (length photos)) photos) size))

;; Use like this:
;; (get-random-url (flickr-get-interesting 6) "t")

(defun get-new-image (page)
  "Return a random photo assoc from Flickr API"
  (let ((photos (if *keyword*
                    (flickr-get-by-keyword *keyword* *num-urls* page)
                    (flickr-get-interesting *num-urls* page))))
    (nth (random (length photos)) photos)))

;; obsolete...
(defun flickr-debug-search (keyword &optional (num-urls 10))
  "Show all photo titles and URLs returned for a keyword search."
  (let* ((result (flickr-call "flickr.photos.search"
                              "tags"     keyword
                              "sort"     "relevance"
                              "content_types" "0" ; photos only, no screens/art
                              "extras"   "url_l"
                              "per_page" (write-to-string num-urls)))
         (photos (cdr (assoc :photo
                             (cdr (assoc :photos result))))))
    (format t "~&Found ~A photos for keyword: ~S~%" (length photos) keyword)
    (loop for p in photos
          for i from 1
          do (format t "~& ~2D. ~A~%     ~A~%"
                     i
                     (cdr (assoc :title p))
                     (cdr (assoc :url--l p))))
    (length photos)))


;;; ── Image loading ─────────────────────────────────────────────────────

(defun load-image-from-url (url)
  ;; make a flag to use a static image (images/cat.bmp)
  (let ((tmp "/tmp/puzzle-img.jpg"))
    (dex:fetch url tmp :if-exists :supersede) ; download directly to file
    (sdl2-image:load-image tmp)))

(defun fetch-puzzle-texture (renderer url)
  "Download a Flickr image, load it into an SDL2 texture, return (texture w h)."
  (let* ((surface (if *debug-image*
                      (sdl2-image:load-image *debug-image*)
                      (load-image-from-url url)))
         (w       (sdl2:surface-width surface))
         (h       (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (sdl2:free-surface surface)
    (when *debug* (format t "FETCH ~a - ~a x ~a~%" url w h))
    (values texture w h)))


;;; ── Sizing helper functions ───────────────────────────────────────────

(defun get-src-tile-size (ix iy num-x num-y src-w src-h)
  "Return (x y w h) dimensions for tile represented by ix,iy"
  (let* ((sx   (round (* src-w ix) num-x))
         (sy   (round (* src-h iy) num-y))
         (ex   (round (* src-w (1+ ix)) num-x))
         (ey   (round (* src-h (1+ iy)) num-y))
         (wx   (- ex sx))
         (wy   (- ey sy)))
    (values sx sy wx wy)))

(defun get-dest-tile-size (ix iy tile-w tile-h)
  "Return (x y w h) dimensions for tile represented by ix,iy"
  (let ((px  (* ix tile-w))
        (py  (* iy tile-h)))
    (values px py tile-w tile-h)))

(defun fit-win-dimensions (img-w img-h max-size num-x num-y)
  "Window and source crop dimensions, both exact multiples of tile count."
  (let* ((scale    (/ (float max-size) (max img-w img-h)))
         ;; Snap source crop to tile boundaries first
         (crop-w   (* num-x (truncate (* img-w scale) num-x)))
         (crop-h   (* num-y (truncate (* img-h scale) num-y)))
         ;; Window matches crop exactly
         (win-w    crop-w)
         (win-h    crop-h))
    (when *debug*
      (format t "fit-win-dim (img w ~d h ~d - scale ~f)~%" img-w img-h scale))
    (values win-w win-h)))


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
  ;;src-outer-rect   ;not actually needed
  src-inner-rect
  dest-outer-rect
  dest-inner-rect
  (selected nil))

(defun cell-right-place-p (c)
  (equal (cell-initial-pos c) (cell-current-pos c)))

(defun shrink-rect-by (rect delta)
  "Adjust (x,y,w,h) of rect by -delta along each border."
  (let ((x      (sdl2:rect-x rect))
        (y      (sdl2:rect-y rect))
        (w      (sdl2:rect-width rect))
        (h      (sdl2:rect-height rect)))
    (setf (sdl2:rect-x rect) (+ x delta)
          (sdl2:rect-y rect) (+ y delta)
          (sdl2:rect-width rect) (- w (* 2 delta))
          (sdl2:rect-height rect) (- h (* 2 delta)))))

(defun cell-toggle-selected (c)
  "Toggle selected state; revise dest-inner-rect to make black border larger."
  (let* ((selected (cell-selected c))
         (delta    (- *border-on* *border-off*))
	 (src      (cell-src-inner-rect c))
         (dest     (cell-dest-inner-rect c)))
    (if selected
        ;; make unselected - back to standard rect
	(progn
	  (shrink-rect-by src (* -1 delta))
	  (shrink-rect-by dest (* -1 delta)))
        ;; make selected - widen outer rect
	(progn
	  (shrink-rect-by src delta) ;fudge by not scaling by ratio dest/src
	  (shrink-rect-by dest delta)))
    (setf (cell-selected c) (not selected))))

;; obsolete (useful for debugging?)
(defun simple-cell-toggle-selected (c)
  "Dummy just to toggle selected state."
  (let ((selected (cell-selected c)))
    (setf (cell-selected c) (not selected))))


;;; ── Grid ──────────────────────────────────────────────────────────────
;;
;; The grid is a hash table mapping (ix . iy) -> cell.
;; All cells share the single full-image texture; src-rect selects the tile.
;;
(defun make-grid (num-x num-y tex-w tex-h tile-w tile-h)
  "Compute src-rects of image texture (i.e. tile boundaries)."
  (let ((grid (make-hash-table :test #'equal)))
    (dotimes (ix num-x grid)
      (dotimes (iy num-y)
        (let ((pos  (cons ix iy)))
          (multiple-value-bind (sx sy sw sh)
              (get-src-tile-size ix iy num-x num-y tex-w tex-h)
            (multiple-value-bind (dx dy dw dh)
                (get-dest-tile-size ix iy tile-w tile-h)
              (setf (gethash pos grid)
                    (make-cell
                     :initial-pos pos
                     :current-pos pos
                     ;;:src-outer-rect (sdl2:make-rect sx sy sw sw)
                     :src-inner-rect
                       (sdl2:make-rect (+ sx *border-off*)
                                       (+ sy *border-off*)
                                       (- sw (* 2 *border-off*))
                                       (- sh (* 2 *border-off*)))
                     :dest-outer-rect (sdl2:make-rect dx dy dw dh)
                     :dest-inner-rect
                       (sdl2:make-rect (+ dx *border-off*)
                                       (+ dy *border-off*)
                                       (- dw (* 2 *border-off*))
                                       (- dh (* 2 *border-off*))))))))))))

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
        (cell2 (grid-get grid ix2 iy2))
        (pos1 (cons ix1 iy1))
        (pos2 (cons ix2 iy2)))
    (setf (cell-current-pos cell1) pos2
          (cell-current-pos cell2) pos1)
    (let ((dest1 (cell-dest-inner-rect cell1))
          (dest2 (cell-dest-inner-rect cell2)))
      (setf (cell-dest-inner-rect cell1) dest2
            (cell-dest-inner-rect cell2) dest1))
    (let ((dest1 (cell-dest-outer-rect cell1))
          (dest2 (cell-dest-outer-rect cell2)))
      (setf (cell-dest-outer-rect cell1) dest2
            (cell-dest-outer-rect cell2) dest1))
    (setf (gethash pos1 grid) cell2
          (gethash pos2 grid) cell1)))

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

(defun draw-cell (renderer texture cell)
  "Blit one tile from the texture, with a filled border underneath."
  (let* ((selected-p (cell-selected cell))
         (color      (if selected-p *color-on* *color-off*))
         (source     (cell-src-inner-rect cell))
         (outer      (cell-dest-outer-rect cell))
         (dest       (cell-dest-inner-rect cell)))

    ;; 1. Fill the full tile area with border color
    (sdl2:set-render-draw-color renderer
          (first color) (second color) (third color) 255)
    (sdl2:render-fill-rect renderer outer)

    ;; 2. Blit the inset portion of the image on top
    (sdl2:render-copy renderer texture :source-rect source :dest-rect dest)))

(defun draw-grid (renderer texture grid num-x num-y)
  (dotimes (ix num-x)
    (dotimes (iy num-y)
      (draw-cell renderer texture
                 (grid-get grid ix iy)))))


;;; ── Main ──────────────────────────────────────────────────────────────

(defstruct game-state
  win renderer texture
  win-rect
  tile-w tile-h
  grid
  photo
  (done     nil)
  (hinting  nil)
  (selected nil)
  (page 0)
  (dirty t))

(defun load-new-image (gs)
  "Reset game-state info/texture with info from photo assoc."
  (when (game-state-texture gs)
    (sdl2:destroy-texture (game-state-texture gs)))
  ;; advance page by 1 to guarantee new images
  (let* ((page  (1+ (game-state-page gs)))
         (photo (get-new-image page))
         (url   (form-photo-url photo *image-size*)))
    (when *debug*
      (format t "get-new-image: ~a~%" (cdr (assoc :title photo))))
    (multiple-value-bind (texture img-w img-h)
        (fetch-puzzle-texture (game-state-renderer gs) url)
      (multiple-value-bind (win-w win-h)
          (fit-win-dimensions img-w img-h *max-size* *num-x* *num-y*)
        (let ((tile-w      (truncate win-w *num-x*))
              (tile-h      (truncate win-h *num-y*)))
          (sdl2:set-window-size (game-state-win gs) win-w win-h)
          ;; After set-window-size, explicitly clear any logical size override:
	  ;; (let ((renderer    (game-state-renderer gs)) ...
          ;; (sdl2-ffi.functions:sdl-render-set-logical-size renderer 0 0)
          ;; (sdl2:render-set-viewport renderer nil)      ; use full window
          (when *debug*
            (format t "load-new-image set-viewport: ~d x ~d~%" win-w win-h))
          (setf (game-state-photo    gs) photo
                (game-state-page     gs) page
                (game-state-texture  gs) texture
                (game-state-win-rect gs) (sdl2:make-rect 0 0 win-w win-h)
                (game-state-tile-w   gs) tile-w
                (game-state-tile-h   gs) tile-h
                (game-state-grid     gs)
                  (make-grid *num-x* *num-y* img-w img-h tile-w tile-h)
                (game-state-done     gs) nil
                (game-state-hinting  gs) nil
                (game-state-selected gs) nil
                (game-state-dirty    gs) t)
          (grid-randomize (game-state-grid gs) *num-x* *num-y*))))))

(defun update-window-title (gs)
  (let ((page    (game-state-page gs))
        (title   (cdr (assoc :title (game-state-photo gs)))))
    (sdl2:set-window-title
     (game-state-win gs)
     (format nil "[pg ~d]: ~A" page title))))


(defun handle-keydown (gs keysym)
  (let ((sc (sdl2:scancode-value keysym)))
    (cond
      ((or (sdl2:scancode= sc :scancode-escape)
           (sdl2:scancode= sc :scancode-q))
       (sdl2:push-event :quit))

      ((sdl2:scancode= sc :scancode-r)
       (load-new-image gs)
       (update-window-title gs))

      ((sdl2:scancode= sc :scancode-h)
       (setf (game-state-hinting gs) t
             (game-state-dirty gs) t)))))

(defun handle-keyup (gs keysym)
  (let ((sc (sdl2:scancode-value keysym)))
    (when (sdl2:scancode= sc :scancode-h)
      (setf (game-state-hinting gs) nil)
      (setf (game-state-dirty gs) t))))

(defun handle-mouse (gs x y button)
  (when (and (= button sdl2-ffi:+sdl-button-left+)
             (not (game-state-done gs)))
    (let* ((tile-w (game-state-tile-w gs))
           (tile-h (game-state-tile-h gs))
           (grid   (game-state-grid gs))
           (ix     (truncate x tile-w))
           (iy     (truncate y tile-h)))
      (if (game-state-selected gs)
          (let* ((selected-pos (game-state-selected gs))
                 (ox (car selected-pos))
                 (oy (cdr selected-pos)))
            (cell-toggle-selected (grid-get grid ox oy))
            (unless (equal (cons ix iy) selected-pos)
              (grid-swap grid ix iy ox oy)
              (when (grid-done-p grid *num-x* *num-y*)
                (setf (game-state-done gs) t)))
            (setf (game-state-selected gs) nil))
          (progn   ;[else] nothing prev selected
            (cell-toggle-selected (grid-get grid ix iy))
            (setf (game-state-selected gs) (cons ix iy))))
      (setf (game-state-dirty gs) t))))

(defun handle-initial-load (gs)
  "Trigger the first image load after a short delay to let the window settle."
  (unless (game-state-texture gs)
    (load-new-image gs)
    (update-window-title gs)
    (when *debug*
      (format t "handle-initial-load (tile w ~d h ~d)~%"
              (game-state-tile-w gs) (game-state-tile-h gs))) ))

(defun handle-idle (gs)
  ;; only redraw if updated
  (when (game-state-dirty gs)
    (when *debug*
      (format t "handle-idle (dirty ~a tile w ~d h ~d)~%"
              (game-state-dirty gs)
              (game-state-tile-w gs) (game-state-tile-h gs)))
    (let ((renderer (game-state-renderer gs))
          (texture  (game-state-texture  gs)))
      (sdl2:set-render-draw-color renderer 0 0 0 255)
      (sdl2:render-clear renderer)
      (when texture
        (if (or (game-state-done gs) (game-state-hinting gs))
            (sdl2:render-copy renderer texture
                              :dest-rect (game-state-win-rect gs))
            (draw-grid renderer texture
                       (game-state-grid gs)
                       *num-x* *num-y*)))
      (sdl2:render-present renderer)
      (if texture
          (setf (game-state-dirty gs) nil)
          ;; delayed startup until after a blank window drawn
          (handle-initial-load gs)))))


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
      ;;(handle-initial-load gs)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym) (handle-keydown gs keysym))
        (:keyup   (:keysym keysym) (handle-keyup   gs keysym))
        (:mousebuttondown (:x x :y y :button button)
                          (handle-mouse gs x y button))
        (:idle ()
               (handle-idle gs)
               (sdl2:delay *idle-delay*))))))

;; run the program
(main)
