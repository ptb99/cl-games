(ql:quickload '(:sdl2 :sdl2-image :sdl2-ttf :dexador :quri :cl-json) 
              :silent t)

(defpackage :flickr-images
  (:use :cl)
  (:export :main))

(in-package :flickr-images)


;;; ── Constants ─────────────────────────────────────────────────────────
(defparameter *max-size* 800)
(defparameter *num-rows* 2)
(defparameter *num-cols* 3)
(defparameter *cell-size* 350)
(defparameter *cell-pad* 40)
(defparameter *idle-delay* 500)         ; pause in ms

(defparameter *font-file*
  "/usr/share/fonts/urw-base35/NimbusSansNarrow-Regular.otf")
(defparameter *font-size* 18)

;;(defparameter *keyword* "tiger")
;;(defparameter *keyword* "cat")
(defparameter *keyword* "kitty")
(defparameter *image-size* "m")   ; t = 100, m = 240, q = 150 sq, b = 1024

(defparameter *debug* nil)


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
  ;; json pkg maps "api_key" -> :API--KEY
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

(defun form-photo-url (photo  &optional (size *image-size*))
  "Construct a Flickr image URL from a photo alist."
  (format nil "https://farm~A.staticflickr.com/~A/~A_~A_~A.jpg"
          (cdr (assoc :farm   photo))
          (cdr (assoc :server photo))
          (cdr (assoc :id     photo))
          (cdr (assoc :secret photo))
          size))

(defun get-random-url (photos &optional (size *image-size*))
  "Choose a random pic from list of returned photo prop-lists"
  (form-photo-url (nth (random (length photos)) photos) size))

;; Use like this:
;; (get-random-url (flickr-get-interesting 6) "t")

(defun flickr-get-interesting (&optional (num-urls 10) (page 1))
  "Return a random photo URL from Flickr interestingness."
  (let* ((result (flickr-call "flickr.interestingness.getList"
                              "per_page" (write-to-string num-urls)
                              "page"     (write-to-string page)))
         (photos (cdr (assoc :photo
                             (cdr (assoc :photos result))))))
    photos))

(defun flickr-get-by-keyword (keyword &optional (num-urls 6) (page 1))
  "Return a list of photo URLs matching keyword, using url_t directly."
  (let* ((result (flickr-call "flickr.photos.search"
                              "tags"     keyword
                              "sort"     "relevance"
                              "content_types" "0" ; photos only, no screens/art
                              "per_page" (write-to-string num-urls)
                              "page"     (write-to-string page)))
         (photos (cdr (assoc :photo
                             (cdr (assoc :photos result))))))
    photos))
    ;; ;; cl-json: "url_t" -> :URL--t
    ;; (mapcar (lambda (x) (cdr (assoc :url--t x))) photos)))

(defun flickr-debug-search (keyword &optional (num-urls 10))
  "Show all photo titles and URLs returned for a keyword search."
  (let ((photos (flickr-get-by-keyword keyword num-urls)))
    (format t "~&Found ~A photos for keyword: ~S~%" (length photos) keyword)
    (loop for p in photos
          for i from 1
          do (format t "~& ~2D. ~A~%     ~A~%"
                     i
                     (cdr (assoc :title p))
                     (form-photo-url p )))
    (length photos)))


;;; ── Image loading ─────────────────────────────────────────────────────

(defun load-image-from-url (url)
  ;; make a flag to use a static image (images/cat.bmp)
  (let ((tmp "/tmp/puzzle-img.jpg"))
    (dex:fetch url tmp :if-exists :supersede) ; download directly to file
    (sdl2-image:load-image tmp)))

(defun fetch-puzzle-texture (renderer photo)
  "Download a Flickr image, load it into an SDL2 texture, return (texture w h)."
  (let* ((url     (form-photo-url photo))
         (surface (load-image-from-url url))
         (w       (sdl2:surface-width surface))
         (h       (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (sdl2:free-surface surface)
    ;; DEBUGGING:
    ;;(format t "FETCH ~a - ~a x ~a~%" url w h)
    (values texture w h)))


;;; ── Fonts/Text ────────────────────────────────────────────────────────

(defun fetch-text-texture (renderer font title)
  ;; Protect against title having a zero length
  (let* ((text    (if (zerop (length title)) " " title))
         (surface (sdl2-ttf:render-utf8-solid font text 255 255 255 255))
         (w       (sdl2:surface-width surface))
         (h       (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    ;; Do NOT free surface -- sdl2-ttf registers a finalizer on it
    ;; Just destroy the texture we created
    ;;(sdl2:free-surface surface)

    ;; XXX: Perhaps we should protect against w being > *cell-size* ??
    (values texture w h)))


;;; ── Cell Structure ────────────────────────────────────────────────────

(defstruct cell
  ix iy
  img-texture
  img-dest-rect
  text-texture
  text-dest-rect
  photo)

(defun cell-set-image (c renderer font photo)
  (let ((ix (cell-ix c))
        (iy (cell-iy c))
        (title (cdr (assoc :title photo))))
    (setf (cell-photo c) photo)
    (let ((dest-x (+ (* ix *cell-size*) *cell-pad*))
          (dest-y (+ (* iy *cell-size*) *cell-pad*)))
      (when (cell-img-texture c)
        (sdl2:destroy-texture (cell-img-texture c)))
      (multiple-value-bind (tex w h)
          (fetch-puzzle-texture renderer photo)
        (setf (cell-img-texture c) tex)
        (setf (cell-img-dest-rect c)
              (sdl2:make-rect dest-x dest-y w h)))
      (when (cell-text-texture c)
        (sdl2:destroy-texture (cell-text-texture c)))
      (multiple-value-bind (tex w h)
          (fetch-text-texture renderer font title)
        (let ((text-x dest-x)
              (text-y (- dest-y 22)))
          (setf (cell-text-texture c) tex)
          (setf (cell-text-dest-rect c)
                (sdl2:make-rect text-x text-y w h)))))))

(defun render-cell (c renderer)
  (when *debug*
    (let((ix (cell-ix c))
         (iy (cell-iy c))
         (img-rect  (cell-img-dest-rect c))
         (text-rect (cell-text-dest-rect c)))
      (format t "[~d,~d] -> (~d ~d) + (~d ~d)~%"
              ix iy
              (sdl2:rect-x img-rect) (sdl2:rect-y img-rect)
              (sdl2:rect-x text-rect) (sdl2:rect-y text-rect))))
  (sdl2:render-copy renderer (cell-img-texture c)
                    :dest-rect (cell-img-dest-rect c))
  (sdl2:render-copy renderer (cell-text-texture c)
                    :dest-rect (cell-text-dest-rect c)))


;;; ── Main ──────────────────────────────────────────────────────────────

(defstruct game-state
  win
  renderer
  font
  cell-list
  (page 1))

;; initialize the cell-list hash table
(defun make-cell-list (num-x num-y)
  (loop for ix from 0 below num-x
        nconcing (loop for iy from 0 below num-y
                       collect (make-cell :ix ix :iy iy))))

;; given a list of N photos, update all cells in cell-list
(defun load-new-images (gs photos)
  (let* ((cell-list (game-state-cell-list gs))
         (renderer (game-state-renderer gs))
         (font     (game-state-font gs)))
    (loop for photo in photos
          for c in cell-list
          do (cell-set-image c renderer font photo))))

(defun get-new-photos (page)
  (let ((num-urls (* *num-rows* *num-cols*)))
    ;; alternates:
    ;;(flickr-get-interesting num-urls page)
    (flickr-get-by-keyword *keyword* num-urls page)))


(defun handle-keydown (gs keysym)
  (let ((sc (sdl2:scancode-value keysym)))
    (cond
      ((or (sdl2:scancode= sc :scancode-escape)
           (sdl2:scancode= sc :scancode-q))
       (sdl2:push-event :quit))

      ((sdl2:scancode= sc :scancode-r)
       (setf (game-state-page gs) 1)
       (let ((photos (get-new-photos 1)))
         (load-new-images gs photos)))

      ((sdl2:scancode= sc :scancode-n)
       ;; next N images
       (let* ((page (+ (game-state-page gs) 1))
              (photos (get-new-photos page)))
         (setf (game-state-page gs) page)
         (load-new-images gs photos)))

      ((sdl2:scancode= sc :scancode-p)
       ;; prev N images
       (let* ((page (- (game-state-page gs) 1))
              (photos (get-new-photos page)))
         (setf (game-state-page gs) page)
         (load-new-images gs photos))))))


(defun handle-idle (gs)
  (let ((renderer  (game-state-renderer gs))
        (cell-list (game-state-cell-list gs)))
    ;; maybe should have a flag for update-needed??
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)
    ;; now iterate over these lists in parallel
    (loop for cell in cell-list
          do (render-cell cell renderer))
    (sdl2:render-present renderer)))


;; SDL boilerplate
(defmacro with-sdl2-game ((win renderer title w h) &body body)
  `(sdl2:with-init (:video)
     (sdl2-image:init '(:jpg :png))
     (sdl2-ttf:init)
     (unwind-protect
         (sdl2:with-window (,win :title ,title :w ,w :h ,h :flags '(:shown))
           (sdl2:with-renderer (,renderer ,win :flags '(:accelerated))
             ,@body))
       (sdl2-image:quit)
       (sdl2-ttf:quit))))


(defun main ()
  (let ((window-w (* *num-cols* *cell-size*))
        (window-h (* *num-rows* *cell-size*)))
    (with-sdl2-game (win renderer "Image Puzzle" window-w window-h)
      (let* ((font (sdl2-ttf:open-font *font-file* *font-size*))
             (gs (make-game-state :win win :renderer renderer :font font)))
        (unwind-protect
             (progn
               (setf (game-state-cell-list gs)
                     (make-cell-list *num-cols* *num-rows*))
               (load-new-images gs (get-new-photos 1))
               (sdl2:with-event-loop (:method :poll)
                 (:quit () t)
                 (:keydown (:keysym keysym) (handle-keydown gs keysym))
                 (:idle () (handle-idle gs)
                        (sdl2:delay *idle-delay*))))
          (sdl2-ttf:close-font font))))))

;; run the program
(main)
