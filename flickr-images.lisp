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
;;(defparameter *font-file* "/usr/share/fonts/gnu-free/FreeSans.ttf")
(defparameter *font-file*
  "/usr/share/fonts/urw-base35/NimbusSansNarrow-Regular.otf")
(defparameter *font-size* 18)
;;(defparameter *keyword* "tiger")
;;(defparameter *keyword* "cat")
(defparameter *keyword* "kitty")
(defparameter *image-size* "m")   ; t = 100, m = 240, q = 150 sq, b = 1024
(defparameter *cell-size* 350)
(defparameter *cell-pad* 40)
(defparameter *idle-delay* 500)		; pause in ms


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

(defun flickr-get-interesting (&optional (num-urls 10))
  "Return a random photo URL from Flickr interestingness."
  (let* ((result (flickr-call "flickr.interestingness.getList"
                              "per_page" (write-to-string num-urls)))
         (photos (cdr (assoc :photo
			     (cdr (assoc :photos result))))))
    photos))

(defun flickr-get-by-keyword (keyword &optional (num-urls 6) (page 1))
  "Return a list of photo URLs matching keyword, using url_t directly."
  (let* ((result (flickr-call "flickr.photos.search"
                              "tags"     keyword
			      "sort"	 "relevance"
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

(defun render-text (renderer font text x y)
  (unless (zerop (length text))
    (let* ((surface (sdl2-ttf:render-utf8-solid font text 255 255 255 255))
	   (w (sdl2:surface-width surface))
	   (h (sdl2:surface-height surface))
	   (texture (sdl2:create-texture-from-surface renderer surface)))
      ;; XXX: should need this free, but get error here
      ;;(sdl2:free-surface surface)
      (sdl2:render-copy renderer texture
			:dest-rect (sdl2:make-rect x y w h)))))


;;; ── Main ──────────────────────────────────────────────────────────────

(defstruct game-state
  win
  renderer
  font
  textures
  photos
  page)

(defun load-new-images (gs)
  (when (game-state-textures gs)
    (mapcar (lambda (x)
	      (let ((tex (car x)))
		(sdl2:destroy-texture tex)))
	    (game-state-textures gs)))
  (unless (game-state-page gs)
    (setf (game-state-page gs) 1))
  (let* ((renderer (game-state-renderer gs))
	 (num-urls (* *num-rows* *num-cols*))
	 (page (game-state-page gs))
	 (photos (flickr-get-by-keyword *keyword* num-urls page)))
    (setf (game-state-photos gs) photos)
    ;; should have a list of triples (texture w h), 1 for each item in photos
    (setf (game-state-textures gs)
	  (mapcar (lambda (photo)
		    (multiple-value-bind (tex w h)
			(fetch-puzzle-texture renderer photo)
		      (list tex w h)))
		  photos))))


(defun handle-keydown (gs keysym)
  (let ((sc (sdl2:scancode-value keysym)))
    (cond
      ((or (sdl2:scancode= sc :scancode-escape)
           (sdl2:scancode= sc :scancode-q))
       (sdl2:push-event :quit))

      ((sdl2:scancode= sc :scancode-r)
       (setf (game-state-page gs) 1)
       (load-new-images gs))

      ((sdl2:scancode= sc :scancode-n)
       ;; next N images
       (setf (game-state-page gs) (+ (game-state-page gs) 1))
       (load-new-images gs))

      ((sdl2:scancode= sc :scancode-p)
       ;; prev N images
       (setf (game-state-page gs) (max 1 (- (game-state-page gs) 1)))
       (load-new-images gs)))))


(defun render-cell (renderer font ix iy tex-item photo)
  (let* ((dest-x (+ (* ix *cell-size*) *cell-pad*))
	 (dest-y (+ (* iy *cell-size*) *cell-pad*))
	 (title  (cdr (assoc :title photo)))
	 (text-x dest-x)
	 (text-y (- dest-y 22))
	 (texture (first tex-item))
	 (img-w  (second tex-item))
	 (img-h  (third tex-item)))
    (sdl2:render-copy
     renderer texture
     :dest-rect (sdl2:make-rect dest-x dest-y img-w img-h))
    ;;(format t "~a~%" title)
    (render-text renderer font title text-x text-y)))


(defun handle-idle (gs)
  (let ((renderer (game-state-renderer gs))
	(font     (game-state-font gs))
        (textures (game-state-textures gs))
	(photos   (game-state-photos gs)))
    ;; maybe should have a flag for update-needed??
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)
    ;; helper list to loop over *num-rows* *num-cols* 
    (let ((cells (loop for ix from 0 below *num-cols*
		       nconcing (loop for iy from 0 below *num-rows*
				      collect (cons ix iy)))))
      ;; now iterate over these lists in parallel
      (loop for cell in cells
	    for tex-item in textures
	    for pic in photos
	    do (render-cell renderer font (car cell) (cdr cell) tex-item pic)))
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
	(load-new-images gs)
	(sdl2:with-event-loop (:method :poll)
          (:quit () t)
          (:keydown (:keysym keysym) (handle-keydown gs keysym))
          (:idle () (handle-idle gs)
		    (sdl2:delay *idle-delay*)))))))

;; run the program
(main)
