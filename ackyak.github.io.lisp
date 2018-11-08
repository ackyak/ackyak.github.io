;;;; ackyak.github.io.lisp

(in-package #:ackyak.github.io)

(defpsmacro line (context x1 y1 x2 y2 color)
  `(progn
     ((@ ,context begin-path))
     ((@ ,context move-to) ,x1 ,y1)
     ((@ ,context line-to) ,x2 ,y2)
     (setf (@ ,context stroke-style) ,color)
     ((@ ,context stroke))))

(defpsmacro point (context x y radius color)
  `(progn
     ((@ ,context begin-path))
     ((@ ,context arc) ,x ,y ,radius 0 (* 2 pi))
     (setf (@ ,context fill-style) ,color)
     ((@ ,context fill))))

(defpsmacro sig (x)
  `(/ (1+ (exp (- ,x)))))

(defpsmacro clamp (l m h)
  `(if (< ,l ,m)
       (min ,m ,h)
       (max ,l ,m)))

(defun render-script (stream)
  (let ((*js-string-delimiter*  #\'))
    (ps-to-stream stream
      (defvar canvas  ((@ document get-element-by-id) "virtual-paint-stirring-machine"))
      (defvar context ((@ canvas get-context) "2d"))
      (defvar date    (new (|Date|)))
      (defvar time    ((@ date get-seconds)))
      (defvar boost   0)
      (defvar width   (@ window inner-width))
      (defvar height  (@ window inner-height))
      (defvar start-x (/ width 2))
      (defvar start-y (/ height 2))
      (defvar cursor-x)
      (defvar cursor-y)
      (defvar direction 1)
      (defvar color-a "#FFF")
      (defvar color-b "#000")

      (defun cursor-click (event)
        (incf boost 0.003)
        (setf direction (- direction)
              cursor-x (@ event client-x)
              cursor-y (@ event client-y))
        (psetf color-a color-b
               color-b color-a)
        (setf (chain canvas style background-color) color-a))

      (labels
          ((draw-tree (time g r x1 y1 x2 y2)
             (unless (< r 1)
               (when (< r 5)
                 (line  context x1 y1 x2 y2 color-b)
                 (point context x1 y1 (/ r) color-a))
               ((@ (list (* (sin (/ time 1.2)) (tan (cos (/ time 1.3))))
                         (* (cos (/ time 1.5)) (sig (sin (/ time 1.7)))))
                   map)
                (lambda (x)
                  (draw-tree
                   time
                   (+ g x)
                   (/ r 1.6)
                   (/ (+ (+ x1 (* 40 (cos (* 40 (sig (cos g))))))
                         (- x1 (* 20 (cos time)) (* 20 (sin g))))
                      2)
                   (/ (+ (+ y1 (* 40 (cos (* 40 (sig (sin g))))))
                         (- y1 (* 20 (sin time)) (* 20 (cos g))))
                      2)
                   x1
                   y1)))))

           (recur (timeout)
             (set-timeout (lambda ()
                            (incf time (* direction (+ (setf boost (/ boost 1.01))
                                                       (/ (- 1000 time)))))
                            (draw-tree time 0 100 start-x start-y)
                            (recur timeout))
                          timeout)))
        
        (setf (@ canvas width) width
              (@ canvas height) height
              cursor-x start-x
              cursor-y start-y)
        (recur (* 100 (/ 60)))))))

(defun render-page (stream)
  (setf (html-mode) :html5
        *attribute-quote-char* #\")
  (with-html-output (_ stream :prologue t :indent t)
    (:html :lang "en"
           (:head (:meta :charset "utf-8")
                  (:title "Ackyak")
                  (:style "html, body {margin: 0px;}"))
           (:body (:canvas :id "virtual-paint-stirring-machine"
                           :onclick (ps (cursor-click event)))
                  (:script :src "virtual-paint-stirring-machine.js")))))

(defun render-all ()
  (setf (html-mode) :html5
        *attribute-quote-char* #\"
        *js-string-delimiter*  #\')
  (with-open-file (file (merge-pathnames #P"index.html" (asdf:system-source-file :ackyak.github.io))
                        :direction :output :if-exists :supersede :external-format uiop:*utf-8-external-format*)
    (render-page file))
  (with-open-file (file (merge-pathnames #P"virtual-paint-stirring-machine.js" (asdf:system-source-file :ackyak.github.io))
                        :direction :output :if-exists :supersede :external-format uiop:*utf-8-external-format*)
    (render-script file)))

#+(or)
(render-all)
