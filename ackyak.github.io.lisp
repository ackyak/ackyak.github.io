;;;; ackyak.github.io.lisp

(in-package #:ackyak.github.io)

(defpsmacro line (ctx x1 y1 x2 y2)
  `(progn
     ((@ ,ctx begin-path))
     ((@ ,ctx move-to) ,x1 ,y1)
     ((@ ,ctx line-to) ,x2 ,y2)
     ((@ ,ctx stroke))))

(defpsmacro point (ctx x y radius color)
  `(progn
     ((@ ,ctx arc) ,x ,y ,radius 0 (* 2 pi))
     (setf (@ ,ctx fill-style) ,color)
     ((@ ,ctx fill))))

(defpsmacro sig (x) `(/ (1+ (exp (- ,x)))))

(defun render-script (stream)
  (let ((*js-string-delimiter*  #\'))
    (ps-to-stream stream
      (let* ((canvas ((@ document get-element-by-id) "coral-thing"))
             (ctx ((@ canvas get-context) "2d"))
             (width-pixels (@ window inner-width))
             (height-pixels (@ window inner-height)))
        (defun tree (time g r x1 y1 x2 y2)
          (unless (< r 1)
            (line  ctx x1 y1 x2 y2)
            (point ctx x1 y1 (/ r) "#FFF")
            (psmap (lambda (x)
                     (tree time
                           (+ g x)
                           (/ r 1.6)
                           (/ (+ (+ x1 (* 40 (cos (* 40 (sig (cos g))))))
                                 (- x1 (* 20 (cos time)) (* 20 (sin g))))
                              2)
                           (/ (+ (+ y1 (* 40 (cos (* 40 (sig (sin g))))))
                                 (- y1 (* 20 (sin time)) (* 20 (cos g))))
                              2)
                           x1
                           y1))
                   (list (* (sin (/ time 1.2)) (tan (cos (/ time 1.3))))
                         (* (cos (/ time 1.5)) (sig (sin (/ time 1.7))))))))
        (setf (@ canvas width) width-pixels
              (@ canvas height) height-pixels)
        (let* ((date (new (|Date|)))
               (time ((@ date |getSeconds|)))
               (ox (/ width-pixels 2))
               (oy (/ height-pixels 2)))
          (labels ((recur (i)
                     (set-timeout (lambda ()
                                    (incf time (/ (- 1000 time)))
                                    (tree time 0 100 ox oy)
                                    (recur i))
                                  i)))
            (recur (* 100 (/ 60)))))))))

(defun render-page (stream)
  (setf (html-mode) :html5
        *attribute-quote-char* #\")
  (with-html-output (_ stream :prologue t :indent t)
    (:html :lang "en"
           (:head (:meta :charset "utf-8")
                  (:title "Ackyak"))
           (:body (:p "Still in the works.")
                  (:canvas :id "coral-thing")
                  (:script :src "coral-carver.js")))))

(defun render-all ()
  (setf (html-mode) :html5
        *attribute-quote-char* #\"
        *js-string-delimiter*  #\')
  (with-open-file (file (merge-pathnames #P"index.html" (asdf:system-source-file :ackyak.github.io))
                        :direction :output :if-exists :supersede)
    (render-page file))
  (with-open-file (file (merge-pathnames #P"coral-carver.js" (asdf:system-source-file :ackyak.github.io))
                        :direction :output :if-exists :supersede)
    (render-script file)))

#+(or)
(render-all)
