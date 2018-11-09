;;;; ackyak.github.io.lisp

(in-package #:ackyak.github.io)

(defun render-script (stream)
  (let ((*js-string-delimiter*  #\'))
    (ps-to-stream stream
      
      (defvar *date* (new (|Date|))
        "A JS Date object. Only used to set `*TIME*' below.")
      
      (defvar *time*
        (chain *date* (get-seconds))
        "The initial value for `*TIME*'.")
      
      (defvar *boost* 0
        "`*BOOST*' is added to `*TIME*'. When `*BOOST*' is nonzero it
        will affect `*TIME*'.")
      
      (defvar *direction* 1
        "The scalar for `*TIME*', i.e. `*TIME*' goes forwards when
         this is positive and backwards when it's negative.")
      
      (defvar *color-a* "#FFF"
        "Initially this will be the colour for the nodes (Later it
         will be swapped with `*COLOR-B*' when the `*CANVAS*' is
         clicked.")
      
      (defvar *color-b* "#000"
        "Initially this will be the colour for the lines (Later it
         will be swapped with `*COLOR-A*' when `*CANVAS*' is clicked.")
      
      (defvar *canvas*
        (chain document (get-element-by-id "virtual-paint-stirring-machine"))
        "This `*CANVAS*' is drawn to by the functions `POINT' and `LINE'.")
      
      (defvar *context* (chain *canvas* (get-context "2d"))
        "A two dimensional context for `*CANVAS*'.")
      
      (defvar *width* (@ window inner-width)
        "The width of the `CANVAS'.")
      
      (defvar *height* (@ window inner-height)
        "The height of the `CANVAS'.")

      (defvar *framerate* (* 100 (/ 60))
        "The timeout for the recursive drawing loop.")

      ;; Make the canvas proportionate to the screen.
      (setf (@ *canvas* width) *width*
            (@ *canvas* height) *height*)
      
      (defun clamp (lower n upper)
        "Return the N unless it is less than LOWER or greater than UPPER."
        (if (< lower n)
            (min n upper)
            lower))
      
      (defun sig (n)
        "The sigmoid of N."
        (/ (1+ (exp (- n)))))

      (defun draw-line (context x1 y1 x2 y2 color)
        "Draw a line from the point (X1, Y1) to (X2, Y2)."
        ((@ context begin-path))
        ((@ context move-to) x1 y1)
        ((@ context line-to) x2 y2)
        (setf (@ context stroke-style) color)
        ((@ context stroke))
        undefined)

      (defun draw-dot (context x y radius color)
        "Draw a dot at the point (X, Y)."
        (chain context (begin-path))
        (chain context (arc x y radius 0 (* 2 pi)))
        (setf (@ context fill-style) color)
        (chain context (fill))
        undefined)
      
      (defun invert-things (event)
        "Invert the background and current drawing colours, and
reverse the flow of `*TIME*'."
        (incf *boost* 0.003)
        (setf *direction* (- *direction*))
        (psetf *color-a* *color-b*
               *color-b* *color-a*)
        (setf (chain *canvas* style background-color) *color-a*)
        undefined)

      (defun draw-tree (time offset depth current-x current-y prev-x prev-y)
        "Draw a tree rooted at the point (X1, Y1) and shaped based on
TIME. n.b. the magic numbers are all chosen based on what looked good."
        (unless (< depth 1)
          ;; Don't draw anything until part of the way down the tree.
          (when (< depth 5)
            (draw-line *context* current-x current-y prev-x prev-y *color-b*)
            (draw-dot *context* current-x current-y (/ depth) *color-a*))
          ;; Branch once for each element of this list.
          (chain (list (* (sin (/ time 1.2)) (tan (cos (/ time 1.3))))
                       (* (cos (/ time 1.5)) (sig (sin (/ time 1.7)))))
                 (map (lambda (x)
                        (draw-tree time
                                   (+ offset x)
                                   (/ depth 1.6)
                                   (* 0.5 (+ (+ current-x (* 40 (cos (* 40 (sig (cos offset))))))
                                             (- current-x
                                                (* 20 (cos time))
                                                (* 20 (sin offset)))))
                                   (* 0.5 (+ (+ current-y (* 40 (cos (* 40 (sig (sin offset))))))
                                             (- current-y
                                                (* 20 (sin time))
                                                (* 20 (cos offset)))))
                                   current-x
                                   current-y))))))

      (defun drawing-loop (timeout)
        "Draw a tree with DRAW-TREE then recur after TIMEOUT (milliseconds)."
        (set-timeout (lambda ()
                       (incf *time* (* *direction* (+ (setf *boost* (/ *boost* 1.01))
                                                      (/ (- 1000 *time*)))))
                       (draw-tree *time* 0 100 (/ *width* 2) (/ *height* 2))
                       (drawing-loop timeout))
                     timeout))

      (drawing-loop *framerate*))))

(defun render-page (stream)
  (with-html-output (_ stream :prologue t :indent t)
    (:html :lang "en"
           (:head (:meta :charset "utf-8")
                  (:title "Ackyak")
                  (:style "html, body {margin: 0px;}"))
           (:body (:canvas :id "virtual-paint-stirring-machine"
                           :onclick (ps (invert-things event)))
                  (:script :src "virtual-paint-stirring-machine.js")))))

(defvar *root*
  (asdf:system-source-file :ackyak.github.io))

(defun render-all ()
  (with-open-file (file (merge-pathnames #P"index.html" *root*)
                        :direction :output
                        :if-exists :supersede
                        :external-format uiop:*utf-8-external-format*)
    (render-page file))
  (with-open-file (file (merge-pathnames #P"virtual-paint-stirring-machine.js" *root*)
                        :direction :output
                        :if-exists :supersede
                        :external-format uiop:*utf-8-external-format*)
    (render-script file)))

#+(or)
(render-all)
