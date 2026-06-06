(in-package #:turtle-geometry)

(defsynonym (clear clr) (&key (line-drawer *line-drawer*))
  ;; reset entities
  ;; reset turtle
  ;; reset line-drawer
  ;; reset camera
  (clear-entities *world*)
  (let ((array (make-instance 'gl-dynamic-array :array-type :float)))
    (with-slots (num-vertices draw-array) line-drawer
      (setf *turtle* (make-turtle)
            *camera* (make-instance 'camera)
            num-vertices 0
            draw-array array))))

;; ---------------------------------------------------------------------------
;; Instant commands -- enqueued into the unified animation command queue
;; ---------------------------------------------------------------------------

(defsynonym (pen-toggle) (&key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :pen-toggle)))

(defsynonym (pen-down) (&key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :pen-down)))

(defsynonym (pen-up) (&key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :pen-up)))

(defsynonym (color) (color-vec &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :color :data color-vec)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow "SPEED")
  (export (find-symbol "SPEED" *package*)))

(defsynonym (speed) (speed-value &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :speed
                                    :data (normalize-turtle-speed speed-value))))

;; ---------------------------------------------------------------------------
;; Animated commands -- also go into the same queue
;; ---------------------------------------------------------------------------

(defsynonym (forward fw) (distance &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :move :amount distance)))

(defmacro def-turtle-synonym ((&rest synonyms)
                              (&rest args)
                              fn-call)
  (let ((fn-extra-args (append fn-call
                               '(:world world :turtle turtle))))
    `(defsynonym (,@synonyms) (,@args
                               &key
                               (world *world*)
                               (turtle *turtle*))
       ,fn-extra-args)))

(def-turtle-synonym (back bk kcab) (distance)
                    (fw (- distance)))

(defsynonym (turtle-rotate rot) (vec &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :rotate :amount vec)))

(def-turtle-synonym (left lt lf) (radians)
                    (turtle-rotate (vec3f 0.0 0.0 radians)))

(def-turtle-synonym (right rt) (radians)
                    (turtle-rotate (vec3f 0.0 0.0 (- radians))))

(def-turtle-synonym (roll) (radians)
                    (turtle-rotate (vec3f radians 0.0 0.0)))

(def-turtle-synonym (pitch) (radians)
                    (turtle-rotate (vec3f 0.0 radians 0.0)))

(def-turtle-synonym (yaw) (radians)
                    (turtle-rotate (vec3f 0.0 0.0 radians)))

;; ---------------------------------------------------------------------------
;; Physics commands -- instant, unified queue
;; ---------------------------------------------------------------------------

(defsynonym (velocity vel sp v) (velocity &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :velocity :data velocity)))

(defsynonym (force f) (force &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :force :data force)))

(defsynonym (add-force af) (force &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :add-force :data force)))

(defsynonym (add-mass) (mass &key (world *world*) (turtle *turtle*))
  (enqueue-turtle-animation
   world turtle
   (make-turtle-animation-command :kind :add-mass :data mass)))

;; ---------------------------------------------------------------------------
;; SVG export
;; ---------------------------------------------------------------------------

(defun export-svg (filepath &key (world *world*) (turtle *turtle*))
  "Export the turtle's logical drawing path to an SVG file.
The SVG contains only the user-given path (no animation intermediates)."
  (let* ((turt (ec world turtle 'turtle-component))
         (subpaths (reverse (turtle-component-svg-subpaths turt)))
         (current (reverse (turtle-component-svg-current-subpath turt)))
         (all-subpaths (if current (append subpaths (list current)) subpaths)))
    (with-open-file (stream filepath :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
      (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" ")
      ;; Compute viewBox
      (let ((min-x most-positive-single-float)
            (max-x most-negative-single-float)
            (min-y most-positive-single-float)
            (max-y most-negative-single-float))
        (dolist (subpath all-subpaths)
          (dolist (pt subpath)
            (let ((x (x-val pt))
                  (y (y-val pt)))
              (setf min-x (min min-x x)
                    max-x (max max-x x)
                    min-y (min min-y y)
                    max-y (max max-y y)))))
        (when (or (= min-x most-positive-single-float)
                  (null all-subpaths))
          (setf min-x 0 max-x 100 min-y 0 max-y 100))
        (let* ((padding 10.0)
               (vb-x (- min-x padding))
               (vb-y (- (- max-y) padding))
               (vb-w (+ (- max-x min-x) (* 2 padding)))
               (vb-h (+ (- max-y min-y) (* 2 padding))))
          (format stream "viewBox=\"~,3f ~,3f ~,3f ~,3f\" " vb-x vb-y vb-w vb-h)
          (format stream "width=\"100%\" height=\"100%\">~%")))
      ;; Draw subpaths
      (dolist (subpath all-subpaths)
        (when (cdr subpath) ;; at least 2 points
          (format stream "  <path d=\"M ~,3f ~,3f "
                  (x-val (first subpath))
                  (- (y-val (first subpath))))
          (dolist (pt (rest subpath))
            (format stream "L ~,3f ~,3f "
                    (x-val pt)
                    (- (y-val pt))))
          (format stream "\" fill=\"none\" stroke=\"black\" stroke-width=\"1\" />~%")))
      (format stream "</svg>~%"))
    filepath))
