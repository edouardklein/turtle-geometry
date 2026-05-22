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
