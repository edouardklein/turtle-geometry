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

(defun send-message (message &key (world *world*)
                               (turtle *turtle*))
  (if (and (is-entity-p world turtle)
           (has-component-p world turtle 'turtle-message-component))
      (let ((component (ec world turtle 'turtle-message-component)))
        (bt:with-lock-held ((turtle-message-component-message-lock component))
          (vector-push-extend message
                              (turtle-message-component-message-list component))))
      (warn "Entity ~A doesn't have a message component. Entity's existence is ~A.~%"
            turtle
            (is-entity-p world turtle))))

(defmacro defmessage ((&rest names) (&rest message-args)
                      &body message-body)
  (let ((message-names (iter (for name in names)
                         (collect
                             (alexandria:symbolicate
                              name "-MESSAGE")))))
    `(progn
       (defsynonym (,@message-names) (,@message-args)
         ,@message-body)
       (defsynonym (,@names) (,@message-args &key (world *world*)
                                             (turtle *turtle*))
         (send-message (,(car message-names) ,@message-args)
                       :world world
                       :turtle turtle)))))

(defmessage (pen-toggle) ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p (not pen-down-p)))))

(defmessage (pen-down) ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p t))))

(defmessage (pen-up) ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p nil))))

(defmessage (color) (color-vec)
  (lambda (w id)
    (with-slots (color) (ec w id 'turtle-component)
      (setf color color-vec))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow "SPEED")
  (export (find-symbol "SPEED" *package*)))

(defmessage (speed) (speed-value)
  (lambda (w id)
    (let ((animation (ec w id 'turtle-animation-component)))
      (bt:with-lock-held ((turtle-animation-component-command-lock animation))
        (setf (turtle-animation-component-speed animation)
              (normalize-turtle-speed speed-value))))))

(defmessage (forward fw) (distance)
  (lambda (w id)
    (enqueue-turtle-animation
     w id
     (make-turtle-animation-command :kind :move
                                    :amount distance))))

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

(defmessage (turtle-rotate rot) (vec)
  (lambda (w id)
    (enqueue-turtle-animation
     w id
     (make-turtle-animation-command :kind :rotate
                                    :amount vec))))

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

(defmessage (velocity vel sp v) (velocity)
  (lambda (w id)
    (with-slots ((tvel velocity)) (ec w id 'newtonian-component)
      (setf tvel velocity))))

(defmessage (force f) (force)
  (lambda (w id)
    (with-slots ((tforce force)) (ec w id 'newtonian-component)
      (setf tforce force))))

(defmessage (add-force af) (force)
  (lambda (w id)
    (with-slots ((tforce force)) (ec w id 'newtonian-component)
      (incf tforce force))))

(defmessage (add-mass) (mass)
  (lambda (w id)
    (with-slots ((tmass mass)) (ec w id 'newtonian-component)
      (incf tmass mass))))
