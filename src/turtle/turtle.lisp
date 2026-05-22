(in-package #:turtle-geometry)

(defstruct orientation-component
  (position (vec3f 0.0) :type vec3f)
  (rotation (vec3f 0.0) :type vec3f))
(defstruct turtle-component
  (color (vec4f 0.0) :type vec4f)
  (pen-down-p t :type boolean))

;; keeps track of velocity and force in the forward direction
(defstruct newtonian-component
  (velocity 0.0 :type real)
  (mass 0.0 :type real)
  (force 0.0 :type real))

;; (defenum:defenum *turtle-message-types*
;;     ((+turtle-move+ :turtle-move)
;;      (+turtle-physics+ :turtle-physics)))

;; (defstruct turtle-message
;;   (type +turtle-move+ :type keyword)
;;   (fn (lambda ()) :type function))

;; a message is a function which takes parameters WORLD and ENTITY-ID
(defstruct turtle-message-component
  (message-list (make-array 8 :fill-pointer 0))
  (message-lock (bt:make-lock "turtle-message-lock")))

(defstruct turtle-animation-command
  kind
  amount
  target
  direction
  remaining
  speed)

(defstruct turtle-animation-component
  (speed 6 :type integer)
  active-command
  (command-list (make-array 8 :fill-pointer 0))
  (command-lock (bt:make-lock "turtle-animation-lock")))

(defsystem turtle-message-system (turtle-message-component))

(defmethod update-system ((world world)
                          (system turtle-message-system)
                          dt)
  (system-do-with-components ((tmc turtle-message-component))
      world system entity-id
    (let ((messages nil))
      (with-slots (message-list message-lock) tmc
        (bt:with-lock-held (message-lock)
          (when (plusp (length message-list))
            (setf messages (copy-seq message-list)
                  (fill-pointer message-list) 0))))
      (when messages
        (run-thread
          (iter (for message in-vector messages)
            (if (functionp message)
                (funcall message world entity-id)
                (warn "Sent entity ~a an invalid message type ~a~%"
                      entity-id (type-of message)))))))))

(defun normalize-turtle-speed (speed)
  (let ((value (cond ((integerp speed) speed)
                     ((realp speed) (round speed))
                     ((or (eq speed :fastest)
                          (and (stringp speed)
                               (string-equal speed "fastest")))
                      0)
                     ((or (eq speed :slowest)
                          (and (stringp speed)
                               (string-equal speed "slowest")))
                      1)
                     ((or (eq speed :slow)
                          (and (stringp speed)
                               (string-equal speed "slow")))
                      3)
                     ((or (eq speed :normal)
                          (and (stringp speed)
                               (string-equal speed "normal")))
                      6)
                     ((or (eq speed :fast)
                          (and (stringp speed)
                               (string-equal speed "fast")))
                      10)
                     (t
                      (error "Unknown turtle speed: ~S" speed)))))
    (max 0 (min 10 value))))

(defun turtle-linear-speed (speed)
  (* 25.0 speed))

(defun turtle-angular-speed (speed)
  (* 1.0 speed))

(defun turtle-forward-position (position rotation distance)
  (vec3f (kit.glm:matrix*vec3
          (vec3f 0.0 distance 0.0)
          (kit.glm:matrix*
           (kit.glm:translate position)
           (kit.glm:rotate rotation)))))

(defun enqueue-turtle-animation (world entity-id command)
  (let ((component (ec world entity-id 'turtle-animation-component)))
    (bt:with-lock-held ((turtle-animation-component-command-lock component))
      (unless (turtle-animation-command-speed command)
        (setf (turtle-animation-command-speed command)
              (turtle-animation-component-speed component)))
      (vector-push-extend command
                          (turtle-animation-component-command-list component)))))

(defun dequeue-turtle-animation (component)
  (bt:with-lock-held ((turtle-animation-component-command-lock component))
    (with-slots (command-list) component
      (when (plusp (length command-list))
        (prog1 (aref command-list 0)
          (replace command-list command-list :start1 0 :start2 1)
          (decf (fill-pointer command-list)))))))

(defun current-turtle-animation-speed (component)
  (bt:with-lock-held ((turtle-animation-component-command-lock component))
    (turtle-animation-component-speed component)))

(defun activate-turtle-animation (command orientation)
  (with-slots ((pos position) (rot rotation)) orientation
    (ecase (turtle-animation-command-kind command)
      (:move
       (let ((distance (turtle-animation-command-amount command)))
         (setf (turtle-animation-command-target command)
               (turtle-forward-position pos rot distance)
               (turtle-animation-command-direction command)
               (signum distance)
               (turtle-animation-command-remaining command)
               (abs distance))))
      (:rotate
       (let* ((amount (turtle-animation-command-amount command))
              (angle (vec3f-length amount)))
         (setf (turtle-animation-command-target command)
               (vec3f+ rot amount)
               (turtle-animation-command-remaining command)
               angle
               (turtle-animation-command-direction command)
               (if (zerop angle)
                   (vec3f 0.0 0.0 0.0)
                   (vec3f/ amount angle)))))))
  command)

(defun finish-turtle-animation (command orientation)
  (with-slots ((pos position) (rot rotation)) orientation
    (ecase (turtle-animation-command-kind command)
      (:move
       (setf pos (turtle-animation-command-target command)))
      (:rotate
       (setf rot (turtle-animation-command-target command)))))
  nil)

(defun update-turtle-move-animation (command orientation speed dt world entity-id)
  (with-slots ((pos position) (rot rotation)) orientation
    (let* ((remaining (turtle-animation-command-remaining command))
           (step-distance (min remaining (* (turtle-linear-speed speed) dt))))
      (if (<= remaining step-distance)
          (progn
            (add-turtle-point :world world :turtle entity-id)
            (setf pos (turtle-animation-command-target command))
            (add-turtle-point :world world :turtle entity-id)
            nil)
          (progn
            (add-turtle-point :world world :turtle entity-id)
            (setf pos (turtle-forward-position
                       pos rot
                       (* (turtle-animation-command-direction command)
                          step-distance))
                  (turtle-animation-command-remaining command)
                  (- remaining step-distance))
            (add-turtle-point :world world :turtle entity-id)
            command)))))

(defun update-turtle-rotate-animation (command orientation speed dt)
  (with-slots ((rot rotation)) orientation
    (let* ((remaining (turtle-animation-command-remaining command))
           (step-angle (min remaining (* (turtle-angular-speed speed) dt))))
      (if (<= remaining step-angle)
          (finish-turtle-animation command orientation)
          (progn
            (setf rot (vec3f+ rot
                              (vec3f* (turtle-animation-command-direction command)
                                      step-angle))
                  (turtle-animation-command-remaining command)
                  (- remaining step-angle))
            command)))))

(defsystem turtle-animation-system (orientation-component
                                    turtle-animation-component))

(defmethod update-system ((world world)
                          (system turtle-animation-system)
                          dt)
  (system-do-with-components ((ori orientation-component)
                              (animation turtle-animation-component))
      world system entity-id
    (with-slots (active-command) animation
      (unless active-command
        (setf active-command
              (let ((command (dequeue-turtle-animation animation)))
                (when command
                  (activate-turtle-animation command ori)))))
      (when active-command
        (let ((speed (or (turtle-animation-command-speed active-command)
                         (current-turtle-animation-speed animation))))
          (cond ((zerop speed)
                 (let ((move-command-p
                         (eq (turtle-animation-command-kind active-command)
                             :move)))
                   (when move-command-p
                     (add-turtle-point :world world :turtle entity-id))
                   (setf active-command
                         (finish-turtle-animation active-command ori))
                   (when move-command-p
                     (add-turtle-point :world world :turtle entity-id))))
                ((<= (turtle-animation-command-remaining active-command) 0.0)
                 (when (eq (turtle-animation-command-kind active-command) :move)
                   (add-turtle-point :world world :turtle entity-id))
                 (setf active-command
                       (finish-turtle-animation active-command ori))
                 (when (null active-command)
                   (add-turtle-point :world world :turtle entity-id)))
                ((eq (turtle-animation-command-kind active-command) :move)
                 (setf active-command
                       (update-turtle-move-animation active-command
                                                     ori speed dt
                                                     world entity-id)))
                ((eq (turtle-animation-command-kind active-command) :rotate)
                 (setf active-command
                       (update-turtle-rotate-animation active-command
                                                       ori speed dt)))))))))

(defsystem newtonian-system (orientation-component
                             newtonian-component))

(defmethod update-system ((world world) (system newtonian-system) dt)
  (system-do-with-components ((ori orientation-component)
                              (newt newtonian-component))
      world system id
    (with-slots (velocity mass force) newt
      (with-slots ((pos position) (rot rotation)) ori
        (let ((accel (if (zerop mass)
                         0.0
                         (/ force mass))))
          ;; semi-implicit euler integration
          (incf velocity (* accel dt))
          (when (not (zerop velocity))
            (add-turtle-point :world world :turtle id)
            (setf pos (vec3f+ pos
                              (kit.glm:matrix*vec3
                               (vec3f 0.0 (* velocity dt) 0.0)
                               (kit.glm:rotate rot))))
            (add-turtle-point :world world :turtle id)))))))

(defsystem turtle-drawer-system (orientation-component
                                 turtle-component))

(defmethod update-system ((world world)
                          (system turtle-drawer-system) dt)
  (system-do-with-components ((ori orientation-component)
                              (turt turtle-component))
      world system id
    (with-slots ((pos position) (rot rotation)) ori
      (with-slots (color) turt
        (turtle-draw :position pos
                     :rotation rot
                     :color color)))))


(defun make-turtle (&key
                      (position (vec3f 0.0 0.0 0.0))
                      (rotation (vec3f 0.0 0.0 0.0))
                      (color (vec4f 0.5 0.0 1.0 1.0))
                      (velocity 0.0)
                      (mass 1.0)
                      (force 0.0)
                      (pen-down-p t)
                      (world *world*))
  "Create a turtle entity. Return entity id."
  (let ((e (make-entity world))
        (ori (make-orientation-component
              :position position
              :rotation rotation))
        (turt (make-turtle-component
               :color color
               :pen-down-p pen-down-p))
        (mess (make-turtle-message-component))
        (anim (make-turtle-animation-component))
        (newt (make-newtonian-component
               :velocity velocity
               :mass mass
               :force force)))
    (add-components world e ori turt mess anim newt)
    (setf *turtle* e)
    e))

(defun add-turtle-data (array &key (world *world*) (turtle *turtle*))
  "Adds the turtle's current position and color to a gl-dynamic-array."
  (let ((ori (get-component world turtle 'orientation-component))
        (turt (get-component world turtle 'turtle-component)))
    (iter (for i in-vector (orientation-component-position ori))
      (gl-dyn-push array i))
    (iter (for i in-vector (turtle-component-color turt))
      (gl-dyn-push array i)))
  array)

(defun add-turtle-point (&key (world *world*) (turtle *turtle*)
                           (line-drawer *line-drawer*))

  (with-slots (pen-down-p) (ec world turtle 'turtle-component)
    (when pen-down-p
      (incf (num-vertices line-drawer))
      (add-turtle-data (draw-array line-drawer)
                       :world world
                       :turtle turtle))))
