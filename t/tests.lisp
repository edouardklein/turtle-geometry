;;;; tests.lisp

(in-package #:turtle-geometry.tests)

(defun test-square-svg-export ()
  "Draw a square with animations on, export SVG, and verify the SVG
contains a minimal square path (no animation intermediate points)."
  (let ((turtle-geometry::*world* (make-world))
        (turtle-geometry::*line-drawer* nil)
        (turtle-geometry::*turtle-drawer* nil)
        (turtle-geometry::*camera* nil)
        (turtle-geometry::*program-manager* nil)
        (orig-add-turtle-point (symbol-function 'turtle-geometry::add-turtle-point)))
    (unwind-protect
        (progn
          ;; Stub out the GL-dependent point recorder so we can run
          ;; the animation system without an OpenGL context.
          (setf (symbol-function 'turtle-geometry::add-turtle-point)
                (lambda (&key world turtle line-drawer)
                  (declare (ignore world turtle line-drawer))))

          (add-systems turtle-geometry::*world*
                       (make-instance 'turtle-geometry::turtle-animation-system))
          (turtle-geometry::make-turtle :world turtle-geometry::*world*)

          ;; Draw a square with animations on (default speed = 6)
          (turtle-geometry::forward 10)
          (turtle-geometry::right (/ pi 2))
          (turtle-geometry::forward 10)
          (turtle-geometry::right (/ pi 2))
          (turtle-geometry::forward 10)
          (turtle-geometry::right (/ pi 2))
          (turtle-geometry::forward 10)
          (turtle-geometry::right (/ pi 2))

          ;; Pump animation system until queue is drained.
          ;; With speed 6 and dt=1.0 each command finishes in a single step,
          ;; so 20 iterations is more than enough.
          (dotimes (i 20)
            (update-world turtle-geometry::*world* 1.0))

          ;; Export SVG
          (let ((path "/tmp/turtle-test-square.svg"))
            (when (probe-file path)
              (delete-file path))
            (assert (fboundp 'turtle-geometry::export-svg) nil
                    "export-svg function is not defined")
            (turtle-geometry::export-svg path)
            (assert (probe-file path) nil
                    "SVG file was not created at ~A" path)
            (let ((content
                   (with-open-file (stream path :direction :input)
                     (let* ((len (file-length stream))
                            (str (make-string len)))
                       (read-sequence str stream)
                       str))))
              ;; Should contain exactly one <path ...> element
              (assert (search "<path" content) nil
                      "No <path> element in exported SVG")
              (let* ((d-start (search "d=" content))
                     (quote-start (when d-start
                                    (position (code-char 34) content :start d-start)))
                     (quote-end (when quote-start
                                  (position (code-char 34) content :start (1+ quote-start)))))
                (assert d-start nil "No d= attribute in <path>")
                (assert quote-start nil "No opening quote for d attribute")
                (assert quote-end nil "No closing quote for d attribute")
                (let ((d-str (subseq content (1+ quote-start) quote-end)))
                  (let ((m-count 0) (l-count 0))
                    (loop for ch across d-str do
                      (cond ((char= ch (code-char 77)) (incf m-count))
                            ((char= ch (code-char 76)) (incf l-count))))
                    (assert (= 1 m-count) nil
                            "Expected exactly 1 M command, got ~A" m-count)
                    (assert (= 4 l-count) nil
                            "Expected exactly 4 L commands for a square, got ~A"
                            l-count)))))))
      (setf (symbol-function 'turtle-geometry::add-turtle-point)
            orig-add-turtle-point))))

(defun run-all-tests ()
  (test-square-svg-export))

;;; Hooking into ASDF
#|
To run:

(ql:quickload :turtle-geometry.tests)
(asdf:perform :test-op :turtle-geometry.tests)

|#
(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :turtle-geometry.tests))))
  (format t "~2&*******************~@
                ** Starting test **~@
                *******************~%~%")
  (handler-bind ((style-warning #'muffle-warning)) (run-all-tests))
  (format t "~2&*****************************************~@
                **            Tests finished           **~@
                *****************************************~%"))
