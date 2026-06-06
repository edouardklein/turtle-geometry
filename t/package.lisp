;;;; package.lisp

(defpackage #:turtle-geometry.tests
  (:use #:cl #:turtle-geometry #:qua)
  (:shadowing-import-from #:turtle-geometry #:speed)
  (:export run-all-tests))
