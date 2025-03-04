;;;; Proof of concept for creating a local hidden configuration file (not for
;;;; user) that can write and read from it so that the files in the directory
;;;; can be dynamically updated.

(ql:quickload :eo-config)

(defpackage :att-test
  (:use :cl :eo-config))

(in-package :att-test)

(defvar *config-dir* (merge-pathnames "att.config" (uiop:getcwd)))

;; Default attributes. Need to define it in the future config file
;(defvar *default* '(*job-name* x y))

(define-allowed-names
  'default
  'job-name
  'test
  'hello)

(eo-config:make-config-globals (load-config *config-dir*) *config-allowed-names*)

; *config-globals-list*
; *config-allowed-names*

;(eo-config/utils:flush-global '*config-globals-list*)

;; Create hidden file

(defun create-hidden (def)
  (print "Creating File...")
  (with-open-file (f (merge-pathnames ".attributes" (uiop:getcwd))
                     :if-exists :supersede ; change this later so it doesn't clobber the file. Testing purposes only
                     :if-does-not-exist :create
                     :direction :output)
    (format f "~S" (collect-attributes def))))

(defun convert-to-global (lst)
  "converts a list of symbols into a list of symbols in a global format"
  (let ((temp '()))
    (mapcar #'(lambda (x)
                (push (find-globalized-symbol x) temp)) lst)
    (reverse temp)))

(defun find-globalized-symbol (sym)
  "converts symbol into globalized format and tries to find it. Returns the symbol it found or nil."
  (nth-value 0 (find-symbol (symbol-name (eo-config/utils:globalize-symbol sym)))))

;; Collects all attributes to stick into the file

(defun collect-attributes (lst)
  (let ((master '()))
    (dolist (x lst)
      (if (boundp x)
          (push (eval `(list ',x ,x)) master)
          nil))
    (reverse master)))

;(ql:quickload :eo-config)
