;;;; Proof of concept for creating a local hidden configuration file (not for
;;;; user) that can write and read from it so that the files in the directory
;;;; can be dynamically updated.

;(ql:quickload :eo-config)
;(load #P "attributes.lisp")

(defpackage :att-test
  (:use :cl :sb-mop :eo-config :eo-config/utils))
(in-package :att-test)

;; Default attributes. Need to define it in the future config file
;(defvar *default* '(*job-name* x y))

(defclass hidden-attributes (config-set)
  ((file-name
     :initarg :file-name
     :accessor file-name)))

(defparameter x (make-instance 'hidden-attributes :file-name "att.config"))

;; Create hidden file

(defun create-hidden (file)
  (print "Creating File...")
  (with-open-file (f (merge-pathnames ".attributes" (uiop:getcwd))
                     :if-exists :supersede ; change this later so it doesn't clobber the file. Testing purposes only
                     :if-does-not-exist :create
                     :direction :output)
    (format f "~S" file)))

;(create-hidden (eo-config/utils:load-config (file-name x)))

;(defun create-hidden (def)
  ;(print "Creating File...")
  ;(with-open-file (f (merge-pathnames ".attributes" (uiop:getcwd))
                     ;:if-exists :supersede ; change this later so it doesn't clobber the file. Testing purposes only
                     ;:if-does-not-exist :create
                     ;:direction :output)
    ;(format f "~S" (collect-attributes (convert-to-global def)))))

(defun get-all-slot-names (obj)
"Get all slot names of the object, including the inherited classes"
  (flatten (mapcar #'(lambda (x)
                       (mapcar #'slot-definition-name (class-direct-slots x))) (get-all-superclasses obj))))

(defun get-all-superclasses (obj)
  "Returns a list of all of the inherited superclasses"
  (if (eql (find-class 'standard-object) (car (class-direct-superclasses (class-of obj)))) ; compare direct superclass against the standard object. If t, return the list of all superclasses of the object
      '()
      (cons (car (class-direct-superclasses (class-of obj)))
            (get-all-superclasses
              (make-instance (car (class-direct-superclasses (class-of obj))))))))

(defun concat-string-list (lst)
  (if (listp lst)
      (let ((result ""))
        (dolist (item lst)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(defun get-slot (obj slot-name)
  "Gets the value of the given slot name of an object. On failure, returns type error."
  (handler-case (slot-value obj slot-name)
    (error (c)
      c)))

(defun process-slots (obj slot-name)
  (if (eql (type-of (get-slot obj slot-name)) 'unbound-slot)
      ""
      (format nil "~%~A: ~A" slot-name (slot-value obj slot-name))))

(defun pprint-obj (obj)
  "pretty print the classes of the object, including the inherited ones"
  (let ((slots-raw (get-all-slot-names obj))
        (slots (concat-string-list (mapcar #'(lambda (x) (process-slots obj x)) (get-all-slot-names obj))))
        (errors '()))
    (dolist (el slots-raw)
      (if (eql (type-of (get-slot obj el)) 'unbound-slot) (push (get-slot obj el) errors)))
    (format t "~%Contents of ~A:~%Warnings: ~{~%~S~%~}~A" obj errors slots)))

;(ql:quickload :eo-config)
