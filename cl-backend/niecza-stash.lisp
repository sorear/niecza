(defpackage niecza-stash (:export wrap-in-let to-stash-name) (:use :common-lisp))
(in-package :niecza-stash)

(ql:quickload "fare-matcher")

(defun to-stash-name (name) (intern (format nil "~{~A~^-~}" name)))

(defun entry (prefix node) 
  (fare-matcher:match node
    ((and (list name var Xref ChildNode) (when (equal var "var")))
        (cons (list (append prefix (list name)) Xref) (process (cons name prefix) ChildNode)))))

;;(trace entry)

(defun process (prefix nodes) (apply 'append (mapcar #'(lambda (x) (entry prefix x)) nodes)))



(defun wrap-in-let (stash body)
  (let ((processed (process '() stash)))
    `(let
       ,(mapcar (lambda (x) (list (to-stash-name (first x)) nil)) processed)
       ,@body
       ,@(set-stash processed))))

(defun only-with-xrefs (stash) (remove-if (lambda (x) (not (second x))) stash))

(defun set-stash (stash)
  (mapcar (lambda (x) `(setf ,(to-stash-name (first x)) ,(niecza::xref-to-symbol (second x)))) (only-with-xrefs stash)))

(defun hide-foreign (stash)
  (remove-if
    (lambda (x)
      (and (second x) (not (equal (first (second x)) "MAIN"))))
    stash))

