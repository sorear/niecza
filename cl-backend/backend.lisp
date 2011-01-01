
(load (merge-pathnames "quicklisp/setup.lisp" 
			   (user-homedir-pathname)))
(ql:quickload "cl-json")
(ql:quickload "fare-matcher")

(defun to-symbol (string) (intern (string-upcase (concatenate 'string "nam-" string))))

(defun first-string-to-symbol (thing)
  (if (and thing (listp thing))
    (cons (to-symbol (first thing)) (mapcar #'first-string-to-symbol (rest thing)))
    thing
  )
)

(setq core (make-hash-table :test #'equal))
(setf (gethash "&say" core) (lambda (thing) (format t "~A~%" thing)))

;(defmacro nam-sub (name body) `(defun ,(to-symbol name) () body))
; for easier development
(defmacro nam-sub (name body) body)

(defmacro nam-ann (filename line op) op)
(defmacro nam-prog (&body ops) `(progn ,@ops))
(defun nam-sink (argument) nil)

(defun nam-str (string) string)
(defun nam-double (number) number)

; ???
(defun nam-const (thing) thing)
(defun nam-box (type thing) thing)
(defun nam-fetch (thing) thing)
(defun nam-scopedlex (var) (gethash var core))
(defun nam-subcall (dunno-what-that-is thing &rest args) (apply thing args))


(defun compile-sub (sub)
  (fare-matcher:match sub 
    ((list
        sub  ; 'sub' literal
        name
        outer ;  raw xref
        flags
        dunno-what-that-is ; [ map { $_->xref->[1] } @{ $self->zyg } ]
        parametric-role-hack
        augment-hack
        hint-hack
        is-phaser
        body-of
        in-class
        cur-pkg
        class
        ltm
        exports
        signature-params
        lexicals
        nam
    ) (list sub name nam))
 )
)

(defun compile-unit (nam)
  (fare-matcher:match nam 
    ((list mainline-xref name log setting bottom-ref filename modtime x-ref t-deps root-stash) (mapcar #'compile-sub x-ref))
  )
)
(let (
      (compiled-unit (first-string-to-symbol (first (compile-unit (json:decode-json (open "test.nam"))))))
    )
    (format t "~a~%~%~%" compiled-unit)
;    (format t "expanded: ~a~%" (macroexpand compiled-unit))
;
    (eval compiled-unit)
)
