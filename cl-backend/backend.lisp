(load (merge-pathnames "quicklisp/setup.lisp" 
			   (user-homedir-pathname)))
(ql:quickload "cl-json")
(ql:quickload "fare-matcher")


(defun to-symbol-first (thing)
  (if (stringp thing)
      (intern (string-upcase (concatenate 'string "nam-" thing)))
      (to-symbol thing)))

(defun to-symbol (thing)
  (if (consp thing)
      (cons (to-symbol-first (first thing)) (mapcar #'to-symbol (rest thing)))
      thing))


 
(defun sub-symbol (i) (intern (concatenate 'string "SUB-" (write-to-string i))))

(defun lexical-to-let (lexical)
  (fare-matcher:match lexical 
    ((list var x y id name) (list (intern var) `(symbol-function ',(sub-symbol id))))
    ((list var simple dunno) (list (intern var) '(quote NYI)))))

(defun lexicals-to-let (lexicals) (mapcar #'lexical-to-let lexicals))

(defmacro nam-sub (i lexicals body) 
  `(defun ,(sub-symbol i) () (let ,(lexicals-to-let lexicals) ,body)))

(defmacro nam-ann (filename line op) op)
(defmacro nam-prog (&body ops) `(progn ,@ops))
(defun nam-sink (argument) nil)

(defun nam-str (string) string)
(defun nam-double (number) number)

; ???
(defun nam-const (thing) thing)
(defun nam-box (type thing) thing)
(defun nam-fetch (thing) thing)
(defmacro nam-scopedlex (var) (intern var))
(defun nam-subcall (dunno-what-that-is thing &rest args) (apply thing args))


(defun compile-sub (i sub)
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
    ) (list 'nam-sub i lexicals (to-symbol-first nam)))))

(defun compile-unit (nam)
  (fare-matcher:match nam 
    ((list mainline-xref name log setting bottom-ref filename modtime x-ref t-deps root-stash) (loop for sub in x-ref for i upfrom 0 collect (compile-sub i sub)))))

(defun wrap-for-eval (compiled-unit)
  `(let 
     ((|&say| (lambda (thing) (format t "~A~%" thing))))
      ,@compiled-unit (,(sub-symbol 0))))


(let ((compiled-unit (compile-unit (json:decode-json (open (first *args*))))))
  (format t "~a~%~%~%" compiled-unit)
  (let ((wrapped (wrap-for-eval compiled-unit)))
;    (format t "~a~%" wrapped)
    (eval wrapped)
    ))
