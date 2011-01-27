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
(defun class-symbol (i) (intern (concatenate 'string "CLASS-" (write-to-string i))))

; P6 Classes

(defun trace (value) (format t "~w~%" value) value)
 
(defun make-scalar (value) (let ((scalar (make-instance 'p6-Scalar)))
  (setf (slot-value scalar 'value) value)
  scalar))

(defclass p6-Object () ())
(defgeneric |new| (invocant))
(defmethod |new| (invocant) (make-instance (class-of invocant)))


(defclass p6-Scalar () (value))

(defgeneric |FETCH| (value)) 
(defmethod |FETCH| ((container p6-Scalar)) (slot-value container 'value))
(defmethod |STORE| ((container p6-Scalar) value) (setf (slot-value container 'value)value))

(defmethod |FETCH| (thing) thing)

 

(defmacro nam-sub (i lexicals body) 
  `(defun ,(sub-symbol i) () (let ,(lexicals-to-let lexicals) ,body)))

(defun xref-to-subsymbol (xref) (sub-symbol (cadr xref)))

(defun compile-method (method)
  (fare-matcher:match method 
    ((and (list name normal dunno xref) (when (equal normal "normal")))
      `(defmethod ,(intern name) (invocant) (,(xref-to-subsymbol xref))))))


(defmacro nam-class (name attributes methods)
  (trace methods)
  (trace
  `(progn 
    (defclass ,(class-symbol name) (p6-Object) ())
    ,@(mapcar #'compile-method methods)
  )))



; HACK
(defun stash-to-lisp-expression (path) `(make-instance (class-symbol (first (last ',path)))))

; converts one lexical to a variable declaration for a let
(defun lexical-to-let (lexical)
  (fare-matcher:match lexical 
    ((and (list var sub dunno-1 id dunno-2) (when (equal sub "sub"))) (list (intern var) `(symbol-function ',(sub-symbol id))))
    ((and (list var simple dunno) (when (equal simple "simple"))) (list (intern var) (make-scalar "")))
    ((and (list* var stash path) (when (equal stash "stash")))
       (list (intern var) (stash-to-lisp-expression path)))))

; converts a list of lexicals
(defun lexicals-to-let (lexicals) (mapcar #'lexical-to-let lexicals))


(defmacro nam-ann (filename line op) op)
(defmacro nam-prog (&body ops) `(progn ,@ops))
(defun nam-sink (argument) nil)

(defun nam-str (string) string)
(defun nam-double (number) number)

(defmacro nam-assign (to what) `(STORE ,to ,what))

(defun nam-const (thing) thing)
(defun nam-box (type thing) thing)
(defun nam-fetch (thing) (FETCH thing))

(defmacro nam-letvar (var) (intern var))

(defmacro nam-scopedlex (var &rest rvalue)
  (if (consp rvalue)
      `(setf ,(intern var) ,@rvalue)
      (intern var)))

(defun to-let-vars (vars)
  (if (consp vars)
      (cons (list (intern (car vars)) (cadr vars)) (to-let-vars (cddr vars)))
      '()))
(defmacro nam-letn (&body vars-and-body)
 `(let ,(to-let-vars (butlast vars-and-body)) ,(first (last vars-and-body))))

; ???
(defun nam-subcall (dunno-what-that-is thing &rest args) (apply thing args))

(defmacro nam-methodcall (method-name dunno invocant &rest args) 
  `(,(intern (cadr method-name)) ,@args))


; HACK
(defmacro nam-corelex (var) `(nam-scopedlex ,var))

(defmacro nam-newboundvar (dunno1 dunno2 thing) thing)



(defun compile-sub (i sub)
  (fare-matcher:match sub 
    ((and (list
      sub  ; "sub" literal
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
    ) (when (equal sub "sub"))) (list 'nam-sub i lexicals (to-symbol-first nam)))
    ((and (list
      class ; "class" literal
      name
      exports
      attributes
      methods
      superclasses
      linearized_mro
    ) (when (equal class "class"))) (list 'nam-class name attributes methods))))

(defun compile-unit (nam)
  (fare-matcher:match nam 
    ((list mainline-xref name log setting bottom-ref filename modtime x-ref t-deps root-stash) (loop for sub in x-ref for i upfrom 0 collect (compile-sub i sub)))))

(defun print-thing (thing) (format t "~A" (FETCH thing)))
(defun p6-say (&rest things) (mapcar #'print-thing things) (format t "~%"))
(defun p6-concat (&rest things) (apply 'concatenate 'string (mapcar #'FETCH things)))


(defun wrap-for-eval (compiled-unit)
  `(let ((|&infix:<~>| #'p6-concat)
         (|&say| #'p6-say)
         (|Nil| "") ; HACK
         )
      ,@compiled-unit (,(sub-symbol 0))))


(let ((compiled-unit (compile-unit (json:decode-json (open (first *args*))))))
  (format t "~w~%~%~%" compiled-unit)
  (let ((wrapped (wrap-for-eval compiled-unit)))
;    (format t "~a~%" wrapped)
    (eval wrapped)
    ))
