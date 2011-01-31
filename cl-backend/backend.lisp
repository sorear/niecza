(load (merge-pathnames "quicklisp/setup.lisp" 
			   (user-homedir-pathname)))
(ql:quickload "cl-json")
(ql:quickload "fare-matcher")


; Logging - not sure we need it

(ql:quickload "cl-log")

(use-package "COM.RAVENBROOK.COMMON-LISP-LOG")
(setf (log-manager) (make-instance 'log-manager :message-class 'formatted-message))
(start-messenger 'text-file-messenger :filename "cl-backend.log")

(log-message :info "cl-backend started")

(defun nam-op-log (name result) "Log what an nam op gets transformed into" (log-message :info (format nil "~a => ~w" name (strip-ann result))) result)


; Macros 
 
(defmacro nam-op (name params &body body) `(defmacro ,(concat-symbol 'nam- name) ,params (nam-op-log ',name (progn ,@body))))


(defun strip-ann (thing) 
  (if (consp thing)
      (if (eq (first thing) 'nam-ann)
          (strip-ann (fourth thing)) 
          (mapcar #'strip-ann thing)
      )
      thing))

(defun concat-symbol (a b) (intern (concatenate 'string (string a) (string b))))



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

 

(defmacro define-nam-sub  
  (i                ; The Xref Id
    name             ; Sub's name for backtraces
    outer_xref       ; OUTER:: sub, may be in a setting unit
    flags            ; See doc/nam.pod
    children         ; Supports tree traversals
    param_role_hack  ; See doc/nam.pod
    augment_hack     ; See doc/nam.pod
    hint_hack        ; See doc/nam.pod
    is_phaser        ; See doc/nam.pod
    body_of          ; Only valid in immediate block of class {} et al
    in_class         ; Innermost enclosing body_of
    cur_pkg          ; OUR:: as a list of names
    class            ; &?BLOCK.WHAT; "Sub" or "Regex"
    ltm              ; Only for regexes; stores declarative prefix
    exports          ; List of global names
    signature        ; May be null in exotic cases
    lexicals         ; Come in multiple forms
    nam              ; See description of opcodes earlier
  )

  `(defun ,(sub-symbol i) () (let ,(lexicals-to-let lexicals) ,@nam)))



(defun xref-to-subsymbol (xref) (sub-symbol (cadr xref)))

(defun compile-method (method)
  (fare-matcher:match method 
    ((and (list name normal dunno xref) (when (equal normal "normal")))
      `(defmethod ,(intern name) (invocant) (,(xref-to-subsymbol xref))))))


(defun define-nam-class (name attributes methods)
  `(progn 
    (defclass ,(class-symbol name) (p6-Object) ())
    ,@(mapcar #'compile-method methods)
  ))



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


(nam-op ann (filename line op) op)
(nam-op prog (&body ops) `(progn ,@ops))
(defun nam-sink (argument) nil)

(defun nam-str (string) string)
(defun nam-double (number) number)

(nam-op assign (to what) `(STORE ,to ,what))

(defun nam-const (thing) thing)
(defun nam-box (type thing) thing)
(defun nam-fetch (thing) (FETCH thing))

(nam-op letvar (var) (intern var))

(nam-op scopedlex (var &rest rvalue)
  (if (consp rvalue)
      `(setf ,(intern var) ,@rvalue)
      (intern var)))

(defun to-let-vars (vars)
  (if (consp vars)
      (cons (list (intern (car vars)) (cadr vars)) (to-let-vars (cddr vars)))
      '()))
(nam-op letn (&body vars-and-body)
 `(let ,(to-let-vars (butlast vars-and-body)) ,(first (last vars-and-body))))

; ???
(defun nam-subcall (dunno-what-that-is thing &rest args) (apply thing args))

(nam-op methodcall (method-name dunno invocant &rest args) 
  `(,(intern (cadr method-name)) ,@args))


; HACK
(nam-op corelex (var) `(nam-scopedlex ,var))

(nam-op newboundvar (dunno1 dunno2 thing) thing)

(defun compile-sub-or-packagoid (i def)
  (let* ((type (first def))
        (args (if (equal type "sub") (append (butlast (rest def)) (list (to-symbol-first (last def)))) (rest def))))

    `(
      ,(intern (string-upcase (concatenate 'string "define-nam-" type)))
      ,i
      ,@args
      )))



;;    ((and (list
;;      class ; "class" literal
;;      name
;;      exports
;;      attributes
;;      methods
;;      superclasses
;;      linearized_mro
;;    ) (when (equal class "class"))) (list 'define-nam-class name attributes methods))))



(defun compile-unit (nam)
  (fare-matcher:match nam 
    ((list
      mainline_ref    ;  Xref to mainline subroutine
      name            ;  Unit's unique name
      log             ;  Mostly unused vestige of last stash system
      setting         ;  Name of setting unit or null
      bottom_ref      ;  Xref to sub containing {YOU_ARE_HERE}, or null
      filename        ;  Filename of source code or null
      modtime         ;  Seconds since 1970-01-01
      xref            ;  Resolves refs from other units
      tdeps           ;  Holds dependency data for recompilation
      stash_root      ;  Trie holding classes and global variables
    ) (loop for thing in xref for i upfrom 0 collect (compile-sub-or-packagoid i thing)))))

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
  ;(format t "~w~%~%~%" (json:decode-json (open (first *args*))))
  ;(format t "~w~%~%~%" compiled-unit)
  (format t "~w~%~%~%" (strip-ann compiled-unit))
  (let ((wrapped (wrap-for-eval compiled-unit)))
    (eval wrapped)
    ))
