(load (merge-pathnames "quicklisp/setup.lisp" 
			   (user-homedir-pathname)))

(ql:quickload "cl-json")
(ql:quickload "fare-matcher")

(defpackage niecza (:use common-lisp)) 
(in-package :niecza)

; Macros 
 
 
(defun xref-to-symbol (xref) (intern (concatenate 'string "XREF-" (first xref) "-" (write-to-string (second xref)))))

(load "cl-backend/niecza-stash.lisp")



(defun main-xref (i) (xref-to-symbol (list "MAIN" i "...")))

(defmacro get-stash (name) (niecza-stash:to-stash-name name))
 
(defmacro nam-op (name params &body body) `(defmacro ,(concat-symbol 'nam- name) ,params ,@body))

(defun concat-symbol (a b) (intern (concatenate 'string (string a) (string b))))

; Translation to symbols

(defun method-name (name) (intern name))
(defun var-name (name) (intern name))

; Hacks

(nam-op ehspan (class name unused start end goto) )
(nam-op span (n1 n2 sync body) body)

(defun strip-ann (thing) 
  (if (consp thing)
      (if (eq (first thing) 'nam-ann)
          (strip-ann (fourth thing)) 
          (mapcar #'strip-ann thing)
      )
      thing))

(defun to-symbol-first (thing)
  (if (stringp thing)
      (intern (string-upcase (concatenate 'string "nam-" thing)))
      (to-symbol thing)))

(defun to-symbol (thing)
  (if (consp thing)
      (cons (to-symbol-first (first thing)) (mapcar #'to-symbol (rest thing)))
      thing))

; P6 Classes

 
(defun make-scalar (value) (let ((scalar (make-instance 'p6-Scalar)))
  (setf (slot-value scalar 'value) value)
  scalar))

(defclass p6-Mu () ())
(defgeneric |new| (invocant &rest rest))
(defmethod |new| (invocant &rest rest) (make-instance (class-of invocant)))


(defclass p6-Scalar () (value))

(defgeneric |FETCH| (value)) 
(defmethod |FETCH| ((container p6-Scalar)) (slot-value container 'value))
(defmethod |STORE| ((container p6-Scalar) value) (setf (slot-value container 'value)value))

(defmethod |FETCH| (thing) thing)


(defun compile-param (param)
  (fare-matcher:match param 
    ((list 
    name            ; For binding error messages
    flags           ; See doc/nam.pod
    slot            ; Name of lexical to accept value
    names           ; All legal named-parameter names
    default         ; Xref    Sub to call if HAS_DEFAULT; must be child of this
    ) (if (equal flags 96)
          (var-name slot)
          nil))))

 
(defun mymap (func list) (remove-if #'null (mapcar func list)))

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

  `(defun ,(main-xref i) ,(mymap #'compile-param signature) (let ,(lexicals-to-let lexicals) ,@nam)))


(defun xref-to-subsymbol (xref) (main-xref (cadr xref)))

; HACK
(defmacro define-nam-module (
 i
 name            ; The object's debug name
 exports         ; List of global names to which object is bound
) `(setf ,(main-xref i) 'placeholder))

(defmacro define-nam-class (
  i
  name            ; The object's debug name
  exports         ; List of global names to which object is bound
  attributes      ; Attributes local to the class
  methods         ; Methods local to the class
  superclasses    ; Direct superclasses of the class
  linear_mro      ; All superclasses in C3 order
  ) 
  (let ((class (main-xref i)))
    `(progn
      (defclass ,class (p6-Mu) ())
      ,@(mapcar (lambda (m) (compile-method class m)) methods)
      (setf ,(main-xref i) (make-instance ',(main-xref i)))
    )))

(defun method-name (name) (intern name))

(defun compile-method (class method)
  (fare-matcher:match method 
    ((and (list 
      name ; Method name without ! decorator
      kind ; Allowable kinds are "normal", "private", and "sub"
      var  ; Variable for implementing sub in param role
      body ; Reference to implementing sub
    ) (when (equal kind "normal")))
      `(defmethod ,(method-name name) ((invocant ,class) &rest rest) (apply ',(xref-to-subsymbol body) invocant rest)))))

; converts one lexical to a variable declaration for a let
(defun lexical-to-let (lexical)
  (fare-matcher:match lexical 
    ((and (list var sub dunno-1 id dunno-2) (when (equal sub "sub"))) (list (var-name var) `(symbol-function ',(main-xref id))))
    ((and (list var simple flags) (when (equal simple "simple")))
      (if (equal flags 4)
          nil
          (list (var-name var) (make-scalar ""))))
    ((and (list* var stash path) (when (equal stash "stash")))
       (list (var-name var) `(get-stash ,path)))))


; converts a list of lexicals
(defun lexicals-to-let (lexicals) (remove-if #'null (mapcar #'lexical-to-let lexicals)))


(nam-op ann (filename line op) op)
(nam-op prog (&body ops) `(progn ,@ops))
(defun nam-sink (argument) nil)

(defun nam-str (string) string)
(defun nam-double (number) number)

(nam-op assign (to what) `(STORE ,to ,what))

(defun nam-const (thing) thing)
(defun nam-box (type thing) thing)
(defun nam-fetch (thing) (FETCH thing))

(nam-op letvar (&rest args) `(nam-scopedlex ,@args))

(nam-op scopedlex (var &rest rvalue)
  (if (consp rvalue)
      `(setf ,(var-name var) ,@rvalue)
      (var-name var)))

(labels 
  ((seperate (mixed)
    (if (stringp (first mixed)) 
        (let ((result (seperate (rest (rest mixed)))))
          (list 
            (cons (list (var-name (first mixed)) (second mixed)) (first result))
            (second result))
          )
        (list nil mixed))))

  (nam-op letn (&body vars-and-body)
    (let ((seperated (seperate vars-and-body)))
    `(let* ,(first seperated) ,@(second seperated)))))


; ???
(defun nam-subcall (dunno-what-that-is thing &rest args) (apply thing args))


(labels
  ((to-string (arg) (if (stringp arg)
                        arg
                        (if (eq (first arg) 'nam-str)
                            (second arg)
                            (format t "Can't handle that sort of method call yet")
                            ))))
(nam-op methodcall (method-name dunno invocant &rest args) 
  `(,(method-name (to-string method-name)) (FETCH ,(first args)) ,@(rest args))))


(defun nam-obj_getbool (obj) (if (numberp obj) (not (equal obj 0)) obj))
(nam-op ternary (cond if then) `(if ,cond ,if ,then))

(defun nam-null (type) nil) 


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
    )

    (list (niecza-stash:wrap-in-let stash_root 
      (loop for thing in xref for i upfrom 0 when thing collect (compile-sub-or-packagoid i thing)))))))

(defun print-thing (thing) (format t "~A" (FETCH thing)))
(defun p6-say (&rest things) (mapcar #'print-thing things) (format t "~%"))
(defun p6-concat (&rest things) (apply 'concatenate 'string (mapcar #'FETCH things)))

; BIF

(defun nam-bif_plus (a b) (+ (FETCH a) (FETCH b)))

(defun nam-bif_minus (a b) (- (FETCH a) (FETCH b)))

(defun nam-bif_mul (a b) (* (FETCH a) (FETCH b)))

(defun nam-bif_numgt (a b) (> (FETCH a) (FETCH b)))
(defun nam-bif_numlt (a b) (< (FETCH a) (FETCH b)))

(defun nam-bif_numeq (a b) (= (FETCH a) (FETCH b)))

(nam-op whileloop (until once cond body) `(loop while ,cond do ,body))

(defun nam-label (label))


(defun wrap-for-eval (compiled-unit)
  `(let ((|&infix:<~>| #'p6-concat)
         (|&say| #'p6-say)
         (|Nil| "") ; HACK
         (|Any| "") ; HACK
         )
      ,@compiled-unit (,(main-xref 0))))

(let ((compiled-unit (compile-unit (json:decode-json (open (first common-lisp-user::*args*))))))
  ;(format t "~w~%~%~%" (json:decode-json (open (first *args*))))
  ;(format t "~w~%~%~%" compiled-unit)
  (format t "--------~%~%~w~%~%~%" (strip-ann compiled-unit))
  (let ((wrapped (wrap-for-eval compiled-unit)))
    (eval wrapped)
    ))
