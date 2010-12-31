
(load (merge-pathnames "quicklisp/setup.lisp" 
			   (user-homedir-pathname)))
(ql:quickload "cl-json")
(ql:quickload "fare-matcher")

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
    ) (list 'this-is-a-sub nam))
 )
)

(defun compile-unit (nam)
  (fare-matcher:match nam 
    ((list mainline-xref name log setting bottom-ref filename modtime x-ref t-deps root-stash) (mapcar #'compile-sub x-ref))
  )
)
(format t "~a" (compile-unit (json:decode-json (open "test.nam"))))
