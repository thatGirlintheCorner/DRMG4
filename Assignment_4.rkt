#lang typed/racket

(require typed/rackunit)

;; PROGRESS STATEMENT GOES HERE

;; DRMG4 Grammer
;; <expr> ::= <num>
;;         |  <id>
;;         |  <string>
;;         |  { if <expr> <expr> <expr> }
;;         |  { local [<id> = <expr>]* in <expr> }
;;         |  {% <id>* -> <expr> }
;;         |  { <expr> <expr>* }
;; ... id CANNOT be if, =, local, %, ->, in

;; define DRMG4 data types
(define-type ExprC (U numC idC strC ifC lamC appC))
(define-type Value (U numV boolV strV cloV primV))

;; define Environment-related data types
(struct binding ([name : Symbol] [val : Value]))
(define-type Env (Listof binding))

;; ExprC type structs
(struct numC ([n : Real]) #:transparent)
(struct idC ([i : Symbol]) #:transparent)
(struct strC ([s : String]) #:transparent)
(struct ifC ([cond : ExprC] [t : ExprC] [f : ExprC]) #:transparent)
(struct lamC ([args : (Listof idC)] [body : ExprC]) #:transparent)
(struct appC ([func : ExprC] [args : (Listof ExprC)]) #:transparent)

;; Value type structs
(struct numV ([n : Real]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct cloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct primV ([op : Symbol] ) #:transparent)

;; top environment
(define top-env (list
                 (binding 'true (boolV #t))
                 (binding 'false (boolV #f))
                 (binding '+ (primV '+))
                 (binding '- (primV '-))
                 (binding '* (primV '*))
                 (binding '/ (primV '/))
                 (binding '<= (primV '<=))
                 (binding 'substring (primV 'substring))
                 (binding 'strlen (primV 'strlen))
                 (binding 'equal? (primV 'equal?))
                 (binding 'error (primV 'error))))

;; check-id
;; check for the validity of ids
(define (check-id [id : Symbol]) : Boolean
  (cond
    [(equal? id 'if) #f]
    [(equal? id '=) #f]
    [(equal? id 'local) #f]
    [(equal? id '%) #f]
    [(equal? id '->) #f]
    [(equal? id 'in) #f]
    [else #t]))

(check-equal? (check-id 'if) #f)
(check-equal? (check-id '=) #f)
(check-equal? (check-id 'local) #f)
(check-equal? (check-id '%) #f)
(check-equal? (check-id '->) #f)
(check-equal? (check-id 'in) #f)
(check-equal? (check-id 'hi) #t)

;; serialize
;; takes in a value from DRMG4 and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(numV n) (format "~v" n)]
    [(boolV b) (if b "true" "false")]
    [(strV s) (format "~v" s)]
    [(cloV _ _ _) "#<procedure>"]
    [(primV _) "#<primop>"]))

(check-equal? (serialize (numV 3)) "3")
(check-equal? (serialize (boolV #t)) "true")
(check-equal? (serialize (boolV #f)) "false")
(check-equal? (serialize (strV "hello")) "\"hello\"")
(check-equal? (serialize (cloV '() (numC 3) '())) "#<procedure>")
(check-equal? (serialize (primV '+)) "#<primop>")

;; lookup
;; look up binding in current environment
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "DRMG: ~e not found" for)]
    [(cons (binding name val) r)
     (if (check-id name) (cond
                           [(symbol=? for name) val]
                           [else (lookup for r)])
         (error 'lookup "DRMG: ~e is not a valid name" for))]))

;; extend-env
;; extend current environment to include interpretations of arguments
(define (extend-env [a-names : (Listof Symbol)] [args : (Listof Value)] [clo-env : Env]) : Env
  (match* (a-names args)
    [('() '()) clo-env]
    [('() l)
     (define vals : (Listof String)
       (for/list ([i l])
         (serialize i)))
     (error 'extend-env "DRMG: ~e must have associated names" vals)]
    [(l '())
     (error 'extend-env "DRMG: ~e must have associated values" l)]
    [((cons f-names r-names) (cons f-args r-args))
     (cons (binding f-names f-args)
           (extend-env r-names r-args clo-env))]))

;; get-args
;; get all symbols of args from a lamC
(define (get-args [a : (Listof idC)]) : (Listof Symbol)
  (match a
    ['() '()]
    [(cons f r) (cons (idC-i f) (get-args r))]))

;; data?
;; predicate for if a given value contains data or not
(define (data? [v : Value])
  (match v
    [(numV _) #t]
    [(strV _) #t]
    [(boolV _) #t]
    [_ #f]))

;; apply-op
;; apply the primitive operations to given values
(define (apply-op [op : Symbol] [vals : (Listof Value)]) : Value
  (match* (op vals)
    [('+ (list (numV a) (numV b))) (numV (+ a b))]
    [('- (list (numV a) (numV b))) (numV (- a b))]
    [('* (list (numV a) (numV b))) (numV (* a b))]
    [('/ (list (numV a) (numV b)))
     (if (= b 0)
         (error 'apply-op "DRMG: cannot divide ~e by 0" a)
         (numV (/ a b)))]
    [('<= (list (numV a) (numV b))) (boolV (<= a b))]
    [('substring
      (list (strV s) (numV (? natural? start)) (numV (? natural? stop))))
       (if (<= 0 start stop (string-length s))
           (strV (substring s start stop))
           (error 'apply-op "DRMG: invalid use of substring"))]
    [('strlen (list (strV s))) (numV (string-length s))]
    [('equal? (list a b)) (if (and (data? a) (data? b))
                                (boolV (equal? a b))
                                (boolV #f))]
    [('error (list v)) (error 'apply-op "DRMG: user-error in ~e" (serialize v))]
    [(o v)
     (define vals : (Listof String)
       (for/list ([i v])
         (serialize i)))
     (error 'apply-op "DRMG: invalid use of ~e on ~e" o vals)]))

;; parse

;; interp
;; interperates provided abstact sytax trees in the form of ExprC's into values
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    ;; numC
    [(numC n) (numV n)]
    ;; strC
    [(strC s) (strV s)]
    ;; idC 
    [(idC i) (lookup i env)]
    ;; lamC
    [(lamC a b) (cloV (get-args a) b env)]
    ;; appC
    [(appC f as)
     (define vals : (Listof Value)
       (for/list ([a as])
         (interp a env)))
     (match (interp f env)
       [(cloV syms body clo-env)
        ;; interp body w extened env
        (interp body
                ;; extend current env with interpretations of args
                (extend-env syms vals clo-env))]
       [(primV op) (apply-op op vals)]
       [other (error 'interp "DRMG: ~e must be a valid function" (serialize other))])]
    ;; ifC
    [(ifC c t f)
     (match (interp c env)
       [(boolV #t) (interp t env)]
       [(boolV #f) (interp f env)]
       [other (error 'interp "DRMG: ~e must evaluate to a boolean value"
                     (serialize other))])]))

(check-equal? (interp (numC 3) top-env) (numV 3))
(check-equal? (interp (strC "hello") top-env) (strV "hello"))
(check-equal? (interp (ifC (appC (idC '<=) (list (numC 2) (numC 3)))
                           (numC 2) (numC 3)) top-env) (numV 2))
(check-equal? (interp (ifC (appC (idC '<=) (list (numC 3) (numC 2)))
                           (numC 2) (numC 3)) top-env) (numV 3))
(check-equal? (interp (appC (lamC (list (idC 'x))
                                  (appC (idC '+) (list (numC 2) (idC 'x))))
                            (list (numC 1))) top-env) (numV 3))
(check-equal? (interp (appC (lamC (list (idC 'x))
                                  (appC (idC '-) (list (numC 2) (idC 'x))))
                            (list (numC 1))) top-env) (numV 1))
(check-equal? (interp (appC (lamC (list (idC 'x))
                                  (appC (idC '*) (list (numC 2) (idC 'x))))
                            (list (numC 1))) top-env) (numV 2))
(check-equal? (interp (appC (lamC (list (idC 'x))
                                  (appC (idC '/) (list (numC 2) (idC 'x))))
                            (list (numC 1))) top-env) (numV 2))
(check-equal? (interp (appC (idC 'equal?) (list (numC 2) (strC "hi"))) top-env)
              (boolV #f))
(check-equal? (interp (appC (idC 'equal?) (list (numC 2) (numC 2))) top-env)
              (boolV #t))
(check-equal? (interp (appC (idC 'equal?) (list
                                           (appC (idC '<=) (list (numC 2) (numC 3)))
                                           (numC 2))) top-env) (boolV #f))
(check-equal? (interp (appC (idC 'equal?) (list (lamC (list (idC 'x))
                                          (appC (idC '/) (list (numC 2) (idC 'x))))
                                                (numC 2))) top-env) (boolV #f))
(check-exn (regexp (regexp-quote "DRMG: cannot divide 2 by 0"))
           (lambda () (interp (appC (lamC (list (idC 'x))
                                          (appC (idC '/) (list (numC 2) (idC 'x))))
                                    (list (numC 0))) top-env)))
(check-equal? (interp (appC (idC 'strlen) (list (strC "hello"))) top-env) (numV 5))
(check-equal? (interp
               (appC (idC 'substring) (list (strC "hello") (numC 0) (numC 4)))
               top-env) (strV "hell"))
(check-exn (regexp (regexp-quote "DRMG: invalid use of substring"))
           (lambda () (interp
                       (appC (idC 'substring) (list (strC "hello") (numC 0) (numC 7)))
                       top-env)))
(check-exn (regexp (regexp-quote "DRMG: invalid use of 'strlen on '(\"2\")"))
           (lambda () (interp
                       (appC (idC 'strlen) (list (numC 2)))
                       top-env)))
(check-exn (regexp (regexp-quote "DRMG: \"1\" must evaluate to a boolean value"))
           (lambda () (interp (ifC (appC (idC '/) (list (numC 2) (numC 2)))
                                   (numC 2)
                                   (numC 1)) top-env)))
(check-exn (regexp (regexp-quote "DRMG: \"true\" must be a valid function"))
           (lambda () (interp (appC (idC 'true) (list (numC 2) (numC 2))) top-env)))
(check-exn (regexp (regexp-quote "DRMG: 'hi not found"))
           (lambda () (interp (appC (idC '-) (list (numC 3) (idC 'hi))) top-env)))
(check-exn (regexp (regexp-quote "DRMG: 'local is not a valid name"))
           (lambda () (interp (appC (lamC (list (idC 'local))
                                          (appC (idC '+) (list (numC 2) (idC 'local))))
                                    (list (numC 1))) top-env)))
(check-exn (regexp (regexp-quote "DRMG: '(\"2\") must have associated names"))
           (lambda () (interp (appC (lamC (list (idC 'x))
                                          (appC (idC '+) (list (numC 2) (idC 'x))))
                                    (list (numC 1) (numC 2))) top-env)))
(check-exn (regexp (regexp-quote "DRMG: '(y) must have associated values"))
           (lambda () (interp (appC (lamC (list (idC 'x) (idC 'y))
                                          (appC (idC '+) (list (idC 'x) (idC 'y))))
                                    (list (numC 1))) top-env)))
(check-exn (regexp (regexp-quote "DRMG: user-error in \"1\""))
           (lambda () (interp (appC (idC 'error) (list (numC 1))) top-env)))

;; '{{% a -> {a 1 2}} {% a b -> {+ a b}}}
(define tree (appC (lamC (list (idC 'a)) (appC (idC 'a) (list (numC 1) (numC 2))))
                   (list (lamC (list (idC 'a) (idC 'b)) (appC (idC '+) (list (idC 'a) (idC 'b)))))))
(check-equal? (interp tree top-env) (numV 3))

#; '{local
    [fact = {% self n -> {if {<= n 0}
                             1
                             {* {self self {- n 1}} n}}}]
    in
    {fact fact 12}}
(define recurs (appC (lamC (list (idC 'fact))
                           (appC (idC 'fact)
                                 (list (idC 'fact) (numC 12))))
                     (list (lamC (list (idC 'self) (idC 'n))
                                 (ifC (appC (idC '<=) (list (idC 'n) (numC 0)))
                                      (numC 1)
                                      (appC (idC '*)
                                            (list (appC (idC 'self)
                                                        (list (idC 'self)
                                                              (appC (idC '-)
                                                                    (list (idC 'n) (numC 1)))))
                                                  (idC 'n))))))))
(check-equal? (interp recurs top-env) (numV 479001600))

;; top-interp
;; calls parse and interp on a given program
;; returns a string of the included ExprC's and the final result
#;(define (top-interp [s : Sexp]) : String
    (serialize (interp (parse s) top-env)))