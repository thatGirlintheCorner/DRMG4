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

;; define data types
(define-type ExprC (U numC idC strC ifC lambC appC))
(define-type Value (U numV boolV strV cloV primV))

(struct binding ([name : Symbol] [val : Value]))
(define-type Env (Listof binding))

;; ExprC type structs
(struct numC ([n : Real]) #:transparent)
(struct idC ([i : Symbol]) #:transparent)
(struct strC ([s : String]) #:transparent)
(struct ifC ([cond : ExprC] [t : ExprC] [f : ExprC]) #:transparent)
(struct lambC ([args : (Listof idC)] [body : ExprC]) #:transparent)
(struct appC ([func : lambC] [args : (Listof ExprC)]) #:transparent)

;; Value type structs
(struct numV ([n : Real]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct cloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct primV ([op : Symbol]) #:transparent)

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

;; look up binding in current environment
(define (lookup [for : symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "name not found: ~e" for)]
    [(cons (binding name val) r) (if (check-id name) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for r)])
                                     (error 'lookup "DRMG4: invalid id name: ~e" for))]))

;; serialize
;; takes in a value from DRMG4 and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(numV n) (format "~v" n)]
    [(boolV b) (if b "true" "false")]
    [(strV s) (format "~v" s)]
    [(cloV _ _ _) "#<procedure>"]
    [(primV _) "#<primop>"]))

;; extend current environment to include interpretations of arguments
(define (extend-env [a-names : (Listof Symbols)] [args : (Listof ExprC)] [curr-env : Env]) : Env
  (match a-names
    ['() curr-env]
    [(cons f r) (cons (binding f (interp (first args) curr-env))
                      (extend-env r (rest args) curr-env))]))

;; get all symbols of args from a lambC
(define (get-args [a : (Listof idC)]) : (Listof Symbol)
  (match a
    ['() '()]
    [(cons f r) (cons (idC-i f) (get-args r))]))

;; top environment
(define top-env (list
                 (binding )))

;; parse

;; interp
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    ;; numC
    [(numC n) n]
    ;; idC 
    [(idC i) (lookup i env)]
    ;; lambC
    [(lambC a b) (cloV (get-args a) b env)]
    ;; appC
    [(appC f a)
     ;; get relevant closure
     (local ([define clo-f (interp f env)])
       ;; interp body w extened env
       (interp (cloV-body clo-f)
               ;; extend current env with interpretations of args
               (extend-env (cloV-args clo-f) a (cloV-env clo-f))))]))

;; top-interp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))