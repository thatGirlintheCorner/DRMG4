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

(struct binding ([name : Symbol] [value : Value]))
(struct Environment ([bindings : (Listof binding)]) #:transparent)

;; ExprC type structs
(struct numC ([n : Real]) #:transparent)
(struct idC ([i : Symbol]) #:transparent)
(struct strC ([s : String]) #:transparent)
(struct ifC ([cond : ExprC] [t : ExprC] [f : ExprC]) #:transparent)
(struct lambC ([args : (Listof idC)] [body : ExprC]) #:transparent)
(struct appC ([name : ExprC] [params : (Listof ExprC)]) #:transparent)

;; Value type structs
(struct numV ([n : Real]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct cloV ([params : (Listof Value)] [body : ExprC] [clo-env : Environment]) #:transparent)
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

;; parse
;; (define (parse [s : Sexp]) : ExprC
;;     ... body stuff)

;; interp
;; (define (interp [e : ExprC] [env : Environment]) : Value
;;     ... body stuff)

;; top-interp
;; (define (top-interp [s : Sexp]) : String
;;     (serialize (interp (parse s) top-env)))