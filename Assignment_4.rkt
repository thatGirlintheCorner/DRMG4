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

;; lookup
;; look up binding in current environment
(define (lookup [for : symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "DRMG: ~e not found in environment" for)]
    [(cons (binding name val) r) (if (check-id name) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for r)])
                                     (error 'lookup "DRMG: ~e must be a valid id name" for))]))

;; serialize
;; takes in a value from DRMG4 and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(numV n) (format "~v" n)]
    [(boolV b) (if b "true" "false")]
    [(strV s) (format "~v" s)]
    [(cloV _ _ _) "#<procedure>"]
    [(primV _) "#<primop>"]))

;; extend-env
;; extend current environment to include interpretations of arguments
(define (extend-env [a-names : (Listof Symbols)] [args : (Listof ExprC)] [curr-env : Env]) : Env
  (match a-names
    ['() curr-env]
    [(cons f r) (cons (binding f (interp (first args) curr-env))
                      (extend-env r (rest args) curr-env))]))

;; get-args
;; get all symbols of args from a lamC
(define (get-args [a : (Listof idC)]) : (Listof Symbol)
  (match a
    ['() '()]
    [(cons f r) (cons (idC-i f) (get-args r))]))

;; calc-op
;; calculate output for similar operations
(define (calc-op [op : symbol] [args : (Listof ExprC)] [env : Env]) : Value
  (if (= 2 (length args))
            (local [(define a (interp (first args) env))
                    (define b (interp (first (rest args)) env))]
              (if (numV? a)
                  (if (numV? b)
                      (if (= b 0)
                          (error 'calc-op "DRMG: cannot divide by zero for ~e/~e" a b)
                          (numV (op a b)))
                      (error 'calc-op "DRMG: ~e must be applied to numbers only" op))
                  (error 'calc-op "DRMG: ~e must be applied to numbers only" op)))
            (error 'calc-op "DRMG: ~e can only take 2 arguments" op)))

;; apply-op
;; apply the primitive operations two to values
(define (apply-op [op : symbol] [args : (Listof ExprC)] [env : Env]) : Value
  (match op
    ['+ (calc-op op args env)]
    ['- (calc-op op args env)]
    ['* (calc-op op args env)]
    ['/ (calc-op op args env)]
    ['<= (calc-op op args env)]
    #;(['substring ]
    ['strlen ]
    ['equal? (if (= 2 (length args))
                 )]
    ['error ])))

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
    [(appC f a)
     (match (interp f env)
       [(cloV args body clo-env)
          ;; interp body w extened env
          (interp body
                  ;; extend current env with interpretations of args
                  (extend-env args a clo-env))]
       [(primV op) (apply-op op a env)]
       [other (error 'interp "DRMG: ~e must be a valid function" other)])]
    ;; ifC
    [(ifC c t f)
     (match (interp c env)
       [(boolV #t) (interp t env)]
       [(boolV #f) (interp f env)]
       [other (error 'interp "DRMG: ~e must evaluate to a boolean value" other)])]))

;; top-interp
;; calls parse and interp on a given program
;; returns a string of the included ExprC's and the final result
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))