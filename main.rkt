#lang nanopass

(module+ test
  (require rackunit
           quickcheck
           racket/match))

(define (variable? x)
  (symbol? x))

(define (primitive? x)
  (memq x '(read + -)))

(define (integer? x)
  (fixnum? x))

(define-language R1
  (entry Expr)
  (terminals 
    (integer (int))
    (variable (var))
    (primitive (prim)))
  (Expr (e) 
    int
    var
    prim
    (e0 e* ...)
    (let ([var e0]) e1)))

(define-parser parse-R1 R1)

(define-pass uniquify : R1 (ir) -> R1 ()
  (Expr : Expr (ir env) -> Expr (env)
    [,int (values int env)]
    [,prim (values prim env)]
    [,var (begin
            (define var^ (hash-ref env var))
            (unless var^ (error 'uniquify "unbound identifier ~a" var)) 
            (values var^ env))] 
    [(,e0 ,e* ...) 
     (begin
      (define (f e) 
        (define-values (ir^ _) (Expr e env))
        ir^)
      (define e*^ (map f e*))

      (values `(,e0 ,e*^ ...) env))]
    [(let ([,var ,e0]) ,e1) 
     (define var^ (gensym var))
     (define-values (e0^ _e0^-env) (Expr e0 env))
     (define-values (e1^ _e1^-env) (Expr e1 (hash-set env var var^)))

     (values `(let ([,var^ ,e0^]) ,e1^) env)]) 
  (let-values ([(ir^ env) (Expr ir (hash))])
    ir^))
 
(module+ test
  (test-case 
    "it should rename variables in let expression bodies to be the same as their declarations"
    (begin
      (define input (parse-R1 '(let ([x 1]) x)))
      (define output (unparse-R1 (uniquify input)))

      (match-define 
        `(let ([,x0 1]) ,x1) output) 
        
      (check-equal? x0 x1))))
