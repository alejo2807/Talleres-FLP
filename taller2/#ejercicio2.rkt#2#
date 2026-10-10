#lang eopl

;Ejercicio2.


; Parse para expresion basada en listas.
; idea : (3 + x) => (add (const 3) (var 'x))

(define PARSE-LT
  (lambda (expr)
    (cond
      [(integer? expr) (const expr)]
      [(symbol? expr) (var expr)]
      [(eqv? '+ (cadr expr)) (add (PARSE-LT (car expr)) (PARSE-LT (caddr expr)))]
      [(eqv? '- (cadr expr)) (sub (PARSE-LT (car expr)) (PARSE-LT (caddr expr)))]
      [(eqv? '* (cadr expr)) (sub (PARSE-LT (car expr)) (PARSE-LT (caddr expr)))]
      [else 'error]
    )))

; Unparse para expresion basada en listas.
; idea : (add (const 3) (var 'x)) => (3 + x)

(define UNPARSE-LT
  (lambda (expr)
    (cond
      [(const? expr) (const->value expr)]
      [(var? expr) (var->value expr)]
      [(add? expr) (list (UNPARSE-LT (add->left expr)) '+ (UNPARSE-LT (add->right expr)))]
      [(sub? expr) (list (UNPARSE-LT (sub->left expr)) '- (UNPARSE-LT (sub->right expr)))]
      [(mul? expr) (list (UNPARSE-LT (mul->left expr)) '* (UNPARSE-LT (mul->right expr)))]
      [else 'error]
    )))

; Parse para datatype expresion
(define PARSE-DT
  (lambda(expr)
    (cond
      [(integer? expr) (const expr)]
      [(symbol? expr) (var expr)]
      [(equal? (cadr expr) '+)             ; Ejemplo: '(1 + x) ->1 car, +->cadr, x->caddr
          (add (PARSE-DT (car expr))
               (PARSE-DT (caddr expr)))]
      [(equal? (cadr expr) '-)            
          (sub (PARSE-DT (car expr))
               (PARSE-DT (caddr expr)))]
      [(equal? (cadr expr) '*)            
          (mul (PARSE-DT (car expr))
               (PARSE-DT (caddr expr)))]
      )
    )
  )


; Pruebas PARSE-DT:
(PARSE-DT '((3 + x) * (4 - y)))
(PARSE-DT '(q * ((2 - w) * (4 - y))))
(PARSE-DT '(b - (t - ((2 + e) * (4 - k)))))
(PARSE-DT '((z - 4) * (u - ((2 + r) * (4 + p)))))
(PARSE-DT '((11 - w) + (h - 3)))

; Unparse para datatype expresion
(define UNPARSE-DT
  (lambda (arb)
    (cases expresion arb
      [const (numero) numero]
      [var (variable) variable]
      [add (expresionIzq expresionDer)
           (list (UNPARSE-DT expresionIzq) '+ (UNPARSE-DT expresionDer))]
      [sub (expresionIzq expresionDer)
           (list (UNPARSE-DT expresionIzq) '- (UNPARSE-DT expresionDer))]
      [mul (expresionIzq expresionDer)
           (list (UNPARSE-DT expresionIzq) '* (UNPARSE-DT expresionDer))]      
      
    )
  ) 
)

; Pruebas UNPARSER-DT:
(UNPARSE-DT (PARSE-DT '((z + 45) + (3 + X))))
(UNPARSE-DT (PARSE-DT '(q * ((2 - w) * (4 - y)))))
(UNPARSE-DT (PARSE-DT '(b - (t - ((2 + e) * (4 - k))))))
(UNPARSE-DT (PARSE-DT '((z - 4) * (u - ((2 + r) * (4 + p))))))
(UNPARSE-DT (PARSE-DT '((11 - w) + (h - 3))))

;Declaracion de uso de IA: Se utilizo IA para explicar un poco mas los conceptos del parse y el unparse.


