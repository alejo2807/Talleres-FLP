#lang eopl

;Ejercicio2.

;Declaracion de uso de IA: Se utilizo IA para explicar un poco mas
;los conceptos del parse y el unparse, porque no los tenia completamente claros.

(define-datatype expresion expresion?
  (const (numero integer?))
  (var (variable symbol?))
  (add (expresionIzq expresion?)
       (expresionDer expresion?))
  (sub (expresionIzq expresion?)
       (expresionDer expresion?))
  (mul (expresionIzq expresion?)
       (expresionDer expresion?))
  )



;------------------------------Parse para datatype expresion
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


;Pruebas PARSE-DT:
(PARSE-DT '((3 + x) * (4 - y)))
(PARSE-DT '(q * ((2 - w) * (4 - y))))
(PARSE-DT '(b - (t - ((2 + e) * (4 - k)))))
(PARSE-DT '((z - 4) * (u - ((2 + r) * (4 + p)))))
(PARSE-DT '((11 - w) + (h - 3)))

;------------------------------Unparse para datatype expresion
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

;Pruebas UNPARSER-DT:
(UNPARSE-DT (PARSE-DT '((z + 45) + (3 + X))))
(UNPARSE-DT (PARSE-DT '(q * ((2 - w) * (4 - y)))))
(UNPARSE-DT (PARSE-DT '(b - (t - ((2 + e) * (4 - k))))))
(UNPARSE-DT (PARSE-DT '((z - 4) * (u - ((2 + r) * (4 + p))))))
(UNPARSE-DT (PARSE-DT '((11 - w) + (h - 3))))



