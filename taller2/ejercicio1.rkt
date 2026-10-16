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
      [(eqv? '* (cadr expr)) (mul (PARSE-LT (car expr)) (PARSE-LT (caddr expr)))]
      [else 'error]
    )))

; Pruebas
(PARSE-LT '(3 + x))
; return: (add (const 3) (var x))

(PARSE-LT '((2 * 3) + (x - y)))
; return: (add (mul (const 2) (const 3)) (sub (var x) (var y)))

(PARSE-LT '(((x * 3) + (x - y)) + (14 + y)))
; return: ((add
;             (add (mul (var x) (const 3)) (sub (var x) (var y)))
;             (add (const 14) (var y)))

(PARSE-LT '((2 * z) + (((x * 12) + (x - z)) + (14 + y))))
; return (add
;            (mul (const 2) (var z))
;            (add
;                 (add (mul (var x) (const 12)) (sub (var x) (var z)))
;                 (add (const 14) (var y))))

(PARSE-LT '((3 * 4) / (2 * 3)))
; return: error

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

; Pruebas
(UNPARSE-LT '(add (const 3) (var x)))
; return: (3 + x)

(UNPARSE-LT '(add (sub (const 2) (const 3)) (sub (var x) (var y))))
; return: ((2 - 3) + (x - y))

(UNPARSE-LT '(add
                 (add (sub (var x) (const 3)) (sub (var x) (var y)))
                 (add (const 14) (var y))))
; return: (((x - 3) + (x - y)) + (14 + y))

(UNPARSE-LT '(add
            (mul (const 2) (var z))
            (add
                 (add (mul (var x) (const 12)) (sub (var x) (var z)))
                 (add (const 14) (var y)))))
; return: ((2 * z) + (((x * 12) + (x - z)) + (14 + y)))

(UNPARSE-LT '(add (3) (var x)))
; return: (error + x)

;---------------------------------------------------------------------------
;-----------------------------------
; Implementacion mediante Datatype  |
;-----------------------------------

;Constructores y predicados van juntos cuando creamos el datatype
;(define-datatype expresion expresion?
;  (const (numero integer?))
;  (var (variable symbol?))
;  (add (expresionIzq expresion?)
;       (expresionDer expresion?))
;  (sub (expresionIzq expresion?)
;       (expresionDer expresion?))
;  (mul (expresionIzq expresion?)
;       (expresionDer expresion?))
;  
;  )

#|
; Extractores
(define calcular
  (lambda (expr)
    (cases expresion expr

      ; Casos de la gramatica
      (const (numero) numero) ;cte
      (var (variable) variable) ;var

      (add (expresionIzq expresionDer) ;suma 
           (+ (calcular expresionIzq)
              (calcular expresionDer)))
      
      (sub (expresionIzq expresionDer) ;resta
           (- (calcular expresionIzq)
              (calcular expresionDer)))
      
      (mul (expresionIzq expresionDer) ;multiplicacion
           (* (calcular expresionIzq)
              (calcular expresionDer)))
      
      (else 'operacion_incorrecta)
      )
    )
  )

; Pruebas Implementacion Datatype:
(mul (add (var 'x) (const 99))
     (sub (const 7) (var 'y)))

;2. (((3 * w) + (y - 89)) - (3 * p))
(sub (add
         (mul (const 3)(var 'w))
         (sub (var 'y)(const 89)))
     (mul (const 3) (var 'p)))

;3. ((((7 * m) + (x + 8)) - (2 * t)) * v)

(mul
 (sub (add
         (mul (const 7)(var 'm))
         (add (var 'x)(const 8)))
     (mul (const 2) (var 't)))
 (var 'v))


|#

