 #lang eopl

; TAD en listas

; EXPRESION := CONST(numero)
;            | VAR(variable)
;            | ADD(EXPRESION, EXPRESION)
;            | SUB(EXPRESION, EXPRESION)
;            | MUL(EXPRESION, EXPRESION)

; Que es equivalente a esta gramatica

; <EXPRESION> := (const numero)
;              | (var variable)
;              | (add <EXPRESION> <EXPRESION>)
;              | (sub <EXPRESION> <EXPRESION>)
;              | (mul <EXPRESION> <EXPRESION>)

; ========================
;         INTERFAZ 
; ========================

; ================
; 1. CONSTRUCTORES 
; ================

;  ** Idea ** la expresion esta definida en lista
; const : Int -> Expresion
; var: Symbol Schema -> Expresion
; add: Expresion x Expresion -> Expresion
; sub : Expresion x Expresion -> Expresion
; mul ::Expresion x Expresion -> Expresion

; Constantes | int -> exp
(define const
  (lambda (numero)
    (list 'const numero)))

; Variable | var -> exp
(define var
  (lambda (variable)
    (list 'var variable)))

; Add | exp X exp -> exp
(define add 
  (lambda (exp_1 exp_2)
    (list 'add exp_1 exp_2)))

; Sub | exp X exp -> exp
(define sub
  (lambda (exp_1 exp_2)
    (list 'sub exp_1 exp_2)))

; Mul | exp X exp -> exp
(define mul
  (lambda (exp_1 exp_2)
    (list 'mul exp_1 exp_2)))

; ===============
; 2. OBSERVADORES
; ===============

; ==============
; 2.1 PREDICADOS
; ==============

; Constantes | exp -> boolean
(define const?
  (lambda (l)
    (eqv? (car l) 'const)))

; Variable | exp -> boolean
(define var?
  (lambda (l)
    (eqv? (car l) 'var)))

; Add | exp -> boolean
(define add?
  (lambda (l)
    (eqv? (car l) 'add)))

; Sub | exp -> boolean
(define sub?
  (lambda (l)
    (eqv? (car l) 'sub)))

; Mul | exp -> boolean
(define mul?
  (lambda (l)
    (eqv? (car l) 'mul)))

; ===============
; 2.2 EXTRACTORES
; ===============

; Constantes | exp -> int
(define const->value
  (lambda (l)
    (cadr l)))

; Variable | exp -> var
(define var->value
  (lambda (l)
    (cadr l)))

; Suma - Izquierda | add -> exp
(define add->left
  (lambda (l)
    (cadr l)))

; Suma - Derecha | add -> exp
(define add->right
  (lambda (l)
    (caddr l)))

; Sub - Izquierda | add -> exp
(define sub->left
  (lambda (l)
    (cadr l)))

; Sub - Derecha | add -> exp
(define sub->right
  (lambda (l)
    (caddr l)))
; Mul - Izquierda | add -> exp
(define mul->left
  (lambda (l)
    (cadr l)))

; Mul - Derecha | add -> exp
(define mul->right
  (lambda (l)
    (caddr l)))

; ========================
;      IMPLEMENTACION 
; ========================

(define expresion
  (lambda (l)
    (cond
      [(const? l) (const->value l)]
      [(var? l) (var->value l)]
      [(add? l) (list (expresion (add->left l)) '+ (expresion (add->right l)))]
      [(sub? l) (list (expresion (sub->left l)) '- (expresion (sub->right l)))]
      [(mul? l) (list (expresion (mul->left l)) '* (expresion (mul->right l)))]
      [else 'jumm]
      )))

; ========
; EJEMPLOS
; ========

; 1. Creación de constantes

(const 5)
; (const 5)

; 2. Creación de variables

(var 'x)
; (var x)

; 3. Creación de sumas

(add (const 5) (var 'x))
; (add (const 5) (var x))

; 4. Creación de restas

(sub (const 8) (var 'y))
; (sub (const 8) (var 'y))

; 5. Creación de multiplicaciones

(mul (const 6) (var 'z))
; (mul (const 6) (var z))


; 6. La construccion de expresiones anidadas

(expresion (sub (const 3) (mul (const 1)(var 'x))))
; (3 - (1 * x))

; 7. La construccion de almenos dos expresiones no triviales

(expresion (add (mul (const 1)(var 'x)) (sub (var 'y)(const 12))))
; ((1 * x) + (y - 12))


(expresion (sub (add
                  (mul (const 3)(var 'w))
                  (sub (var 'y)(const 89)))
                (mul (const 3) (var 'p))))
;(((3 * w) + (y - 89)) - (3 * p))

; Declaración de IA
; * Para el predicado Constante, se uso la IA para verificar que predicado evalua si es
;   entero o no.
; * Profundizar el tema de TAD, dado el ejemplo de SumaAnidada.




;---------------------------------------------------------------------------
;-----------------------------------
; Implementacion mediante Datatype  |
;-----------------------------------


;Constructores y predicados van juntos cuando creamos el datatype
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

; Extractores
(define calcular
  (lambda (expr)
    (cases expresion expr

      ;Casos de la gramatica
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

; Pruebas:
;
;1. ((x + 99) * (7 - y))
; #(struct:mul #(struct:add #(struct:var x) #(struct:const 99))
;              #(struct:sub #(struct:const 7) #(struct:var y)))
(mul (add (var 'x) (const 99))
     (sub (const 7) (var 'y)))

;2. (((3 * w) + (y - 89)) - (3 * p))
;#(struct:sub #(struct:add #(struct:mul #(struct:const 3) #(struct:var w)) #(struct:sub #(struct:var y) #(struct:const 89)))
;             #(struct:mul #(struct:const 3) #(struct:var p)))
(sub (add
         (mul (const 3)(var 'w))
         (sub (var 'y)(const 89)))
     (mul (const 3) (var 'p)))

;3. ((((7 * m) + (x + 8)) - (2 * t)) * v)
; #(struct:mul
;  #(struct:sub #(struct:add #(struct:mul #(struct:const 7) #(struct:var m)) #(struct:add #(struct:var x) #(struct:const 8))) #(struct:mul #(struct:const 2) #(struct:var t)))
;  #(struct:var v))
(mul
 (sub (add
         (mul (const 7)(var 'm))
         (add (var 'x)(const 8)))
     (mul (const 2) (var 't)))
 (var 'v))
     




