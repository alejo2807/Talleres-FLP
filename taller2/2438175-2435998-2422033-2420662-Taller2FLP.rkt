#lang eopl


;David Alejandro Garcia Grueso     2438175
;Brandon Alexis Franco Flor        2435998
;Daniel Andres Micolta Gongora     2422033
;Jorge Luis Junior Lasprilla Prada 2420662

; Declaración de uso de IA:
; Se utilizó IA para: 
; 1. aclarar conceptos teóricos (recursión estructural, ambientes,
;    define-datatype y cases).
; 2. comprender mensajes de error del intérprete.
; 3. Profundizar el tema de TAD, dado el ejemplo de SumaAnidada.
; 4. Revisar la redacción de los comentarios.
; 5. Explicar con más detalle los conceptos de parse y unparse.
; Se declara que todos los integrantes comprenden y pueden explicar
; completamente cada solución implementada.


;--------------------------------------------------------------
;                         Ejercicio 1
;--------------------------------------------------------------

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

;Nota: se agregó el sufijo "-lt" a los constructores, predicados y extractores
;      para que puedan coexistir con los del datatype y no tener errores al correr el programa.

;  ** Idea ** la expresion esta definida en lista
; const-lt : Int -> Expresion
; var-lt: Symbol Schema -> Expresion
; add-lt: Expresion x Expresion -> Expresion
; sub-lt : Expresion x Expresion -> Expresion
; mul-lt ::Expresion x Expresion -> Expresion

; Constantes | int -> exp
(define const-lt
  (lambda (numero)
    (list 'const numero)))

; Variable | var -> exp
(define var-lt
  (lambda (variable)
    (list 'var variable)))

; Add | exp X exp -> exp
(define add-lt
  (lambda (exp_1 exp_2)
    (list 'add exp_1 exp_2)))

; Sub | exp X exp -> exp
(define sub-lt
  (lambda (exp_1 exp_2)
    (list 'sub exp_1 exp_2)))

; Mul | exp X exp -> exp
(define mul-lt
  (lambda (exp_1 exp_2)
    (list 'mul exp_1 exp_2)))

; ===============
; 2. OBSERVADORES
; ===============

; ==============
; 2.1 PREDICADOS
; ==============

; Constantes | exp -> boolean
(define const-lt?
  (lambda (l)
    (eqv? (car l) 'const)))

; Variable | exp -> boolean
(define var-lt?
  (lambda (l)
    (eqv? (car l) 'var)))

; Add | exp -> boolean
(define add-lt?
  (lambda (l)
    (eqv? (car l) 'add)))

; Sub | exp -> boolean
(define sub-lt?
  (lambda (l)
    (eqv? (car l) 'sub)))

; Mul | exp -> boolean
(define mul-lt?
  (lambda (l)
    (eqv? (car l) 'mul)))

; ===============
; 2.2 EXTRACTORES
; ===============

; Constantes | expr -> int
(define const-lt->value
  (lambda (l)
    (cadr l)))

; Variable | expr -> var
(define var-lt->name
  (lambda (l)
    (cadr l)))

; Suma - Izquierda | add -> expr
(define add-lt->left
  (lambda (l)
    (cadr l)))

; Suma - Derecha | add -> expr
(define add-lt->right
  (lambda (l)
    (caddr l)))

; Resta - Izquierda | sub -> expr
(define sub-lt->left
  (lambda (l)
    (cadr l)))

; Resta - Derecha | sub -> expr
(define sub-lt->right
  (lambda (l)
    (caddr l)))

; Multiplicación - Izquierda | mul -> expr
(define mul-lt->left
  (lambda (l)
    (cadr l)))

; Multiplicación - Derecha | mul -> expr
(define mul-lt->right
  (lambda (l)
    (caddr l)))

; ========================
;      IMPLEMENTACION 
; ========================

(define expresion-lt
  (lambda (l)
    (cond
      [(const-lt? l) (const-lt->value l)]
      [(var-lt? l) (var-lt->name l)]
      [(add-lt? l) (list (expresion-lt (add-lt->left l)) '+ (expresion-lt (add-lt->right l)))]
      [(sub-lt? l) (list (expresion-lt (sub-lt->left l)) '- (expresion-lt (sub-lt->right l)))]
      [(mul-lt? l) (list (expresion-lt (mul-lt->left l)) '* (expresion-lt (mul-lt->right l)))]
      [else 'error]
      )))

; ========
; EJEMPLOS
; ========

; 1. Creación de constantes

(const-lt 5)
; (const 5)

; 2. Creación de variables

(var-lt 'x)
; (var x)

; 3. Creación de sumas

(add-lt (const-lt 5) (var-lt 'x))
; (add (const 5) (var x))

; 4. Creación de restas

(sub-lt (const-lt 8) (var-lt 'y))
; (sub (const 8) (var 'y))

; 5. Creación de multiplicaciones

(mul-lt (const-lt 6) (var-lt 'z))
; (mul (const 6) (var z))


; 6. La construccion de expresiones anidadas

(expresion-lt (sub-lt (const-lt 3) (mul-lt (const-lt 1)(var-lt 'x))))
; (3 - (1 * x))

; 7. La construccion de almenos dos expresiones no triviales

(expresion-lt (add-lt (mul-lt (const-lt 1)(var-lt 'x)) (sub-lt (var-lt 'y)(const-lt 12))))
; ((1 * x) + (y - 12))


(expresion-lt (sub-lt (add-lt
                  (mul-lt (const-lt 3)(var-lt 'w))
                  (sub-lt (var-lt 'y)(const-lt 89)))
                (mul-lt (const-lt 3) (var-lt 'p))))
;(((3 * w) + (y - 89)) - (3 * p))


; ===================================
; Implementacion mediante Datatype  
; ===================================

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

; ================================
; Pruebas Implementacion Datatype:
; ================================
;1. ((x + 99) * (7 - y)) 
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


;--------------------------------------------------------------
;                         Ejercicio 2
;--------------------------------------------------------------

; Sintaxis concreta:
; <expr-concreta> ::= <integer>
;                  | <symbol>
;                  | (<expr-concreta> + <expr-concreta>)
;                  | (<expr-concreta> - <expr-concreta>)
;                  | (<expr-concreta> * <expr-concreta>)
;
; La sintaxis abstracta es la del ejercicio 1: listas (const-lt, var-lt, ...)
; para PARSE-LT / UNPARSE-LT y datatype (const, var, ...) para PARSE-DT / UNPARSE-DT.

; ========================
;   PARSE / UNPARSE (listas)
; ========================

; PARSE-LT | expr-concreta -> exp (listas)
; Recibe una expresión concreta infija y construye su representación abstracta
; basada en listas.
; (PARSE-LT '(3 + x))  =>  (add (const 3) (var x))
(define PARSE-LT
  (lambda (exp)
    (cond
      [(integer? exp) (const-lt exp)]
      [(symbol? exp) (var-lt exp)]
      [(eqv? '+ (cadr exp)) (add-lt (PARSE-LT (car exp)) (PARSE-LT (caddr exp)))]
      [(eqv? '- (cadr exp)) (sub-lt (PARSE-LT (car exp)) (PARSE-LT (caddr exp)))]
      [(eqv? '* (cadr exp)) (mul-lt (PARSE-LT (car exp)) (PARSE-LT (caddr exp)))]
      [else (eopl:error 'PARSE-LT "Expresion concreta invalida: ~s" exp)]
    )))

; Pruebas PARSE-LT:
(PARSE-LT '(3 + x))
; return: (add (const 3) (var x))

(PARSE-LT '((2 * 3) + (x - y)))
; return: (add (mul (const 2) (const 3)) (sub (var x) (var y)))

(PARSE-LT '(((x * 3) + (x - y)) + (14 + y)))
; return: (add
;             (add (mul (var x) (const 3)) (sub (var x) (var y)))
;             (add (const 14) (var y)))

(PARSE-LT '((2 * z) + (((x * 12) + (x - z)) + (14 + y))))
; return (add
;            (mul (const 2) (var z))
;            (add
;                 (add (mul (var x) (const 12)) (sub (var x) (var z)))
;                 (add (const 14) (var y))))

;(PARSE-LT '((3 * 4) / (2 * 3)))
; return: error (se deja comentado para que no marque el error al correr el programa)



; UNPARSE-LT | expr (listas) -> expr-concreta
; Recibe una expresión abstracta basada en listas y produce su representación
; concreta infija.
; (UNPARSE-LT '(add (const 3) (var x)))  =>  (3 + x)
(define UNPARSE-LT
  (lambda (exp)
    (cond
      [(const-lt? exp) (const-lt->value exp)]
      [(var-lt? exp) (var-lt->name exp)]
      [(add-lt? exp) (list (UNPARSE-LT (add-lt->left exp)) '+ (UNPARSE-LT (add-lt->right exp)))]
      [(sub-lt? exp) (list (UNPARSE-LT (sub-lt->left exp)) '- (UNPARSE-LT (sub-lt->right exp)))]
      [(mul-lt? exp) (list (UNPARSE-LT (mul-lt->left exp)) '* (UNPARSE-LT (mul-lt->right exp)))]
      [else (eopl:error 'UNPARSE-LT "Expresion abstracta invalida: ~s" exp)]
    )))

; Pruebas UNPARSE-LT:
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

(UNPARSE-LT (PARSE-LT '((2 * z) + (((x * 12) + (x - z)) + (14 + y)))))
; return: ((2 * z) + (((x * 12) + (x - z)) + (14 + y)))

;(UNPARSE-LT '(add (3) (var x)))
; return: (error) (se deja comentado para que no marque el error al correr el programa)



; ========================
;   PARSE / UNPARSE (datatype)
; ========================

; PARSE-DT | expr-concreta -> expresion (datatype)
; Recibe una expresión concreta infija y construye su representación abstracta
; basada en el datatype expresion.
; (PARSE-DT '(3 + x))  =>  #(struct:add #(struct:const 3) #(struct:var x))
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

; UNPARSE-DT | expresion (datatype) -> expr-concreta
; Recibe una expresión abstracta basada en datatype y produce su representación
; concreta infija, como lista de Racket.
; (UNPARSE-DT (PARSE-DT '(3 + x)))  =>  (3 + x)
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

; Pruebas UNPARSE-DT:
(UNPARSE-DT (PARSE-DT '((z + 45) + (3 + X))))
(UNPARSE-DT (PARSE-DT '(q * ((2 - w) * (4 - y)))))
(UNPARSE-DT (PARSE-DT '(b - (t - ((2 + e) * (4 - k))))))
(UNPARSE-DT (PARSE-DT '((z - 4) * (u - ((2 + r) * (4 + p))))))
(UNPARSE-DT (PARSE-DT '((11 - w) + (h - 3))))



;--------------------------------------------------------------
;                         Ejercicio 3
;--------------------------------------------------------------

;; Evaluación de expresiones y ambientes
;; (usa el datatype expresion y PARSE-DT definidos arriba)

;; ============================================================
;; GRAMÁTICAS
;; ============================================================

;; Expresión (sintaxis abstracta): ver gramática del ejercicio 1.

;; Ambiente:
;; <environment> ::= (empty-env)
;;                 | (extend-env (<symbol>*) (<integer>*) <environment>)

;; ============================================================
;; FUNCIONES AUXILIARES
;; ============================================================

;; list-of: (any -> boolean) -> (any -> boolean)
;; Recibe un predicado y devuelve otro predicado que es verdadero cuando
;; el dato es una lista (posiblemente vacía) cuyos elementos cumplen el
;; predicado recibido. Se usa para validar los campos del ambiente.
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

;; Pruebas list-of
;((list-of symbol?) '(x y z))    #t
;((list-of symbol?) '(x 3 z))    #f
;((list-of integer?) '())        #t

;; is-in?: symbol x list -> boolean
;; Indica si un dato aparece dentro de una lista. Se usa en apply-env
;; para saber si una variable está en el nivel actual del ambiente.
(define is-in?
  (lambda (name lista)
    (cond
      [(null? lista) #f]
      [(eqv? name (car lista)) #t]
      [else (is-in? name (cdr lista))])))

;; Pruebas is-in?
;(is-in? 'y '(x y z))    #t
;(is-in? 'w '(x y z))    #f
;(is-in? 'x '())         #f

;; position: symbol x list -> integer
;; Devuelve la posición (empezando en 0) de una variable dentro de una lista.
;; Si no está, lanza un error. Se usa en apply-env para hallar el valor
;; que corresponde a la variable en la lista de valores.
(define position
  (lambda (variable lista)
    (cond
      [(null? lista)
       (eopl:error 'posicion "No se ha encontrado una posicion para ~s" variable)]
      [(eqv? variable (car lista)) 0]
      [else (+ 1 (position variable (cdr lista)))])))

;; Pruebas position
;(position 'x '(x y z))    0
;(position 'z '(x y z))    2
;(position 'w '(x y z))   error

;; ============================================================
;; TIPOS DE DATOS
;; ============================================================

;; environment: tipo de dato para ambientes (ver gramática arriba).
;; empty-env es el ambiente vacío; extend-env agrega una lista de variables
;; con su lista de valores sobre otro ambiente. Predicado: environment?
(define-datatype environment environment?
  (empty-env)
  (extend-env (name (list-of symbol?))
              (value (list-of integer?))
              (rest environment?)))

;; Pruebas environment
;(environment? (empty-env))                            #t
;(environment? (extend-env '(a) '(1) (empty-env)))     #t
;(environment? '(x y z))                               #f

;; ============================================================
;; AMBIENTES
;; ============================================================

;; apply-env: environment x symbol -> integer
;; Busca el valor asociado a una variable. Si está en el nivel actual lo
;; devuelve; si no, delega la búsqueda al ambiente de abajo. Si llega al
;; ambiente vacío sin encontrarla, lanza un error.
(define apply-env
  (lambda (env variable)
    (cases environment env
      (empty-env () (eopl:error 'apply-env "El valor no ha sido encontrado"))
      (extend-env (name value rest)
        (if (is-in? variable name)
            (list-ref value (position variable name))
            (apply-env rest variable))))))

;; Ambiente de ejemplo: x=5, y=10, z=2
(define ambiente
  (extend-env '(x y z) '(5 10 2)
              (empty-env)))

;; Pruebas apply-env
(apply-env ambiente 'y)                                       ; 10
(apply-env (extend-env '(w) '(7) ambiente) 'x)                ; 5 (lo encuentra en el nivel de abajo)
(apply-env (extend-env '(x) '(99) ambiente) 'x)               ; 99 (el nivel de arriba tapa al de abajo)
;(apply-env ambiente 'q)                                      ; error (se deja comentado para que no marque el error al correr el programa)

;; ============================================================
;; EVALUACIÓN
;; ============================================================

;; EVALUAR: expresion x environment -> integer
;; Evalúa una expresión en sintaxis abstracta (datatype) usando un ambiente
;; para resolver las variables. Sigue la estructura de la gramática:
;; const devuelve su número; var consulta el ambiente con apply-env;
;; add, sub y mul evalúan recursivamente sus dos lados (pasando el ambiente)
;; y combinan los resultados con +, - y *.
(define EVALUAR
  (lambda (expr ambiente)
    (cases expresion expr
      (const (numero) numero)
      (var (variable) (apply-env ambiente variable))
      (add (expresionIzq expresionDer)
           (+ (EVALUAR expresionIzq ambiente) (EVALUAR expresionDer ambiente)))
      (sub (expresionIzq expresionDer)
           (- (EVALUAR expresionIzq ambiente) (EVALUAR expresionDer ambiente)))
      (mul (expresionIzq expresionDer)
           (* (EVALUAR expresionIzq ambiente) (EVALUAR expresionDer ambiente))))))

;; Pruebas EVALUAR (construyendo la expresión con los constructores)
(EVALUAR (const 7) ambiente)                           ; 7
(EVALUAR (var 'y) ambiente)                            ; 10
(EVALUAR (add (const 4) (var 'x)) ambiente)            ; 9
(EVALUAR (sub (var 'y) (const 3)) ambiente)            ; 7
(EVALUAR (mul (var 'z) (var 'x)) ambiente)             ; 10
(EVALUAR (mul (mul (const 3) (var 'x))
              (sub (var 'y) (const 2)))
         ambiente)                                     ; 120

;; ============================================================
;; PRUEBAS FINALES (ejemplo del enunciado y otro propio)
;; ============================================================

;; Se parsea con PARSE-DT (ejercicio 2) y se evalúa con el ambiente x=5, y=10, z=2
(EVALUAR (PARSE-DT '((3 * x) * (y - 2))) ambiente)     ; 120
(EVALUAR (PARSE-DT '((x + 4) - (z * 3))) ambiente)     ; 3

