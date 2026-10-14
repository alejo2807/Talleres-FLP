#lang eopl

;; Declaración de uso de IA:
;; Se utilizó IA para: aclarar conceptos teóricos (recursión estructural, ambientes,
;; define-datatype y cases), ver un ejemplo ilustrativo distinto al del taller,
;; comprender mensajes de error del intérprete y revisar la redacción de los comentarios.
;; Se declara que todos los integrantes comprenden y pueden explicar
;; completamente cada solución implementada.

;; EJERCICIO 3: Evaluación de expresiones y ambientes

;; ============================================================
;; GRAMÁTICAS
;; ============================================================

;; Expresión (sintaxis abstracta):
;; <expresion> ::= (const <integer>)
;;               | (var <symbol>)
;;               | (add <expresion> <expresion>)
;;               | (sub <expresion> <expresion>)
;;               | (mul <expresion> <expresion>)

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
;(position 'w '(x y z))   error: se deja comentada porque detiene el programa

;; ============================================================
;; TIPOS DE DATOS
;; ============================================================

;; expresion: tipo de dato para expresiones aritméticas en sintaxis abstracta
;; (ver gramática arriba). Constructores: const, var, add, sub, mul.
;; Predicado: expresion?
(define-datatype expresion expresion?
  (const (numero integer?))
  (var (variable symbol?))
  (add (expresionIzq expresion?) (expresionDer expresion?))
  (sub (expresionIzq expresion?) (expresionDer expresion?))
  (mul (expresionIzq expresion?) (expresionDer expresion?)))

;; Pruebas expresion
;(expresion? (const 5))                       #t
;(expresion? (add (const 1) (var 'x)))        #t
;(expresion? '(const 5))                      #f (una lista no es un datatype)

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
;(apply-env ambiente 'y)                                       10
;(apply-env (extend-env '(w) '(7) ambiente) 'x)                5 (lo encuentra en el nivel de abajo)
;(apply-env (extend-env '(x) '(99) ambiente) 'x)               99 (el nivel de arriba tapa al de abajo)
;(apply-env ambiente 'q)  ; error: variable inexistente, se deja comentada

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
;(EVALUAR (const 7) ambiente)                           7
;(EVALUAR (var 'y) ambiente)                            10
;(EVALUAR (add (const 4) (var 'x)) ambiente)            9
;(EVALUAR (sub (var 'y) (const 3)) ambiente)            7
;(EVALUAR (mul (var 'z) (var 'x)) ambiente)             10
;(EVALUAR (mul (mul (const 3) (var 'x))
;              (sub (var 'y) (const 2)))
;         ambiente)                                     120

;; ============================================================
;; PARSE-DT (necesario para la prueba oficial del enunciado)
;; ============================================================

;; Aquí se incluye solo para poder probar EVALUAR como pide el
;;enunciado la versión del grupo es la del ejercicio 2.
(define PARSE-DT
  (lambda (expr)
    (cond
      [(integer? expr) (const expr)]
      [(symbol? expr) (var expr)]
      [(equal? (cadr expr) '+)
       (add (PARSE-DT (car expr)) (PARSE-DT (caddr expr)))]
      [(equal? (cadr expr) '-)
       (sub (PARSE-DT (car expr)) (PARSE-DT (caddr expr)))]
      [(equal? (cadr expr) '*)
       (mul (PARSE-DT (car expr)) (PARSE-DT (caddr expr)))])))

;; Pruebas con PARSE-DT (ejemplo del enunciado y otro propio)
;(EVALUAR (PARSE-DT '((3 * x) * (y - 2))) ambiente)     120
;(EVALUAR (PARSE-DT '((x + 4) - (z * 3))) ambiente)     3