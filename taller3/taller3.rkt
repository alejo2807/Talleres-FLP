#lang eopl

;*******************************************************************************************
; Interpretador

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          := <expresion>(
;;                         un-program (exp)
;;  <expresion>         := <numero>
;;                         numero-lit (num
;;                      := "\""<texto>"\""
;;                         text-lit (txt)
;;                      := <identificador>
;;                         var-exp (id)
;;                      := (<expresion> <primitiva-binaria> <expresion>)
;;                         primapp-bin-exp (exp1 prim-binaria exp2)
;;                      := <primitiva-unaria> (<expresion>)
;;                         primapp-un-exp (prim-unaria exp)
;;  <primitiva-binaria> := + (primitiva-suma)
;;                      := ~ (primitiva-resta)
;;                      := / (primitiva-div)
;;                      := * (primitiva-multi)
;;                      := concat (primitiva-concat)
;;                      := > (primitiva-mayor)
;;                      := < (primitiva-menor)
;;                      := >= (primitiva-mayor-igual)
;;                      := <= (primitiva-menor-igual)
;;                      := =! (primitiva-diferente)
;;                      := == (primitiva-comparador-igual)
;;  <primitiva-unaria>  := longitud (primitiva-longitud)
;;                      := add1 (primitiva-add1)
;;                      := sub1 (primitiva-sub1)
;;                      := neg (primitiva-negacion-booleana)
;*******************************************************************************************

;*******************************************************************************************
; Especificación Léxica

; Instrucciones para crear la especificación léxica
; <scanner-spec      := ({<exp-reg-y-acción>}*)
; <exp-reg-y-acción> := (<nombre> ({<exp-reg>}*) <salida>)
; <nombre>           := <símbolo>
; <exp-reg>          := <cadena> | letter | digit | whitespace | any
;                    := (not <caracter>) | (or {<exp-reg>}*)
;                    := (arbno <exp-reg>) | (concat {<exp-reg>}*)
; <salida>           := skip | symbol | number | string

(define scanner-interpretador
  '((espacio-blanco (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)
    (decimal-positivo (digit(arbno digit) "." digit (arbno digit)) number)
    (decimal-negativo ("-" digit(arbno digit) "." digit (arbno digit)) number)
    (entero-positivo (digit (arbno digit)) number)
    (entero-negativo ("-" digit (arbno digit)) number)
    (texto (letter(arbno (or letter digit "_"))) symbol)
    (identificar ("@" letter (arbno letter)) symbol) ; por revisar
    ))
;*******************************************************************************************

;*******************************************************************************************
; Especificacion Sintáctica (Gramatica)

; Instrucciones para crear la especificación sintática
; <gramática>   := ({<producción>}*)
; <producción>  := (<lhs> ({rhs-item>}*) <nombre-prod>)
; <lhs>         := <símbolo>
; <rhs-item>    := <símbolo> | <cadena>
;               := (arbno {<rhs-item>}*)
;               := (separated-list {<rhs-item>}* <cadena>)
; <nombre-prod> := <símbolo>

; Nota: rhs = nombre de la especificación lexica si solo si esta definido en la léxica

(define grammar-interpretador
  '((programa (expresion) un-programa)

    ; Cualquier numero a numero-lit
    (expresion (entero-positivo) numero-lit)
    (expresion (entero-negativo) numero-lit)
    (expresion (decimal-positivo) numero-lit)
    (expresion (decimal-negativo) numero-lit)

    ; Variables
    (expresion (identificar) var-exp)

    ; Texto
    (expresion ("\""texto"\"") texto-lit)

    ; Los dos casos de la gramatica que nos faltaban:
    ; 1). := (<expresion> <primitiva-binaria> <expresion>) --> primapp-bin-exp (exp1 prim-binaria exp2)
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp) ;faltaban los parentesis "(" y ")" para que funcionara jajaj
    ; 2). := <primitiva-unaria> (<expresion>) --> primapp-un-exp (prim-unaria exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)

    ; Primitiva Binaria
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)
    
    ; Primitiva Uninaria
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)
    
    ))
;*******************************************************************************************

;*******************************************************************************************
; La función show-the-datatypes permite consultar las definiciones de tipos de datos
; que corresponden a la gramática.

(sllgen:make-define-datatypes scanner-interpretador grammar-interpretador)
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-interpretador grammar-interpretador)))
;*******************************************************************************************

;*******************************************************************************************
; FrontEnd => Análisis léxico (scanner) y sintáctico (parser) integrados
( define scan&parse
   ( sllgen:make-string-parser
     scanner-interpretador
     grammar-interpretador))

; Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-interpretador grammar-interpretador))

; Interpretador (FrontEnd + Evaluación + Señal para lectura)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (evaluar-programa  pgm)) ; error porque no se ha definido eval-program
    (sllgen:make-stream-parser 
      scanner-interpretador
      grammar-interpretador)))

;*******************************************************************************************

;*******************************************************************************************
;                                      Nuestro intreprete

; eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente
; dado (se inicializa dentro del programa)
(define evaluar-programa
  (lambda (pgm)
    pgm))
       ;          (eval-expression body (ambiente-inicial ))))))
       ; Falta implementar la evluacion de expresiones
;*******************************************************************************************

;*******************************************************************************************
; Creación de Ambiente

; definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-record (simbolos (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env ambiente?)))

(define scheme-value? (lambda (v) #t))

; Ambiente inicial
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (ambiente-vacio))))

; Ambiente extendido
(define ambiente-extendido 
  (lambda (simbolos vals env)
    (ambiente-extendido-record simbolos vals env)))

;Aply-env
(define buscar-variable
  (lambda (env sym)
    (cases ambiente env
      (ambiente-vacio () "Error, la variable no existe")
      (ambiente-extendido-record (simbolos vals env)
                           (let ((pos (list-find-position sym simbolos)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym)))))))
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; Prueba
(ambiente-extendido '(@a @b @c @d @e) '(1 2 3 "hola" "FLP") (ambiente-vacio))

; return:
;#(struct:ambiente-extendido-record
  ;(@a @b @c @d @e)
  ;(1 2 3 "hola" "FLP")
  ;#(struct:ambiente-vacio))
;*******************************************************************************************

;*******************************************************************************************


; * evaluar-expression
; eval-expression
; (define eval-expression
;   (lambda (exp env)
;     (cases expression exp
;       (numero-lit (numero) numero)
;       (var-exp (identificador) (apply-env env id))
;       (txt (texto) texto)
;       (primitiva-suma (any prim any)
;                       (let ((args (eval-any any env)))
;                         (aplicar-primitva prim args))))))
; 
;       (primapp-exp (prim rands)
;                    (let ((args (eval-rands rands env)))
;                      (apply-primitive prim args)))
; 

; eval-rands
;(define eval-rands
;  (lambda (rands env)
;    (map (lambda (x) (eval-rand x env)) rands)))

; eval-rand
;(define eval-rand
;  (lambda (rand env)
;    (eval-expression rand env)))
      

;apply-primitive: <primitiva> <list-of-expression> -> numero
;(define apply-primitive
  ;(lambda (prim args)
   ; (cases primitive prim
    ;  (add-prim () (+ (car args) (cadr args)))
     ; (substract-prim () (- (car args) (cadr args)))
      ;(mult-prim () (* (car args) (cadr args)))
      ;(incr-prim () (+ (car args) 1))
      ;(decr-prim () (- (car args) 1)))))

;faltantes:---------------------------
; * evaluar-primitiva-binaria
; * evaluar-primitiva-unaria 


    
;*******************************************************************************************
; Errores por solucionar (TODOs)
;*******************************************************************************************

; 1. El punto decimal suelto se ignora (No lanza error léxico)
; (just-scan "3.")
; return ((entero-positivo 3 1))
; Deberia salir error porque obligamos a poner un numero despues del punto.
; CAUSA: El scanner lee el "3" como entero-positivo, y al ver el "." (que no
;        matchea con ninguna regla que empiece con punto) simplemente lo ignora.
; SOLUCION: Agregar una regla al final del scanner que atrape el punto suelto
;           o cualquier caracter invalido y lance un error, por ejemplo:
;           (error-caracter (".") error) o (error-caracter (any) error)

; 2. El punto decimal sin numero entero se ignora
; (just-scan ".5")
; return ()
; Deberia salir error porque en nuestra gramatica exigimos un digito antes del punto.
; CAUSA: Igual que el caso anterior, el "." no matchea con ninguna regla,
;        entonces el scanner lo ignora y deja el "5" para ser escaneado.
; SOLUCION: La misma que el caso 1. Atrapar el punto suelto o caracteres invalidos.

; 3. Identificadores sin "@" se ignoran silenciosamente
; (just-scan "variable")
; return ()
; Deberia salir error porque definimos que los identificadores deben empezar con "@".
; CAUSA: La regla (identificar ("@" letter ...)) exige el "@". Como no esta,
;        la "v" no matchea con ninguna regla (no es numero, no es "@", etc.)
;        y el scanner la descarta sin avisar.
; SOLUCION: La misma. Si agregamos una regla de "error" al final, el scanner
;           atrapa la "v" y se detiene lanzando un error lexico.

; 4. Error interno de SLLGEN al escanear "- 5" (Signo separado del numero)
; (just-scan "- 5")
; Error: scanning: no actions found for ()
; CAUSA: El scanner intenta leer el "-" como entero-negativo, pero al ver el
;        espacio (whitespace) se rompe la regla ("-" digit ...). SLLGEN no sabe
;        que hacer con el "-" huerfano y lanza un error interno.
; SOLUCION: Verificar que la regla de entero-negativo exija que el "-" vaya
;           pegado al digito. Al usar un catch-all de error al final, podremos
;           interceptar este "-" y mostrar un mensaje mas amigable.
;*******************************************************************************************
; Expresiones en la gramatica por implementar
; Expresión con primitiva binaria: ( exp prim-bin exp )
; (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    
; Expresión con primitiva unaria: ( prim-un exp )
; (expresion ("(" primitiva-unaria expresion ")") primapp-un-exp)
;*******************************************************************************************

;; Declaración de IA: Se uso la IA para identificación de errores, deficiones de funciones y
;; conceptos de SLLGEN 


