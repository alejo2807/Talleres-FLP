#lang eopl

; *********************************************************
; Interpretador

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          := <expresion>
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
; *********************************************************

; *********************************************************
; Especificación Léxica

(define scanner-interpretador
  '((espacio-blanco (whitespace) skip)
    (comentario (";" (arbno (not "#newline"))) skip)
    (entero-positivo (digit (arbno digit)) number)
    (entero-negativo ("-" digit (arbno digit)) number)
    (decimal-positivo (digit ("." digit arbno digit)) number) ; por revisar
    (decimal-negativo ("-" digit ("." digit arbno digit)) number) ; por revisar
    (texto (letter (arbno (or letter digit "_"))) string) ; por revisar
    (identificar (concat "@" letter (arbno letter)) symbol) ; por revisar
    ))
; *********************************************************

; *********************************************************
; Especificacion Sintáctica (Gramatica)

(define grammar-interpretador
  '((programa (expresion) un-programa)
    ))



