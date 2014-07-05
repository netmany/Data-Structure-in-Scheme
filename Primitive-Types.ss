; Primitive types

(define a #t) ; 逻辑类型, true or false
(and a #f)    ; return #f

(define b #\B)  ; 字符

(define f 10.45) ; 浮点数, single-precision real number values
(+ f 3.45e-3)    ; 10.45345, Double, a wider floating-point size

(define i 999)  ; 整数, integral or fixed-precision values
(- i 33)        ; 966 

(define c 'APPLE) ; 符号型，可用于枚举类型, a small set of uniquely named values
(eq? c 'PEAR)     ; #f

(define name "henry") ; 字符串类型
(string=? name "jack") ; #f
(string<? name "jack") ; #t
(string-append "ab" "def") ; "abdef"
