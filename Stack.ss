; Abstract data types

; Stack, 栈，先进后出(FILO)
; ===============================================================================================
(define s (stack 1 2 3 "name" #f))     ; 构造栈 s = (1 2 3 "name" #f)
(stk-push s 100)                       ; s = (100 1 2 3 "name" #f)
(stk-pop s)                            ; 100, s = (1 2 3 "name" #f)
(stk-empty? s)                         ; #f
(stk-top s)                            ; 1        
(stk-size s)                           ; 5

; stack 是 list的一种约定访问接口
; ================================================================================================
(define stack list)

(define-syntax stk-push
  (syntax-rules ()
    ((_ s e) (set! s (cons e s)))))
    
(define-syntax stk-pop
  (syntax-rules ()
    ((_ s) 
     (let ((e (car s)))
       (set! s (cdr s)) e))))
       
(define stk-empty? null?)
(define (stk-top s) (car s))
(define stk-size length)


; stack oop. 风格的访问接口
; ================================================================================================
(define s (stack 1 2 3 "name" #f))     ; 构造栈 s = (1 2 3 "name" #f)
(s 'push 100)                          ; s = (100 1 2 3 "name" #f)
(s 'pop)                               ; 100, s = (1 2 3 "name" #f)
(s 'empty?)                            ; #f
(s 'top)                               ; 1        
(s 'size)                              ; 5

; =================================================================================================
(define stack
  (lambda ls
    (lambda (msg . args)
      (cond   
        ((eqv? msg 'push)
         (set! ls (cons (car args) ls)))
        ((eqv? msg 'pop)
         (let ((e (car ls)))
           (set! ls (cdr ls)) e))
        ((eqv? msg 'empty?) (null? ls))
        ((eqv? msg 'top) (car ls))
        ((eqv? msg 'size) (length ls))
        (else "oops")))))

