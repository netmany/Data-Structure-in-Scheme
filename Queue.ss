
; Queue, 队列，先进先出(FIFO)
; ======================================================================================

(define q (queue 1 3 "name" #t))   ; q = (1 3 "name" #t)
(queue-in q 100)                   ; q = (1 3 "name" #t 100)
(queue-out q)                      ; 1, q = (3 "name" #t 100)
(queue-empty? q)                   ; #f
(queue-size q)                     ; 4
(queue-head q)                     ; 3

; ======================================================================================

(define queue list)

(define-syntax queue-in
  (syntax-rules ()
    ((_ q e) (set! q (append q (list e))))))
    
(define-syntax queue-out
  (syntax-rules ()
    ((_ q)
     (let ((e (car q)))
       (set! q (cdr q)) e))))
       
(define queue-empty? null?)
(define queue-size length)
(define queue-head car)


; Deque, 双端队列，两端皆可出入
; ============================================================================================

(define dq (deque 1 3 "price"))      ; dq = (1 3 "price")
(deque-in-left dq 10)                ; dq = (10 1 3 "price")
(deque-in-right dq 32)               ; dq = (10 1 3 "price" 32)
(deque-out-left dq)                  ; 10 , dq = (1 3 "price" 32)
(deque-out-right dq)                 ; 32 , dq = (1 3 "price")
(deque-empty? dq)                    ; #f
(deque-size dq)                      ; 3
(deque-head dq)                      ; 1
(deque-tail dq)                      ; "price"

; =============================================================================================
(define deque list)

(define-syntax deque-in-left
  (syntax-rules ()
    ((_ q e) (set! q (cons e q)))))
    
(define-syntax deque-in-right
  (syntax-rules ()
    ((_ q e) (set! q (append q (list e))))))
    
(define-syntax deque-out-left
  (syntax-rules ()
    ((_ q)
     (let ((e (car q)))
       (set! q (cdr q)) e))))
       
(define-syntax deque-out-right
  (syntax-rules ()
    ((_ q)
     (let* ((len (length q))
            (e (list-ref q (- len 1))))
       (if (> len 1)
           (set-cdr! (list-tail q (- len 2)) '())
           (set! q '()))
       e))))
       
(define deque-empty? null?)
(define deque-size length)
(define deque-head car)
(define (deque-tail q) (list-ref q (- (length q) 1)))
