
; List, 链表
;=================================================================================

(define l (list 2 3 4 6 7))                            ; l = (2 3 4 6 7)
(list-length l)                                        ; 5
(list-refz l 3)                                        ; 6
(list-set! l 3 11)                                     ; l = (2 3 4 11 7)
(list-insert l 2 13)                                   ; l = (2 3 13 4 11 7)
(list-remove l 4)                                      ; l = (2 3 13 11 7)
(list-append l 10)                                     ; l = (2 3 13 11 7 10)
(list-head l)                                          ; 2
(list-sub l 3)                                         ; (11 7 10)
(list-empty? l)                                        ; #f
(list-contain l 7)                                     ; (7 10)         
(list? l)                                              ; #t
(set! l (list-sort l))                                 ; (2 3 7 10 11 13)，MergeSort list, 归并排序
(list-traverse l (lambda (i) (printf "i = ~d\n" i)))   ; 顺序遍历链表


; =================================================================================

(define list-length length)
(define list-empty? null?)

(define (list-refz l i)
  (if (not (null? l))
      (if (= 0 i)
          (car l)
          (list-refz (cdr l) (- i 1)))))

(define (list-set! l i e)
  (if (not (null? l))
      (if (= 0 i)
          (set-car! l e)
          (list-set! (cdr l) (- i 1) e))))

(define-syntax list-insert
  (syntax-rules () 
    ((_ l i e)
     (if (null? l)
         (set! l (cons e '()))
         (let t ((j i) (ls l))
           (if (not (null? ls))
               (if (= 0 j)
                   (let ((p (car ls)))
                     (set-car! ls e)
                     (set-cdr! ls (cons p (cdr ls))))
                   (t (- j 1) (cdr ls)))))))))

(define-syntax list-remove
  (syntax-rules ()
    ((_ l i)
     (if (not (null? l))
         (if (null? (cdr l))
             (if (= 0 i)
                 (set! l '()))
             (let t ((j i) (ls l) (p l))
               (if (not (null? ls))
                   (if (= 0 j)
                       (if (null? (cdr ls))
                           (set-cdr! p '())
                           (begin
                             (set-car! ls (car (cdr ls)))
                             (set-cdr! ls (cdr (cdr ls)))))
                       (t (- j 1) (cdr ls) ls)))))))))

(define-syntax list-append
  (syntax-rules ()
    ((_ l e)
     (if (null? l)
         (set! l (list e))
         (let t ((ls l))
           (if (null? (cdr ls))
               (set-cdr! ls (list e))
               (t (cdr ls))))))))

(define list-head car)

(define (list-sub l i)
  (let t ((j i) (ls l))
    (if (= 0 j)
        ls
        (t (- j 1) (cdr ls)))))

(define (list-contain l e)
  (let t ((ls l))
    (if (null? ls)
        '()
        (if (= e (car ls))
            ls
            (t (cdr ls))))))

(define (list-sort l)

  (define (merge a b)
    (if (null? a)
        b
        (if (null? b)
            a
            (if (< (car a) (car b))
                (cons (car a) (merge (cdr a) b))
                (cons (car b) (merge a (cdr b)))))))
                
  (define (half l)
    (if (or (null? l)
            (null? (cdr l)))
        '()
        (let t ((p l) (q l) (s l))
          (if (or (null? p)
                  (null? (cdr p)))
              (begin (set-cdr! s '()) q)         
              (t (cdr (cdr p)) (cdr q) q)))))
              
  (if (or (null? l)
          (null? (cdr l)))
      l
      (let ((a l) (b (half l)))
        (set! a (list-sort a))
        (set! b (list-sort b))
        (merge a b))))

(define (list-traverse l f)
  (let t ((ls l))
    (if (not (null? ls))
        (begin
          (f (car ls))
          (t (cdr ls))))))

