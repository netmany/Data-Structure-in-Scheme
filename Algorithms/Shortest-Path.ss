; Branch-Bound method
; find the shortest path from src to dst in graph G
; =============================================================
> (define G (make-graph 5 (list '(0 1 10) '(0 3 30) '(0 4 100)
                                '(1 2 50)
                                '(2 4 10)
                                '(3 2 20) '(3 4 60))))
> (spath G 0 4)
[0 -> 4]: (0 3 2 4), length = 60
> (spath G 2 4)
[2 -> 4]: (2 4), length = 10
> (spath G 3 4)
[3 -> 4]: (3 2 4), length = 30

; ===================================================================
(define (spath G src dst)
  (let* ((min (cons +inf.0 (list dst)))
         (less? (lambda (p q) (< (car p) (car q))))
         (h (heap (vector-length G) less?)))
    (define (heap-find h e)
      (let t ((i (heap-size h)) (res 0))
        (if (and (= 0 res) (> i 0))
            (t (- i 1) (if (= (cadr e) (cadr (heap-ref h i))) i 0))
            res)))
    (define (found? p) (= (cadr p) (cadr min)))
    (define (branches p)
      (let ((try (vector-ref G (cadr p))))
        (let t ((j (- (vector-length try) 1)) (brs (list)))
          (if (< j 0) 
              brs
              (begin
                (if (not (or (infinite? (vector-ref try j))
                             (memv j (cdr p))))
                    (t (- j 1) (cons (cons (+ (car p) (vector-ref try j))
                                           (cons j (cdr p)))
                                     brs))
                    (t (- j 1) brs)))))))
    (define (bounded? p) (less? p min))
    (let search ((p (cons 0 (list src))))
      (if (null? p)
          (if (infinite? (car min))
              (printf "no shortest path found!\n")
              (printf "[~a -> ~a]: ~a, length = ~a\n" 
                      src dst (reverse (cdr min)) (car min)))
          (begin
            (if (found? p)
                (if (bounded? p) (set! min p))
                (let t ((q (branches p)))
                  (if (not (null? q))
                      (let ((e (car q)))
                        (if (bounded? e)
                            (let ((i (heap-find h e)))  ; merge same path head
                              (if (found? e) (set! min e))
                              (if (> i 0)
                                  (if (less? e (heap-ref h i))
                                      (begin
                                        (heap-set! h i e)
                                        (sift-up (car h) i less?)))
                                  (heap-in h e))))
                        (t (cdr q))))))
            (search (heap-out h)))))))


; initialize a graph
; ===============================================================
(define (make-graph n conf)
  (let ((G (make-vector n)))
    (let t ((i 0))
      (if (< i n)
          (begin
            (vector-set! G i (make-vector n +inf.0))
            (t (+ i 1)))))
    (let t ((p conf))
      (if (not (null? p))
          (begin
            (let ((q (car p)))
              (vector-set! (vector-ref G (car q)) (cadr q) (caddr q)))
            (t (cdr p)))))
    G))

; priority-queue
; ====================================================================
(define (heap len less?) (list (make-vector (+ 1 len)) less?))
(define (heap-size h) (vector-ref (car h) 0))
(define (heap-capacity h) (- (vector-length (car h)) 1))
(define (heap-empty? h) (= 0 (heap-size h)))
(define (heap-ref h i) (vector-ref (car h) i))
(define (heap-set! h i e) (vector-set! (car h) i e))
(define (heap-head h) (heap-ref h 1))


(define (heap-in h e)
  (if (= (heap-size h) (heap-capacity h))
      #f
      (begin
        (vector-set! (car h) 0 (+ 1 (heap-size h)))
        (vector-set! (car h) (heap-size h) e)
        (sift-up (car h) (heap-size h) (cadr h)))))

(define (sift-up hq len less?)
  (let t ((i len))
    (if (> i 1)
        (let* ((p (div i 2))
               (vi (vector-ref hq i))
               (vp (vector-ref hq p)))
          (if (less? vi vp)
              (begin 
                (vector-set! hq p vi)
                (vector-set! hq i vp)
                (t p)))))))

(define (heap-out h)
  (if (heap-empty? h)
      '()
      (let ((e (heap-head h)))        
        (vector-set! (car h) 1 (vector-ref (car h) (heap-size h)))
        (vector-set! (car h) 0 (- (heap-size h) 1))
        (sift-down (car h) (heap-size h) (cadr h))
        e)))

(define (sift-down hq len less?)
  (let t ((i 1))
    (let* ((vi (vector-ref hq i))
           (l (* 2 i))
           (r (+ 1 l))
           (hasLeft (<= l len))
           (hasRight (<= r len)))
      (if (and hasLeft hasRight)
          (let* ((p (if (less? (vector-ref hq l) (vector-ref hq r))
                        l r))
                 (vp (vector-ref hq p)))
            (if (less? vp vi)
                (begin
                  (vector-set! hq i vp)
                  (vector-set! hq p vi)
                  (t p))))
          (if hasLeft
              (if (less? (vector-ref hq l) vi)
                  (begin
                    (vector-set! hq i (vector-ref hq l))
                    (vector-set! hq l vi))))))))
