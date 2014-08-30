; Quick-sort, 快速排序
;======================================================================================
(quick-sort (vector 12 23 34 20 11 30) <) ; #(11 12 20 23 30 34)
(quick-sort (vector "one" "two" "three" "too" "four") string<?) ; #("four" "one" "three" "too" "two")

;======================================================================================
(define (quick-sort v less?)
  (define (swap i j)
    (let ((s (vector-ref v i))
          (k (vector-ref v j)))
      (if (less? k s)
          (begin
            (vector-set! v i k)
            (vector-set! v j s)))))
  
  (define (partition i j)
    (let t ((p (+ i 1)) (q (- j 1)))
      (if (>= p q)
          (begin (swap i p) p)
          (if (less? (vector-ref v p) (vector-ref v i))
              (t (+ p 1) q)
              (if (not (less? (vector-ref v q) (vector-ref v i)))
                  (t p (- q 1))
                  (begin 
                    (swap p q)
                    (t (+ p 1) (- q 1))))))))
  
  (let sort ((start 0) (end (vector-length v)))
    (if (< start (- end 1))
        (let ((p (partition start end)))
          (sort start p)
          (sort p end))))
  v)

;========================================================================================
(define (quick-sort2 v less?)
  (define (swap i j)
    (if (< i j)
        (let ((r (vector-ref v i))) 
          (vector-set! v i (vector-ref v j))
          (vector-set! v j r))))
  
  (let sort ((start 0) (end (vector-length v)))
    (if (< start (- end 1))
        (let t ((p (+ start 1)) (q (+ start 1)))
          (if (< q end)
              (if (less? (vector-ref v q) (vector-ref v start))
                  (begin
                    (swap p q)
                    (t (+ p 1) (+ q 1)))
                  (t p (+ q 1)))
              (begin
                (swap start (- p 1))
                (sort start p)
                (sort p end))))))
  v)
  
