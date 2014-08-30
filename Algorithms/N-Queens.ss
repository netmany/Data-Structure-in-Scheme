; ==========================================================
> (Queens 8)
1:      #(0 4 7 5 2 6 1 3)
2:      #(0 5 7 2 6 3 1 4)
....
91:     #(7 2 0 5 1 4 6 3)
92:     #(7 3 0 2 5 1 6 4)
total = 92

; N-Queen layout
; =============================================================
(define (Queens n)
  (let ((count 0) (layout (make-vector n)))
    (define (valid? col row)
      (let p ((i 0) (res #t))
        (if (and res (< i col))
            (let ((diff (abs (- row (vector-ref layout i)))))
              (p (+ i 1) (and res (not (or (= diff 0) (= diff (- col i)))))))
            res)))
    (let t ((col 0) (row 0))
      (if (= n col)
          (begin
            (set! count (+ count 1))
            (printf "~a:\t~a\n" count layout)))
      (cond 
        ((or (= n col) (= n row))
         (if (> col 0)
             (t (- col 1) (+ 1 (vector-ref layout (- col 1))))))
        (else
          (if (valid? col row)
              (begin
                (vector-set! layout col row)
                (t (+ col 1) 0))
              (t col (+ row 1))))))
    (printf "total = ~a\n" count)))

