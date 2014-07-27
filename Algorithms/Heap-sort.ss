; ===============================================================================
(heap-sort (vector 2 5 4 10 3 32 23 44 11 55 23 12))
; #(55 44 32 23 23 12 11 10 5 4 3 2)

; ===============================================================================
(define (heap-sort v)
  (define len (vector-length v))
  (let t ((i (div len 2)))
    (if (> i 0)
        (begin 
          (sift-down v i len)
          (t (- i 1)))))
  (let t ((i len))
    (if (> i 1)
        (let ((head (vector-ref v 0)))
          (vector-set! v 0 (vector-ref v (- i 1)))
          (vector-set! v (- i 1) head)
          (sift-down v 1 (- i 1))
          (t (- i 1)))))
  v)

(define (sift-down h start end)
  (let t ((i start))
    (let* ((vi (vector-ref h (- i 1)))
           (l (* 2 i))
           (r (+ 1 l))
           (hasLeft (<= l end))
           (hasRight (<= r end)))
      (if (and hasLeft hasRight)
          (let* ((p (if (< (vector-ref h (- l 1)) 
                           (vector-ref h (- r 1)))
                        l r))
                 (vp (vector-ref h (- p 1))))
            (if (> vi vp)
                (begin
                  (vector-set! h (- i 1) vp)
                  (vector-set! h (- p 1) vi)
                  (t p))))
          (if hasLeft
              (if (> vi (vector-ref h (- l 1)))
                  (begin
                    (vector-set! h (- i 1) (vector-ref h (- l 1)))
                    (vector-set! h (- l 1) vi))))))))

