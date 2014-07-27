
; Binary-search， 二分查找
======================================================================================
(binary-search (vector 11 12 20 23 30 34) 23)  ; 3

(define (binary-search v e)
  (let search ((start 0) 
               (end (- (vector-length v) 1)))
    (if (> start end)
        #f
        (let* ((mid (div (+ start end) 2))
               (piv (vector-ref v mid)))
          (cond
            ((> e piv) (search (+ mid 1) end))
            ((< e piv) (search start (- mid 1)))
            (else mid))))))

