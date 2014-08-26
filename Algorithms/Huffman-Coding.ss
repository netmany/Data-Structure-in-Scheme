; Greedy-Algorithm, Huffman-Coding
; 哈夫曼编码
; =================================================================================
> (huff-code (vector 10 5 8))
10 => 0
5 => 10
8 => 11
> (huff-code (vector 1 1 1 1))
1 => 00
1 => 01
1 => 10
1 => 11
> (huff-code (vector 0.45 0.13 0.12 0.16 0.09 0.05))
0.45 => 0
0.12 => 100
0.13 => 101
0.05 => 1100
0.09 => 1101
0.16 => 111

; ========================================================================================
(define (huff-code v)
  (define (heap-vector v less?)
    (define len (vector-length v))
    (define h (heap len less?))
    (let t ((i 0))
      (if (< i len)
          (begin
            (heap-in h (list (vector-ref v i)))
            (t (+ i 1)))))
    h)
  
  (let ((h (heap-vector v (lambda (p q) (< (car p) (car q))))))
    (let t ()
      (if (> (heap-size h) 1)
          (let ((p (heap-out h))
                (q (heap-out h)))
            (heap-in h (list (+ (car p) (car q)) p q))
            (t))))
    (let t ((prefix "") (z (heap-out h)))
      (if (null? (cdr z))
          (printf "~a => ~a\n" (car z) prefix)
          (begin
            (t (string-append prefix "0") (cadr z))
            (t (string-append prefix "1") (caddr z)))))))

; heap, priority-queue
; =================================================================
(define (heap len less?) (list (make-vector (+ 1 len)) less?))
(define (heap-size h) (vector-ref (car h) 0))
(define (heap-capacity h) (- (vector-length (car h)) 1))
(define (heap-empty? h) (= 0 (heap-size h)))
(define (heap-head h) (vector-ref (car h) 1))

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
      #f
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
