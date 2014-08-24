; Greedy-Algorithm, Huffman-Coding
; 哈夫曼编码
; =========================================================
; input = [0.45, 0.13, 0.12, 0.16, 0.09, 0.05]
; output= [0, 101, 100, 111, 1101, 1100]

(define (huff-code v)
  (define (heap-vector v)
    (define len (vector-length v))
    (define h (heap len))
    (let t ((i 0))
      (if (< i len)
          (begin
            (heap-in h (vector-ref v i))
            (t (+ i 1)))))
    h)
    
  (let ((h (heap-vector v))
        (ht (make-eqv-hashtable)))
    (let t ()
      (if (> (heap-size h) 1)
          (let ((p (heap-out h))
                (q (heap-out h)))
            (heap-in h (+ p q))
            (hashtable-set! ht (+ p q) (cons p q))           
            (t))))
    (let t ((prefix "") (z 1.0))
      (if (hashtable-contains? ht z)
          (begin
            (t (string-append prefix "0") (car (hashtable-ref ht z -1)))
            (t (string-append prefix "1") (cdr (hashtable-ref ht z -1))))
          (printf "~a => ~a\n" z prefix)))))

; ========================================================
> (huff-code (vector 0.45 0.13 0.12 0.16 0.09 0.05))
0.45 => 0
0.12 => 100
0.13 => 101
0.05 => 1100
0.09 => 1101
0.16 => 111
> (huff-code (vector 0.2 0.2 0.2 0.2 0.2))
0.2 => 00
0.2 => 01
0.2 => 10
0.2 => 110
0.2 => 111
> (huff-code (make-vector 10 0.1))
0.1 => 000
0.1 => 001
0.1 => 010
0.1 => 011
0.1 => 100
0.1 => 101
0.1 => 1100
0.1 => 1101
0.1 => 1110
0.1 => 1111      
