
; Heap (Priority queue) 堆, 又名优先级队列，高优先级元素先出
; =================================================================================

(define h (heap 20))
(heap-in h 2)                       ; h = [2]
(heap-in h 5)                       ; h = [2 5]
(heap-in h 4)                       ; h = [2 5 4]
(heap-in h 10)                      ; h = [2 5 4 10]
(heap-in h 3)                       ; h = [2 3 4 10 5]
(heap-out h)                        ; 2, h = [3 5 4 10]
(heap-out h)                        ; 3, h = [4 5 10]
(heap-head h)                       ; 4, h = [4 5 10]
(heap-size h)                       ; 3
(heap-capacity h)                   ; 20
(heap-empty? h)                     ; #f
(heap-sort (vector 2 5 4 10 3))     ; #(10 5 4 3 2)
 
 
; ============================================================================

(define (heap s) (make-vector (+ 1 s)))
(define (heap-size h) (vector-ref h 0))
(define (heap-capacity h) (- (vector-length h) 1))
(define (heap-empty? h) (= 0 (heap-size h)))
(define (heap-head h) (vector-ref h 1))

(define (heap-in h e)
  (if (= (heap-size h) (heap-capacity h))
      #f
      (begin
        (vector-set! h 0 (+ 1 (heap-size h)))
        (vector-set! h (heap-size h) e)
        (sift-up h (heap-size h) #t))))

(define (heap-offset i offset)
  (if offset i (- i 1)))

(define (sift-up h len s)
  (let t ((i len))
    (if (> i 1)
        (let* ((p (div i 2))
               (vi (vector-ref h (heap-offset i s)))
               (vp (vector-ref h (heap-offset p s))))
          (if (< vi vp)
              (begin 
                (vector-set! h (heap-offset p s) vi) 
                (vector-set! h (heap-offset i s) vp)
                (t p)))))))

(define (heap-out h)
  (if (heap-empty? h)
      #f
      (let ((e (heap-head h)))        
        (vector-set! h 1 (vector-ref h (heap-size h)))
        (vector-set! h 0 (- (heap-size h) 1))
        (sift-down h (heap-size h) #t)
        e)))

(define (sift-down h len s)
  (let t ((i 1))
    (let* ((vi (vector-ref h (heap-offset i s)))
           (l (* 2 i))
           (r (+ 1 l))
           (hasLeft (<= l len))
           (hasRight (<= r len)))
      (if (and hasLeft hasRight)
          (let* ((p (if (< (vector-ref h (heap-offset l s)) 
                          (vector-ref h (heap-offset r s)))
                       l r))
                (vp (vector-ref h (heap-offset p s))))
            (if (> vi vp)
                (begin
                  (vector-set! h (heap-offset i s) vp)
                  (vector-set! h (heap-offset p s) vi)
                  (t p))))
          (if hasLeft
              (if (> vi (vector-ref h (heap-offset l s)))
                  (begin
                    (vector-set! h (heap-offset i s) (vector-ref h (heap-offset l s)))
                    (vector-set! h (heap-offset l s) vi))))))))

(define (heap-sort v)
  (let t ((i 1))
    (if (<= i (vector-length v))
        (begin 
          (sift-up v i #f)
          (t (+ 1 i)))))
  (let t ((i (vector-length v)))
    (if (> i 1)
        (let ((head (vector-ref v 0)))
          (vector-set! v 0 (vector-ref v (- i 1)))
          (vector-set! v (- i 1) head)
          (sift-down v (- i 1) #f)
          (t (- i 1)))))
  v)


; in heap-sort2, heapify array through sift-down2 instead of sift-up for speed
=====================================================================================
(heap-sort2 (vector 2 5 4 10 3 32 23 44 11 55 23 12))
                                    ; #(55 44 32 23 23 12 11 10 5 4 3 2)
(define (heap-sort2 v)
  (define len (vector-length v))
  (let t ((i (div len 2)))
    (if (> i 0)
        (begin 
          (sift-down2 v i len)
          (t (- i 1)))))
  (let t ((i len))
    (if (> i 1)
        (let ((head (vector-ref v 0)))
          (vector-set! v 0 (vector-ref v (- i 1)))
          (vector-set! v (- i 1) head)
          (sift-down2 v 1 (- i 1))
          (t (- i 1)))))
  v)

(define (sift-down2 h start end)
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

