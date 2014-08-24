; Dynamic Programming, 动态规划
; ====================================================
; 题目：有面值为1元、3元和5元的硬币若干枚，如何用最少的硬币凑够11元？
; Problem: min(x+y+z), 
;           sub{ x,y,z ~ +I; x+3y+5z = S; S = 11 }

; ====================================================
; min(S) = min(min(S-1), min(S-3), min(S-5))+1

;=====================================================
(define (dp-min s)
  (let ((tmp (vector 1 2 1 2 1)))
    (let t ((i 5))
      (if (or (<= s 5) 
              (= i s))
          (vector-ref tmp (mod (- s 1) 5))
          (begin
            (vector-set! tmp (mod i 5)
                         (+ 1
                            (min (vector-ref tmp (mod (- i 1) 5))
                                 (vector-ref tmp (mod (- i 3) 5))
                                 (vector-ref tmp (mod (- i 5) 5)))))
            (t (+ i 1)))))))

;========================================================
> (dp-min 5)
1
> (dp-min 6)
2
> (dp-min 11)
3
> (dp-min 112)
24
> (dp-min 121412)
24284
> (dp-min 1214123)
242825
> (dp-min 121412355)
24282471
