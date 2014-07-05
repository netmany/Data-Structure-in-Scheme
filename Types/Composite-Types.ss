; Composite types
; (Sometimes also referred to as Plain old data structures.)

; Array, 数组或向量，固定长度的序列
(define v (vector 2 3 "good" #t))      
(define x (make-vector 10))            ; x 为10个0
(+ (vector-ref v 1) (vector-ref x 3))  ; 3
(vector-set! x 3 12)                   ; x = #(0 0 0 12 0 0 0 0 0 0)

; Linked list, 链表，动态长度的序列
(define res (list 2 3 "new" #t))       ; (2 3 "new" #t)
(+ 1 (list-ref res 1))                 ; 4
(car res)                              ; 2
(cdr res)                              ; (3 "new" #t)
(null? res)                            ; 判断空链表 #f

; Pair,Tuple; 二元点对, 链表是点对的扩展
(define p (cons "age" 18))             ; ("age" . 18) 
(car p)                                ; "age" 
(cdr p)                                ; 18
(set-car! p "years")                   ; p = ("years" . 18)
(set-cdr! p 20)                        ; p = ("years" . 20)
(cons 2 (cons 3 (cons "new" (cons #t (list))))) ; (list 2 3 "new" #t)

; HashTable, 哈希表，固定时间查找表
(define ht (make-eq-hashtable))
(hashtable-set! ht 'go 20)
(hashtable-ref ht 'go "not found")     ; 20
(hashtable-contains? ht 'go)           ; #t
(hashtable-delete! ht 'go)             

; Record (also called struct), 记录或结构体
(define-record-type 
  point (fields                        ; 定义二维点类型
                (mutable x)                  ; x 坐标可修改
                y))                          ; y 坐标只读

(define p (make-point 36 -17))
(point? p)                             ; #t
(point? '(cons 36 -17))                ; #f
(point-x p)                            ; 36
(point-y p)                            ; -17
(point-x-set! p (- (point-x p) 12))
(point-x p)                            ; 24

; Union, 联合体,同一字段可存放不同类型的数据
(point-x-set! p "age")                 ; Tagged union 
(point-x-set! p #t)                    ; (also called a variant, variant record, discriminated union, or disjoint union)
