
; Tree, 树, 嵌套链表
; =====================================================================================

(define tp (tree 'type))                       ; tp = ('type)
(tree-add tp 'unkown)                          ; tp = ('type 'unkown)
(tree-add tp (tree 'int))                      ; tp = ('type 'unkown ('int))
(tree-add tp (tree 'float))                    ; tp = ('type 'unkown ('int) ('float))
(tree-add tp (tree 'bool))                     ; tp = ('type 'unkown ('int) ('float) ('bool))
(tree-add tp (tree 'string))                   ; tp = ('type 'unkown ('int) ('float) ('bool) ('string))
(tree-root tp)                                 ; 'type
(tree-childs tp)                               ; ('unkown ('int) ('float) ('bool) ('string))
(tree? tp)                                     ; #t                           
(tree-empty? tp)                               ; #f
(tree-add (list-ref (tree-childs tp) 1) 
          10)                                  ; tp = ('type 'unkown ('int 10) ('float) ('bool) ('string))
(tree-add (list-ref (tree-childs tp) 1) 
          12)                                  ; tp = ('type 'unkown ('int 10 12) ('float) ('bool) ('string))
(tree-add (list-ref (tree-childs tp) 3)
          #t)                                  ; tp = ('type 'unkown ('int 10 12) ('float) ('bool #t) ('string))
(tree-remove tp 3)                             ; tp = ('type 'unkown ('int 10 12) ('float) ('string))


; ======================================================================================

(define tree list)

(define (tree-add t e)
  (if (null? (cdr t))
      (set-cdr! t (list e))
      (tree-add (cdr t) e)))

(define tree-root car)
(define tree-childs cdr)
(define tree? pair?)

(define (tree-empty? t)
  (and (tree? t)
       (null? (tree-childs t))))

(define (tree-remove t i)
  (let ((c (tree-childs t)))
    (if (null? c)
        #f
        (if (zero? i)
            (set-cdr! t (cdr c))
            (if (< i (length c))
                (let rm ((d c) (j i))
                  (if (= 1 j)
                      (set-cdr! d (cdr (cdr d)))
                      (rm (cdr d) (- j 1))))
                #f)))))
