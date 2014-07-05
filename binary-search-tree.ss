; binary-search-tree, 二叉查找树 
; ================================================================================

(define bst (bstree 10))              ; bst = '(10)
(bstree-add bst 5)                    ; bst = '(10 '(5) '())
(bstree-add bst 17)                   ; bst = '(10 '(5) '(17))
(bstree-add bst 13)                   ; bst = '(10 '(5) '(17 '(13) '()))
(bstree-add bst 9)                    ; bst = '(10 '(5 '() '(9)) '(17 '(13) '()))
(bstree-remove bst 13)                ; bst = '(10 '(5 '() '(9)) '(17))

(bstree-root bst)                     ; 10
(bstree-left bst)                     ; '(5 '() '(9))
(bstree-right bst)                    ; '(17)
(bstree-empty? bst)                   ; #f
(bstree-leaf? (bstree-right bst))     ; #t

(bstree-traverse bst (lambda (i) (printf "i = ~d\n" i)))            
                                      ; 中序遍历每个节点，使用func处理
(bstree-find bst 9)                   ; '(9), 查找bst中是含有节点e的子树

(bstree->list bst)                    ; '(5 9 10 17) 输出bst的有序链表
(list->bstree '(10 5 17 13 9))        ; '(10 '(5 '() '(9)) '(17 '(13) '())), 从链表构建bst
(bstree-sort '(10 5 17 13 9))         ; '(5 9 10 13 17), 通过二叉树排序链表

; =================================================================
(define bstree list)
(define bstree-empty? null?)
(define (bstree-leaf? b) (null? (cdr b)))
(define bstree-root car)
(define (bstree-left b) (car (cdr b)))
(define (bstree-right b) (car (cdr (cdr b))))
(define (bstree-left-set! b l) (set-car! (cdr b) l))
(define (bstree-right-set! b r) (set-car! (cdr (cdr b)) r))

(define (dbg loc bz)
  (printf "~a/ bz = " loc) (display bz) (newline))

(define-syntax bstree-add
  (syntax-rules ()
    ((_ b e)
     (if (null? b)
         (set! b (list e))
         (let t ((bz b))
           (if (bstree-leaf? bz)
               (if (< e (car bz))
                   (set-cdr! bz (list (list e) '()))
                   (set-cdr! bz (list '() (list e))))
               (if (< e (car bz))
                   (if (null? (bstree-left bz))
                       (bstree-left-set! bz (list e))
                       (t (bstree-left bz)))
                   (if (null? (bstree-right bz))
                       (bstree-right-set! bz (list e))
                       (t (bstree-right bz))))))))))

(define (bstree-traverse b f)
  (if (not (null? b))
      (if (bstree-leaf? b)
          (f (car b))
          (begin
            (bstree-traverse (bstree-left b) f)
            (f (car b))
            (bstree-traverse (bstree-right b) f)))))

(define (bstree-find bz e)
  (let t ((b bz) (p bz) (d 'root))
    (if (null? b)
        '() 
        (cond
          ((= e (car b))
           (list b p d))
          ((< e (car b))
           (if (or (bstree-leaf? b)
                   (null? (bstree-left b)))
               '()
               (t (bstree-left b) b 'left)))
          (else
            (if (or (bstree-leaf? b)
                    (null? (bstree-right b)))
                '()
                (t (bstree-right b) b 'right)))))))

(define (checkleaf b)
  (if (and (null? (bstree-left b))
           (null? (bstree-right b)))
      (set-cdr! b '())))

(define-syntax bstree-remove
  (syntax-rules ()
    ((_ b e)
     (let ((bz (bstree-find b e)))
       (if (not (null? bz))
           (let ((base (list-ref bz 0))
                 (parent (list-ref bz 1))
                 (pos (list-ref bz 2)))
             (case pos
               ((root) (set! b (remove-root b)))
               ((left) (begin (bstree-left-set! parent (remove-root base)) 
                         (checkleaf parent)))
               ((right) (begin (bstree-right-set! parent (remove-root base)) 
                          (checkleaf parent))))))))))

(define (remove-root b)
  (if (bstree-leaf? b)
      '()
      (begin (set-car! b  
                       (if (null? (bstree-left b))
                           (adjust-min 'right (bstree-right b) b)
                           (adjust-max 'left (bstree-left b) b)))
        b)))

(define (adjust-min d b p)
  (let ((turn (case d
                ((right) bstree-right-set!)
                ((left) bstree-left-set!))))
    (if (null? (cdr b))
        (begin (turn p '()) (checkleaf p) (car b))
        (if (null? (bstree-left b))
            (begin (turn p (bstree-right b)) (car b))         
            (adjust-min 'left (bstree-left b) b)))))

(define (adjust-max d b p)
  (let ((turn (case d
                ((right) bstree-right-set!)
                ((left) bstree-left-set!))))
    (if (null? (cdr b))
        (begin (turn p '()) (checkleaf p) (car b))
        (if (null? (bstree-right b))
            (begin (turn p (bstree-left b)) (car b))         
            (adjust-max 'right (bstree-right b) b)))))

(define (bstree->list b)
  (let ((ls '())
        (tail '()))
    (bstree-traverse
      b
      (lambda (i)
        (if (null? tail)
            (begin 
              (set! ls (list i)) 
              (set! tail ls))
            (begin 
              (set-cdr! tail (list i)) 
              (set! tail (cdr tail))))))
    ls))

(define (list->bstree lst)
  (let ((b (bstree)))
    (let t ((ls lst))
      (if (not (null? ls))
          (begin
            (bstree-add b (car ls))
            (t (cdr ls)))))
    b))

(define (bstree-sort ls)
  (bstree->list (list->bstree ls)))

