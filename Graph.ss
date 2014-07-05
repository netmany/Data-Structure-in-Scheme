; Graph, 图
; ==================================================================================

; undirected graph , 无向图 
; ((0 1 2 4)                              ; (list (cons v0 (list v1 v2 v4))
;  (1 0 2)                                        (cons v1 (list v0 v2))
;  (2 0 1 3)                                      ...)
;  (3 2 4)
;  (4 1 3))

(define g (graph))
(graph-vertex-add g 0)                      ; 添加顶点
(graph-edges-add g (cons 0 1))              ; 添加边
(graph-vertex-remove g 0)                   ; 删除顶点
(graph-edges-remove g (cons 0 1))           ; 删除边
(graph-traverse g (lambda (i) (printf "v = ~d\n" i)))  
                                            ; DFS-Traverse,深度优先遍历图
(graph-vertex g)                            ; 获取图的顶点列表
(graph-edges g 0)                           ; 获取图中与顶点0相连的顶点列表
(graph-connected? g 1 3)                    ; 判断两顶点是否连通

; =====================================================================================
(define graph list)
(define (graph-vertex g) (map car g))
(define (graph-edges g v)
  (if (null? g)
      '()
      (if (= v (car (car g)))
          (cdr (car g))
          (graph-edges (cdr g) v))))

(define-syntax graph-vertex-add 
  (syntax-rules ()
    ((_ g v)
     (set! g (if (null? g)
                 (list (cons v '()))
                 (append g (list (cons v '()))))))))

(define (graph-edges-add g p)
  (if (null? g)
      #f
      (if (= (car p) (car (car g)))
          (begin
            (set-cdr! (car g) (append (cdr (car g)) (list (cdr p))))
            #t)
          (graph-edges-add (cdr g) p))))

(define (zmap f ls)
  (if (not (null? ls))
      (begin (f (car ls))
        (zmap f (cdr ls)))))

(define-syntax graph-vertex-remove
  (syntax-rules ()
    ((_ g v)
     (if (not (null? g))
         (begin 
           (set! g (filter (lambda (p) (not (= v (car p)))) g))
           (zmap (lambda (p) 
                   (set-cdr! p (filter (lambda (i) (not (= v i))) (cdr p))))
                 g))))))

(define (graph-edges-remove g e)
  (zmap (lambda (p)
          (set-cdr! p (filter 
                        (lambda (i)
                          (not (or
                                 (and (= (car p) (car e))
                                      (= i (cdr e)))
                                 (and (= (car p) (cdr e))
                                      (= i (car e))))))
                        (cdr p))))
        g))

(define (graph-traverse g f)
  (define (graph-node g i)
    (if (= i (car (car g)))
        (car g)
        (graph-node (cdr g) i)))
  (define (dfs p)
    (set-car! (cdr p) #t)
    (f (car p))
    (zmap (lambda (i)
            (let ((q (graph-node g i)))
              (if (not (car (cdr q)))
                  (dfs q)))) 
          (cdr (cdr p))))
  (zmap (lambda (p) (set-cdr! p (cons #f (cdr p)))) g)
  (zmap (lambda (p)
          (if (not (car (cdr p)))
              (dfs p)))
        g)
  (zmap (lambda (p) (set-cdr! p (cdr (cdr p)))) g))    

(define (graph-connected? g v1 v2)
  (define res #f)
  (define (graph-node g i)
    (if (= i (car (car g)))
        (car g)
        (graph-node (cdr g) i)))
  (define (dfs p)
    (set-car! (cdr p) #t)
    (if (= v2 (car p))
        (set! res #t)
        (zmap (lambda (i)
                (let ((q (graph-node g i)))
                  (if (not (car (cdr q)))
                      (dfs q)))) 
              (cdr (cdr p)))))
  
  (zmap (lambda (p) (set-cdr! p (cons #f (cdr p)))) g)
  (dfs (graph-node g v1))
  (zmap (lambda (p) (set-cdr! p (cdr (cdr p)))) g)
  res)
