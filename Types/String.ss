
; String, 字符串
; ============================================================================

(define name "Henry Mill")
(string #\a #\b #\c)                         ; "abc"
(make-string 3 #\x)                          ; "xxx"
                      
(string-length name)                         ; 10
(string-ref name 3)                          ; #\r
(string-set! name 3 #\l)                     ; name = "Henly Mill"

(define tmp (string-copy name))              ; tmp = "Henly Mill", name = "Henly Mill"
(string-append "name = \"" tmp "\".")        ; "name = "Henly Mill""
(string-upcase "Hi")                         ; "HI"
(string-downcase "Hi")                       ; "hi"
(string-split " name = Jhon " #\=)           ; (" name " " Jhon ")
(string-trim " Jhon ")                       ; "Jhon"

(substring name 6 8)                         ; "Mi"
(string-find name "l")                       ; 3
(string-find name "l" 4)                     ; 8
(string-find name "ly")                      ; 3
(string-replace name "en" "ong")             ; Hongly Mill 

(string=? name "jack")                       ; #f
(string<? name "jack")                       ; #t
(string-ci=? "Mom and Dad" "mom and dad")    ; #t
(string-ci<=? "say what" "Say What!?")       ; #t

(string->list "abc")                         ; (#\a #\b #\c)
(list->string '(#\a #\b #\c))                ; "abc"

; ============================================================================

(define (string-trim str)

  (define (space? i)
    (char=? #\space (string-ref str i)))
    
  (let ((len (string-length str)))
    (if (= 0 len)
        ""
        (let t ((s 0))
          (if (and (< s len) (space? s))
              (t (+ 1 s))
              (if (= s len)
                  ""
                  (let r ((e (- len 1)))
                    (if (and (< s e) (space? e))
                        (r (- e 1))
                        (substring str s (+ 1 e))))))))))

(define (string-split str chr)
  
  (define len (string-length str))
  
  (define (delim? i)
    (char=? chr (string-ref str i)))
  
  (define (get-start i)
    (if (and (< i len) (delim? i))
        (get-start (+ 1 i))
        i))
  
  (define (get-end j)
    (if (and (< j len) (not (delim? j)))
        (get-end (+ 1 j))
        j))
  
  (let t ((i 0))
    (let* ((b (get-start i)) (e (get-end b)))
      (if (= b e)
          '()
          (cons (substring str b e) (t e))))))
 
(define string-find
  (case-lambda
    ((str sub)
     (string-find str sub 0))
    ((str sub n)
     
     (define (match? i start)
       (char=? (string-ref sub i) (string-ref str (+ i start))))
     
     (define (next sub len)
       (if (= 0 len)
           1
           (let t ((p 0) (offset 1))
             (if (and (< (+ p offset) len)
                      (char=? (string-ref sub p) (string-ref sub (+ p offset))))
                 (t (+ 1 p) offset)
                 (if (= (+ p offset) len)
                     offset
                     (t 0 (+ 1 offset)))))))
     
     (let ((len (string-length str))
            (sl (string-length sub)))
       (if (= 0 sl)
           -1
           (let ((mv (make-vector sl)))
             (let while ((i 0))
               (if (< i sl)
                   (begin 
                     (vector-set! mv i (next sub i)) 
                     (while (+ 1 i)))))
             (let s ((p n))
               (if (> (+ p sl) len)
                   -1
                   (let t ((q 0))
                     (if (and (< q sl) (match? q p))
                         (t (+ 1 q))
                         (if (= q sl)
                             p
                             (s (+ p (vector-ref mv q)))))))))))))))
       
(define (string-replace str sub1 sub2)
  (let ((p (string-find str sub1)))
    (if (>= p 0)
        (string-append
          (substring str 0 p)
          sub2
          (substring str (+ p (string-length sub1)) (string-length str))))))

