; file and directory operations
(define (pwd)
  (cd))

(define (ls . dir)
  (directory-list 
    (if (null? dir) 
        (cd)
        (car dir))))

(cd "somedir")

;(file-regular? path) 
;(file-directory? path) 
;(file-symbolic-link? path) 
;(file-exists? path) 

(define (time-info t)
  (define d (time-utc->date t))
  (format "~10a ~12a" 
          (format "~a:~a:~a" 
                  (date-hour d) (date-minute d) (date-second d))
          (format "~a-~a-~a"
                  (date-year d) (date-month d) (date-day d))))

(define (file-size f)
  (define KB (expt 2 10))
  (define MB (expt 2 20))
  (define GB (expt 2 30))
  (call-with-input-file f 
    (lambda (p) 
      (let ((s (file-length p)))
        (cond
          ((< s KB) s)
          ((< s MB) (format "~,2fK" (inexact (/ s KB))))
          ((< s GB) (format "~,2fM" (inexact (/ s MB))))
          (else (format "~,2fG" (inexact (/ s GB)))))))))


(define (file-info f)
  (cond
    ((file-regular? f) 
     (printf "~20a\t~a\t~a\n" f (time-info (file-change-time f)) (file-size f)))
    ((file-directory? f)
     (printf "~20a\t~a\n"  (format "~a/" f) (time-info (file-change-time f))))
    ((file-sysbolic-link? f)
     (printf "~20a\t~a\n" (format "~a@" f)  (time-info (file-change-time f))))))

(define (ll . path)
  (if (null? path)
      (ll ".")
      (let ((p (car path)))
        (if (file-exists? p)
            (if (file-regular? p) 
                (file-info p)
                (let ((orig (cd))
                      (items (directory-list p)))
                  (cd p)
                  (map (lambda (item) (file-info item)) items)
                  (cd orig)))))))

;(mkdir path)

(define (mkdirs path)
  (define orig (cd))
  (let mk ((p path))
    (if (not (file-exists? p))
        (let ((top (path-first p)))
          (if (string=? "" top)
              (mkdir p)
              (begin
                (if (not (file-exists? top)) (mkdir top))
                (if (file-directory? top)
                    (begin (cd top) (mk (path-rest p)))))))))
  (cd orig))

;(delete-file path)
;(delete-directory path)
;(rename-file old-pathname new-pathname) 

(define (rm dir)
  (if (file-exists? dir)
      (if (file-regular? dir)
          (delete-file dir)
          (let rmdir ((p dir))
            (define orig (cd))
            (define items (directory-list p))
            (cd p)
            (map (lambda (item) 
                   (if (file-regular? item)
                       (delete-file item)
                       (rmdir item)))
                 items)
            (cd orig)
            (delete-directory p)))))

(define (cp-file src dst)
  (let ((in (open-file-input-port src))
        (out (open-file-output-port dst (file-options no-fail))))
    (let loop ()
      (define buf (get-bytevector-n in 1024))
      (if (eof-object? buf)
          (begin (close-port in) (close-port out))
          (begin (put-bytevector out buf) (loop))))))

(define (cp-dir src dst)
  (map (lambda (item) 
         (cp (string-append src (string (directory-separator)) item) dst)) 
       (directory-list src)))

(define (cp src dst)
  (if (file-exists? src)
      (if (file-regular? src)
          (if (file-directory? dst)
              (cp-file src (string-append dst (string (directory-separator)) (path-last src)))
              (begin
                (if (not (file-exists? dst)) (mkdirs (path-parent dst)))
                (cp-file src dst)))
          (if (not (file-exists? dst)) 
              (begin (mkdirs dst) (cp-dir src dst))
              (if (file-directory? dst)
                  (let ((new-dst (string-append dst (string (directory-separator)) (path-last src))))
                    (if (not (file-exists? new-dst)) (mkdir new-dst))
                    (if (file-directory? new-dst) (cp-dir src new-dst))))))))

(define (mv src dst)
  (cp src dst)
  (rm src))

(define renamez rename-file) ; in same disk

(define (echo str . file)
  (if (null? file)
      (put-string (current-output-port)  str)
      (let ((p (open-output-file (car file) 'append)))
        (put-string p str)
        (close-output-port p))))

(define (cat file)
  (let ((p (open-input-file file)))
    (display (get-string-all p))
    (close-input-port p)))

(define (contain? str patt)
  (not (< (string-find str patt) 0)))

; find filename similar to patt in directory of dir
(define (ff patt . dir)
  (if (null? dir)
      (ff patt ".")
      (let ((f (car dir)))
        (if (file-exists? f)
            (begin
              (if (contain? (path-last f) patt) (printf "~a\n"f))
              (if (file-directory? f)
                  (map (lambda (i)
                         (ff patt (format "~a~a~a" f (directory-separator) i)))
                       (directory-list f)))))
        '())))

; grep wd in directory of file
(define (fw wd . file)
  (if (null? file)
      (fw wd ".")
      (let ((f (car file)))
        (cond
          ((file-regular? f)
           (call-with-input-file f
             (lambda (p)
               (let parse ((ln 1) (line (get-line p)))
                 (if (not (eof-object? line))
                     (begin
                       (if (contain? line wd)
                           (printf "~a:~a ~a\n" f ln line))
                       (parse (+ ln 1) (get-line p))))
                 '()))))
          ((file-directory? f)
           (map (lambda (i) 
                  (fw wd (format "~a~a~a" f (directory-separator) i)))
                (directory-list f))
           '())))))

