#lang racket

(define-syntax-rule (define/memo (id args ...) body ...)
  (define id
    ((lambda (ht)
       (lambda (args ...)
         (hash-ref ht
                   (list args ...)
                   (lambda ()
                     (define ret
                       (let ()
                         body ...))
                     (hash-set! ht (list args ...) ret)
                     ret))))
     (make-hash))))

(define (hash-find ht find)
  (for/first ([key (hash-keys ht)]
              #:do ((define value (hash-ref ht key)))
              #:when (equal? value find))
    key))

(define up (list sub1 identity))
(define right (list identity add1))
(define down (list add1 identity))
(define left (list identity sub1))

(define (go dir pos)
  (list ((first dir) (first pos)) ((second dir) (second pos))))

(define (fmt fn)
  (for/list ([l (file->lines fn)])
    (string->list l)))

(define map1
  (make-hash (list (cons '(0 0) #\7)
                   (cons '(0 1) #\8)
                   (cons '(0 2) #\9)
                   (cons '(1 0) #\4)
                   (cons '(1 1) #\5)
                   (cons '(1 2) #\6)
                   (cons '(2 0) #\1)
                   (cons '(2 1) #\2)
                   (cons '(2 2) #\3)
                   (cons '(3 1) #\0)
                   (cons '(3 2) #\A))))

(define map2
  (make-hash
   (list (cons '(0 1) #\^) (cons '(0 2) #\A) (cons '(1 0) #\<) (cons '(1 1) #\v) (cons '(1 2) #\>))))

(define (dirchar from to)
  (cond
    [(equal? (go up from) to) #\^]
    [(equal? (go right from) to) #\>]
    [(equal? (go down from) to) #\v]
    [(equal? (go left from) to) #\<]))

(define (combos lst)
  (cond
    [(empty? lst) '(())]
    [else
     (for*/list ([hd (car lst)]
                 [tl (combos (cdr lst))])
       (cons hd tl))]))

(define (all-paths ht start find)
  (define (loop queue result)
    (cond
      [(empty? queue) result]
      [else
       (define path (first queue))
       (define curr (first path))
       (loop (append (rest queue)
                     (for/list ([dir (list up right down left)]
                                #:do ((define next (go dir curr)))
                                #:when (and (not (member next path)) (hash-has-key? ht next)))
                       (cons next path)))
             (if (equal? curr find)
                 (cons (reverse path) result)
                 result))]))
  (loop (list (list start)) '()))

(define (possible-paths m l)
  (combos (reverse (for/list ([s (cons #\A l)]
                              [e l])
                     (define paths (all-paths m (hash-find m s) (hash-find m e)))
                     (define min-length (length (argmin length paths)))
                     (for/list ([path paths]
                                #:when (equal? min-length (length path)))
                       (append (for/list ([a path]
                                          [b (cdr path)])
                                 (dirchar a b))
                               (list #\A)))))))

(define/memo (robots lst input)
             (if (empty? lst)
                 (length input)
                 (apply min
                        (for/list ([paths (possible-paths (car lst) input)])
                          (for/sum ((steps paths)) (robots (cdr lst) steps))))))

(define (day21 count input)
  (for/sum ((l input))
           (* (string->number (list->string (filter char-numeric? l)))
              (robots (cons map1 (make-list count map2)) l))))

(module+ test
  (require rackunit)
  (check-equal? (day21 2 (fmt "static/day21example.txt")) 126384)
  (check-equal? (day21 2 (fmt "static/day21input.txt")) 94284)
  (check-equal? (day21 25 (fmt "static/day21input.txt")) 116821732384052))

(module+ main
  (define input (fmt "static/day21input.txt"))
  (printf "day21\n\tpart1: ~a\n\tpart2: ~a\n" (day21 2 input) (day21 25 input)))
