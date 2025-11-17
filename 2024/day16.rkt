#lang racket

(require graph)

(define (fmt fn)
  (define ht (make-hash))
  (define lines (file->lines fn))
  (define turns
    (append* (for/list ([l lines]
                        [linen (length lines)])
               (define chars (filter non-empty-string? (string-split l "")))
               (append* (for/list ([c chars]
                                   [charn (length chars)]
                                   #:when (not (equal? c "#")))
                          (hash-set! ht (list linen charn) c)
                          (list (list 1000 (list linen charn 'n) (list linen charn 'e))
                                (list 1000 (list linen charn 'e) (list linen charn 's))
                                (list 1000 (list linen charn 's) (list linen charn 'w))
                                (list 1000 (list linen charn 'w) (list linen charn 'n))
                                (list 1000 (list linen charn 'n) (list linen charn 'w))
                                (list 1000 (list linen charn 'w) (list linen charn 's))
                                (list 1000 (list linen charn 's) (list linen charn 'e))
                                (list 1000 (list linen charn 'e) (list linen charn 'n))))))))
  (define forwards
    (append* (for/list ([key (hash-keys ht)])
               (match-define (list linen charn) key)
               (define n (list (sub1 linen) charn 'n))
               (define e (list linen (add1 charn) 'e))
               (define s (list (add1 linen) charn 's))
               (define w (list linen (sub1 charn) 'w))
               (for/list ([dir (list n e s w)]
                          #:when (hash-has-key? ht (take dir 2)))
                 (list 1 (list linen charn (last dir)) dir)))))
  (define ew (append turns forwards))
  (list ht (directed-graph (map cdr ew) (map car ew))))

(define (key-by-value ht v)
  (for/first ([key (hash-keys ht)]
              #:do ((define val (hash-ref ht key)))
              #:when (equal? val v))
    key))

(define (keys-by-value ht find)
  (for/list ([k (hash-keys ht)]
             [v (hash-values ht)]
             #:when (equal? v find))
    k))

(define (go-left key)
  (match (third key)
    ['n (append (take key 2) (list 'w))]
    ['e (append (take key 2) (list 'n))]
    ['s (append (take key 2) (list 'e))]
    ['w (append (take key 2) (list 's))]))

(define (go-right key)
  (match (third key)
    ['n (append (take key 2) (list 'e))]
    ['e (append (take key 2) (list 's))]
    ['s (append (take key 2) (list 'w))]
    ['w (append (take key 2) (list 'n))]))

(define (go-back key)
  (match (third key)
    ['n (list (add1 (first key)) (second key) 'n)]
    ['e (list (first key) (sub1 (second key)) 'e)]
    ['s (list (sub1 (first key)) (second key) 's)]
    ['w (list (first key) (add1 (second key)) 'w)]))

(define (backtrack scores key end)
  (cond
    [(equal? key end) (list key)]
    [else
     (define score (hash-ref scores key))
     (define checks
       (list (list (go-left key) (curryr - 1000))
             (list (go-right key) (curryr - 1000))
             (list (go-back key) (curryr - 1))))
     (cons key
           (append* (for/list ([c checks]
                               #:when (member (first c) (keys-by-value scores ((last c) score))))
                      (backtrack scores (first c) end))))]))

(define (calc ht g)
  (define start-key (append (key-by-value ht "S") (list 'e)))
  (define end-pos (key-by-value ht "E"))
  (define-values (scores _) (dijkstra g start-key))
  (match-define (list distance end-key)
    (argmin car
            (map (lambda (dir)
                   (list (hash-ref scores (append end-pos (list dir))) (append end-pos (list dir))))
                 (list 'n 'e 's 'w))))
  (list start-key end-key scores distance))

(define (part1 ht g)
  (last (calc ht g)))

(define (part2 ht g)
  (match-define (list start-key end-key scores _) (calc ht g))
  (length (remove-duplicates (map (curryr take 2) (backtrack scores end-key start-key)))))

(module+ test
  (require rackunit)
  (check-equal? (apply part1 (fmt "static/day16example.txt")) 7036)
  (check-equal? (apply part1 (fmt "static/day16example02.txt")) 11048)
  (check-equal? (apply part1 (fmt "static/day16input.txt")) 123540)
  (check-equal? (apply part2 (fmt "static/day16example.txt")) 45)
  (check-equal? (apply part2 (fmt "static/day16example02.txt")) 64)
  (check-equal? (apply part2 (fmt "static/day16input.txt")) 665))

(module+ main
  (define input (fmt "static/day16input.txt"))
  (printf "day16\n\tpart1: ~a\n\tpart2: ~a\n" (apply part1 input) (apply part2 input)))
