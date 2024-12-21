#lang racket

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

(define (directed edges start)
  (define all (remove-duplicates (append (map first edges) (map last edges))))
  (define scores (make-hash))
  (for/list ([key all])
    (define value (if (equal? key start) 0 +inf.0))
    (hash-set! scores key value))
  (define neighbor-ht (make-hash))
  (for/list ([edge edges])
    (hash-update! neighbor-ht (first edge) (lambda (lst) (cons (last edge) lst)) '()))
  (define (loop node score)
    (define neighbors
      (if (hash-has-key? neighbor-ht node)
          (hash-ref neighbor-ht node)
          '()))
    (for/list ([n neighbors])
      (define neighbor-score-curr (hash-ref scores n))
      (define neighbor-score-next (add1 score))
      (when (< neighbor-score-next neighbor-score-curr)
        (hash-set! scores n neighbor-score-next)
        (loop n neighbor-score-next))))
  (loop start 0)
  scores)

;; (define (undirected edges start)
;;  (directed (append edges (map (lambda (edge) (list (last edge) (first edge))) edges)) start))

(define (fmt fn)
  (for/list ((l (file->lines fn)))
    (string->list l)))

(define map1 (make-hash (list (cons '(0 0) #\7) (cons '(0 1) #\8) (cons '(0 2) #\9)
                              (cons '(1 0) #\4) (cons '(1 1) #\5) (cons '(1 2) #\6)
                              (cons '(2 0) #\1) (cons '(2 1) #\2) (cons '(2 2) #\3)
                              (cons '(3 1) #\0) (cons '(3 2) #\A))))

(define map2 (make-hash (list (cons '(0 1) #\^) (cons '(0 2) #\A)
                              (cons '(1 0) #\<) (cons '(1 1) #\v) (cons '(1 2) #\>))))

(define (edges ht)
  (for*/list ([key (hash-keys ht)]
              [dir (list up right down left)]
              #:do ((define neighbor-pos (go dir key)))
              #:when (hash-has-key? ht neighbor-pos))
    (list key neighbor-pos)))

(define (paths ht scores pos find)
  (cond ((equal? pos find) (list (list pos)))
        (else (for/list ((dir (list up right down left))
                         #:do ((define next (go dir pos)))
                         #:when (and (hash-has-key? scores next)
                                     (equal? (add1 (hash-ref scores pos)) (hash-ref scores next)))
                         (tl (paths ht scores next find)))
                (cons pos tl)))))

(define (dirchar from to)
  (cond ((equal? (go up from) to) #\^)
        ((equal? (go right from) to) #\>)
        ((equal? (go down from) to) #\v)
        ((equal? (go left from) to) #\<)))

(define (combos lst)
  (cond ((empty? lst) '(()))
        (else (for*/list ((hd (car lst)) (tl (combos (cdr lst))))
                (cons hd tl)))))

(define (calc l m)
  (map append*
       (combos
        (for/list ((s (cons #\A l)) (e l))
          (define start (hash-find m s))
          (define end (hash-find m e))
          (for/list ((path (paths m (directed (edges m) start) start end)))
            (append (for/list ((a path) (b (cdr path))) (dirchar a b)) (list #\A)))))))

(define (part1 input)
  (for/sum ((l input))
    (define n (string->number (list->string (filter char-numeric? l))))
    (define len (length (argmin length (for*/list ((a (calc l map1))
                                                   (b (calc a map2))
                                                   (c (calc b map2)))
                                         c))))
    (* n len)))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day21example.txt")) 126384)
  (check-equal? (part1 (fmt "static/day21input.txt")) 94284))