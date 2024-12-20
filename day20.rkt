#lang racket

(define up (list sub1 identity))
(define right (list identity add1))
(define down (list add1 identity))
(define left (list identity sub1))

(define (go dir pos)
  (list ((first dir) (first pos)) ((second dir) (second pos))))

(define (fmt fn)
  (define ht (make-hash))
  (define lines (file->lines fn))
  (for/list ([l lines]
             [linen (length lines)])
    (define chars (string->list l))
    (for/list ([c chars]
               [charn (length chars)])
      (hash-set! ht (list linen charn) c)))
  ht)

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

(define (undirected edges start)
  (directed (append edges (map (lambda (edge) (list (last edge) (first edge))) edges)) start))

(define (ht-find ht find)
  (for/first ([key (hash-keys ht)]
              #:do ((define value (hash-ref ht key)))
              #:when (equal? value find))
    key))

(define (cheats scores diff size)
  (for/sum ((combo (in-combinations (hash-keys scores) 2))
            #:do ((match-define (list a b) combo)
                  (define cheat (+ (abs (- (first a) (first b))) (abs (- (second a) (second b)))))
                  (define score-diff (abs (- (hash-ref scores b) (hash-ref scores a)))))
            #:when (and (<= cheat size) (<= cheat (- score-diff diff))))
           1))

(define (calc input diff size)
  (define edges
    (append* (for/list ([key (hash-keys input)]
                        #:when (not (equal? #\# (hash-ref input key))))
               (map (lambda (n) (list key n))
                    (for/list ([dir (list up right down left)]
                               #:do ((define neighbor-pos (go dir key)))
                               #:when (and (hash-has-key? input neighbor-pos)
                                           (not (equal? #\# (hash-ref input neighbor-pos)))))
                      neighbor-pos)))))
  (cheats (undirected edges (ht-find input #\S)) diff size))

(module+ test
  (require rackunit)
  (check-equal? (calc (fmt "static/day20example.txt") 2 2) 44)
  (check-equal? (calc (fmt "static/day20input.txt") 100 2) 1289)
  (check-equal? (calc (fmt "static/day20input.txt") 100 20) 982425))

(module+ main
  (define input (fmt "static/day20input.txt"))
  (printf "day20\n\tpart1: ~a\n\tpart2: ~a\n"
          (calc (fmt "static/day20input.txt") 100 2)
          (calc (fmt "static/day20input.txt") 100 20)))
