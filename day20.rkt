#lang racket

(define up (list sub1 identity))
(define right (list identity add1))
(define down (list add1 identity))
(define left (list identity sub1))

(define (go dir pos)
  (list ((first dir) (first pos)) ((second dir) (second pos))))

(define (neighbors ht key)
  (map (lambda (n) (list key n))
       (for/list ((dir (list up right down left))
                  #:do ((define neighbor-pos (go dir key)))
                  #:when (and (hash-has-key? ht neighbor-pos)
                              (not (equal? #\# (hash-ref ht neighbor-pos)))))
         neighbor-pos)))

(define (fmt fn)
  (define ht (make-hash))
  (define lines (file->lines fn))
  (for/list ((l lines) (linen (length lines)))
    (define chars (string->list l))
    (for/list ((c chars) (charn (length chars)))
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
  (for/first ((key (hash-keys ht))
              #:do ((define value (hash-ref ht key)))
              #:when (equal? value find))
    key))
    
(define (part1-bad input)
  (define edges
    (append*
     (for/list ((key (hash-keys input)) #:when (not (equal? #\# (hash-ref input key))))
       (neighbors input key))))
  (define cheats
    (for/list ((key (hash-keys input)) #:when (equal? #\# (hash-ref input key)))
      (neighbors input key)))
  (define start (ht-find input #\S))
  (define end (ht-find input #\E))
  (define default (hash-ref (undirected edges start) end))
  (length (filter (lambda (v) (< v default))
                  (for/list ((c cheats) (i (length cheats)) #:when (not (empty? c)))
                    (hash-ref (undirected (append edges c) start) end)))))

(define (cheats scores pos checked proc)
  (hash-set! checked pos #t)
  (define cheat-positions (for/list ((c (list up right down left))
                                     #:do ((define c-pos (go c (go c pos))))
                                     #:when (and (hash-has-key? scores c-pos)
                                                 (proc (hash-ref scores c-pos) (hash-ref scores pos))))
                            (go c pos)))
  (append* cheat-positions 
           (for/list ((dir (list up right down left))
                      #:do ((define next (go dir pos)))
                      #:when (and (hash-has-key? scores next)
                                  (not (hash-has-key? checked next))))
             (cheats scores next checked proc))))

(define (part1 input diff)
  (define edges
    (append*
     (for/list ((key (hash-keys input)) #:when (not (equal? #\# (hash-ref input key))))
       (neighbors input key))))
  (define start (ht-find input #\S))
  (define end (ht-find input #\E))
  (define scores (undirected edges start))
  (define (proc score expected) (< score (- expected diff)))
  (length (remove-duplicates (cheats scores end (make-hash) proc))))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day20example.txt") 2) 44)
  (check-equal? (part1 (fmt "static/day20input.txt") 100) 1289))