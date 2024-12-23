#lang racket/base

(require racket/list
         racket/file
         racket/match
         racket/function
         racket/string
         racket/set
         threading)

(define (fmt fn)
  (append* (for/list ([line (file->lines fn)])
             (match-define (list _ left right) (regexp-match #rx"([a-z]+)-([a-z]+)" line))
             (list (cons left right) (cons right left)))))

(define (part1 undirected)
  (define ts (filter (curryr string-prefix? "t") (remove-duplicates (map car undirected))))
  (define groups '())
  (for ([node ts])
    (for ([p1 undirected]
          #:when (equal? (car p1) node))
      (for ([p2 undirected]
            #:when (equal? (car p2) (cdr p1)))
        (for ([p3 undirected]
              #:when (and (equal? (car p3) (cdr p2)) (equal? (cdr p3) node)))
          (set! groups (cons (sort (map car (list p1 p2 p3)) string<?) groups))))))
  (length (remove-duplicates groups)))

(define (bron-kerbosch ht r p x)
  (if (andmap empty? (list p x))
      (list r)
      (let loop ([p p]
                 [accum '()])
        (if (empty? p)
            accum
            (loop
             (cdr p)
             (append
              accum
              (bron-kerbosch
               ht
               (cons (car p) r)
               (filter (lambda (u) (member u (hash-ref ht (car p) '()))) p)
               (filter (lambda (u) (member u (hash-ref ht (car p) '()))) x))))))))

(define (part2 undirected)
  (define ht ;; #hash((co . #<set: de ka ta>))
    (for/fold ([ht (make-immutable-hash)]) ([node (map car undirected)])
      (define connected
        (for/list ([pair undirected]
                   #:when (equal? node (car pair)))
          (cdr pair)))
      (hash-set ht node connected)))
  (~> (bron-kerbosch ht '() (hash-keys ht) '())
      (argmax length _)
      (sort string<?)
      (string-join ",")))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day23example.txt")) 7)
  (check-equal? (part1 (fmt "static/day23input.txt")) 1062)
  (check-equal? (part2 (fmt "static/day23example.txt")) "co,de,ka,ta")
  (check-equal? (part2 (fmt "static/day23input.txt")) "bz,cs,fx,ms,oz,po,sy,uh,uv,vw,xu,zj,zm"))

(module+ main
  (define input (fmt "static/day23input.txt"))
  (printf "day23\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
