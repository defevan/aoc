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
              #:when (and (equal? (car p3) (cdr p2))
                          (equal? (cdr p3) node)))
          (set! groups (cons (sort (map car (list p1 p2 p3)) string<?) groups))))))
  (length (remove-duplicates groups)))

(define (part2 undirected)
  (define ht ;; #hash((co . #<set: de ka ta>))
    (for/fold ([ht (make-immutable-hash)]) ([node (map car undirected)])
      (define connected
        (for/set ([pair undirected]
                  #:when (equal? node (car pair)))
          (cdr pair)))
      (hash-set ht node connected)))
  (define maxset ;; #<set: co de ka ta>
    (for/fold ([accum (set)])
              ([node (hash-keys ht)])
      (define connected (set-copy (hash-ref ht node)))
      (for ([c1 connected])
        (for ([c2 (set-remove (set-union (set) connected) c1)])
          (unless (set-member? (hash-ref ht c2) c1)
            (set-remove! connected c2))))
      (argmax set-count (list accum (set-union (set node) connected)))))
  (~> (set->list maxset)
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
