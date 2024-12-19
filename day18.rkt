#lang racket/base

(require racket/file
         racket/string
         racket/list)

(define (up x y)
  (list x (sub1 y)))
(define (down x y)
  (list x (add1 y)))
(define (right x y)
  (list (add1 x) y))
(define (left x y)
  (list (sub1 x) y))

(define (fmt fn)
  (for/list ([l (file->lines fn)])
    (map string->number (string-split l ","))))

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

(define (calc input w h steps target)
  (define corrupted (take input steps))
  (define ht
    (for*/hash ([h h]
                [w w]
                #:do ((define key (list w h)))
                #:when (not (member key corrupted)))
      (values key #t)))
  (define edges
    (append* (for/list ([k (hash-keys ht)])
               (filter (lambda (lst) (hash-has-key? ht (last lst)))
                       (map (lambda (dir) (list k (dir (first k) (last k))))
                            (list up right down left))))))
  (undirected edges (list 0 0)))

(define (part1 input w h steps target)
  (hash-ref (calc input w h steps target) target))

(define (bin proc lst)
  (define cursor (sub1 (floor (/ (length lst) 2))))
  (define middle (list-ref lst cursor))
  (define middle-value (proc middle))
  (define next-value (proc (list-ref lst (add1 cursor))))
  (cond
    [(and middle-value next-value) (bin proc (drop lst cursor))]
    [(and middle-value (not next-value)) middle]
    [else (bin proc (take lst cursor))]))

(define (part2 input w h start-steps target)
  (define (proc steps)
    (< (hash-ref (calc input w h steps target) target) +inf.0))
  (define result (bin proc (range start-steps (sub1 (length input)))))
  (string-join (map number->string (list-ref input result)) ","))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day18example.txt") 7 7 12 (list 6 6)) 22)
  (check-equal? (part2 (fmt "static/day18example.txt") 7 7 12 (list 6 6)) "6,1")
  (check-equal? (part1 (fmt "static/day18input.txt") 71 71 1024 (list 70 70)) 310)
  (check-equal? (part2 (fmt "static/day18input.txt") 71 71 1024 (list 70 70)) "16,46"))

(module+ main
  (define input (fmt "static/day18input.txt"))
  (printf "day18\n\tpart1: ~a\n" (part1 input 7 7 12 (list 6 6)))
  (printf "\tpart2: ~a\n" (part2 input 71 71 1024 (list 70 70))))
