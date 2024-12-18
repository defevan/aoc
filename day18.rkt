#lang racket

(require graph
         threading)

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
               (match-define (list x y) k)
               (filter (lambda (lst) (hash-has-key? ht (last lst)))
                       (map (lambda (dir) (list (list x y) (dir x y))) (list up right down left))))))
  (define-values (scores _) (dijkstra (undirected-graph edges) (list 0 0)))
  scores)

(define (part1 input w h steps target)
  (hash-ref (calc input w h steps target) target))

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
  (define/memo (proc steps) (not (infinite? (hash-ref (calc input w h steps target) target))))
  (~> (bin proc (range start-steps (sub1 (length input))))
      (list-ref input _)
      (map number->string _)
      (string-join ",")))

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
