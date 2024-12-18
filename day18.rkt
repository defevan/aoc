#lang racket

(require graph threading)

(define (up x y) (list x (sub1 y)))
(define (down x y) (list x (add1 y)))
(define (right x y) (list (add1 x) y))
(define (left x y) (list (sub1 x) y))

(define (fmt fn)
  (for/list ((l (file->lines fn)))
    (map string->number (string-split l ","))))

(define (calc input w h steps target)
  (define corrupted (take input steps))
  (define ht
    (for*/hash ((h h) (w w)
                      #:do ((define key (list w h)))
                      #:when (not (member key corrupted)))
      (values key #t)))
  (define edges (append*
                 (for/list ((k (hash-keys ht)))
                   (match-define (list x y) k)
                   (filter (lambda (lst) (hash-has-key? ht (last lst)))
                           (map (lambda (dir) (list (list x y) (dir x y)))
                                (list up right down left))))))
  (dijkstra (undirected-graph edges) (list 0 0)))

(define (part1 input w h steps target)
  (define-values (scores paths) (calc input w h steps target))
  (hash-ref scores target))

(define (chunk lst size)
  (cond ((null? lst)
         '())
        ((< (length lst) size)
         (list lst))
        (else (cons (take lst size) (chunk (drop lst size) size)))))

;; Six minute run time on input. You should reealy make this better.
;; Optimized dijkstras + Binary search?
(define (part2 input w h start-steps target)
  (define threads
    (for/list ((c (chunk (range start-steps (sub1 (length input))) 102)))
      (thread
       (lambda ()
         (define (loop step-lst)
           (if (null? step-lst)
               'nil
               (let ()
                 (define-values (scores paths) (calc input w h (car step-lst) target))
                 (if (and (hash-has-key? scores target)
                          (infinite? (hash-ref scores target)))
                     (list-ref input (sub1 (car step-lst)))
                     (loop (cdr step-lst))))))
         (thread-send (thread-receive) (loop c))))))
  (for/list ((t threads))
    (thread-send t (current-thread)))
  (~> (for/list ((t threads)) (thread-receive))
      (filter list? _)
      (map (lambda (v) (list v (index-of input v))) _)
      (argmin last _)
      (first)
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