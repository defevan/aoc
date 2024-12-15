#lang racket

(require threading)

(define up (list sub1 identity))
(define right (list identity add1))
(define down (list add1 identity))
(define left (list identity sub1))

(define (dir c)
  (match c
    ["^" up]
    [">" right]
    ["v" down]
    ["<" left]))

(define (go dir pos)
  (list ((first dir) (first pos)) ((last dir) (last pos))))

(define (oneof lst v)
  (ormap (curry equal? v) lst))

(define (fmt fn)
  (match-define (list mapstr dirstr) (string-split (file->string fn) "\r\n\r\n"))
  (define lines (string-split mapstr "\r\n"))
  (define ht
    (make-hash (append* (for/list ([l lines]
                                   [linen (length lines)])
                          (define chars (filter non-empty-string? (string-split l "")))
                          (for/list ([c chars]
                                     [charn (length chars)])
                            `(,(list linen charn) . ,c))))))
  (define dirs
    (~> (string-split dirstr "") (filter (curry oneof (list "^" ">" "v" "<")) _) (map dir _)))
  (list ht dirs))

(define (find ht c)
  (for/list ([k (hash-keys ht)]
             #:do ((define value (hash-ref ht k)))
             #:when (equal? value c))
    k))

(define (can-move? ht pos dir)
  (define c (hash-ref ht pos))
  (cond
    [(equal? c ".") #t]
    [(equal? c "#") #f]
    [(equal? c "O") (can-move? ht (go dir pos) dir)]
    [(equal? c "@") (can-move? ht (go dir pos) dir)]
    [(and (oneof (list "[" "]") c) (oneof (list right left) dir)) (can-move? ht (go dir pos) dir)]
    [(and (equal? c "[") (oneof (list up down) dir))
     (and (can-move? ht (go dir pos) dir) (can-move? ht (go dir (go right pos)) dir))]
    [(and (equal? c "]") (oneof (list up down) dir))
     (and (can-move? ht (go dir pos) dir) (can-move? ht (go dir (go left pos)) dir))]))

(define (step ht c1 p1 dir)
  (define pos2 (go dir p1))
  (define c2 (hash-ref ht pos2))
  (define pos3 (go dir pos2))
  (define c3 (hash-ref ht pos3))
  (cond
    [(equal? c2 ".")
     (hash-set! ht pos2 c1)
     (hash-set! ht p1 c2)]
    [(equal? c2 "O")
     (step ht c2 pos2 dir)
     (hash-set! ht p1 ".")
     (hash-set! ht pos2 c1)]
    [(and (oneof (list "[" "]") c2) (oneof (list left right) dir))
     (step ht c3 pos3 dir)
     (step ht c2 pos2 dir)
     (hash-set! ht p1 ".")
     (hash-set! ht pos2 c1)]
    [(and (oneof (list "@" "]") c1) (equal? c2 "[") (oneof (list up down) dir))
     (step ht c2 pos2 dir)
     (step ht "]" (go right pos2) dir)
     (hash-set! ht pos2 c1)
     (hash-set! ht p1 ".")]
    [(and (oneof (list "@" "[") c1) (equal? c2 "]") (oneof (list up down) dir))
     (step ht c2 pos2 dir)
     (step ht "[" (go left pos2) dir)
     (hash-set! ht pos2 c1)
     (hash-set! ht p1 ".")]
    [(and (oneof (list "[" "]") c2) (oneof (list up down) dir))
     (step ht c2 pos2 dir)
     (hash-set! ht p1 ".")
     (hash-set! ht pos2 c1)])
  ht)

(define (move ht pos dirs)
  (cond
    [(empty? dirs) ht]
    [(can-move? ht pos (car dirs)) (move (step ht "@" pos (car dirs)) (car (find ht "@")) (cdr dirs))]
    [else (move ht pos (cdr dirs))]))

(define (calc ht dirs c)
  (for/sum ((b (find (move ht (car (find ht "@")) dirs) c))) (+ (* (first b) 100) (last b))))

(define (part1 ht dirs)
  (calc ht dirs "O"))

(define (wide input)
  (define ht (make-hash))
  (define w (last (argmax last (hash-keys input))))
  (define h (first (argmax first (hash-keys input))))
  (for/list ([linen (range (add1 h))])
    (for/list ([charn (range (add1 w))])
      (match-define (list left right)
        (match (hash-ref input (list linen charn))
          ["#" (list "#" "#")]
          ["O" (list "[" "]")]
          ["." (list "." ".")]
          ["@" (list "@" ".")]))
      (hash-set! ht (list linen (* charn 2)) left)
      (hash-set! ht (list linen (add1 (* charn 2))) right)))
  ht)

(define (part2 ht dirs)
  (calc (wide ht) dirs "["))

(module+ test
  (require rackunit)
  (check-equal? (apply part1 (fmt "static/day15example.txt")) 2028)
  (check-equal? (apply part1 (fmt "static/day15example02.txt")) 10092)
  (check-equal? (apply part1 (fmt "static/day15input.txt")) 1412971)
  (check-equal? (apply part2 (fmt "static/day15example02.txt")) 9021)
  (check-equal? (apply part2 (fmt "static/day15input.txt")) 1429299))

(module+ main
  (define input (fmt "static/day15input.txt"))
  (printf "day15\n\tpart1: ~a\n\tpart2: ~a\n" (apply part1 input) (apply part2 input)))