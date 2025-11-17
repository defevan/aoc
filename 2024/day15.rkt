#lang racket

(define up (list sub1 identity))
(define right (list identity add1))
(define down (list add1 identity))
(define left (list identity sub1))

(define (dir c)
  (match c
    [#\^ up]
    [#\> right]
    [#\v down]
    [#\< left]))

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
    (for/list ([c (string->list dirstr)]
               #:when (oneof (list #\^ #\> #\v #\<) c))
      (dir c)))
  (list ht dirs))

(define (find ht c)
  (for/list ([k (hash-keys ht)]
             #:do ((define value (hash-ref ht k)))
             #:when (equal? value c))
    k))

(define (can-step? ht pos dir)
  (define c (hash-ref ht pos))
  (cond
    [(equal? c ".") #t]
    [(equal? c "#") #f]
    [(oneof (list "O" "@") c) (can-step? ht (go dir pos) dir)]
    [(and (oneof (list "[" "]") c) (oneof (list right left) dir)) (can-step? ht (go dir pos) dir)]
    [(and (equal? c "[") (oneof (list up down) dir))
     (and (can-step? ht (go dir pos) dir) (can-step? ht (go dir (go right pos)) dir))]
    [(and (equal? c "]") (oneof (list up down) dir))
     (and (can-step? ht (go dir pos) dir) (can-step? ht (go dir (go left pos)) dir))]))

(define (step ht curr-char curr-pos dir)
  (define next-pos (go dir curr-pos))
  (define next-char (hash-ref ht next-pos))
  (cond
    [(equal? next-char ".")
     (hash-set! ht next-pos curr-char)
     (hash-set! ht curr-pos next-char)]
    [(and (oneof (list "@" "]") curr-char) (equal? next-char "[") (oneof (list up down) dir))
     (step ht next-char next-pos dir)
     (step ht "]" (go right next-pos) dir)
     (hash-set! ht next-pos curr-char)
     (hash-set! ht curr-pos ".")]
    [(and (oneof (list "@" "[") curr-char) (equal? next-char "]") (oneof (list up down) dir))
     (step ht next-char next-pos dir)
     (step ht "[" (go left next-pos) dir)
     (hash-set! ht next-pos curr-char)
     (hash-set! ht curr-pos ".")]
    [else
     (step ht next-char next-pos dir)
     (hash-set! ht curr-pos ".")
     (hash-set! ht next-pos curr-char)]))

(define (move ht pos dirs)
  (cond
    [(empty? dirs) ht]
    [(can-step? ht pos (car dirs))
     (step ht "@" pos (car dirs))
     (move ht (car (find ht "@")) (cdr dirs))]
    [else (move ht pos (cdr dirs))]))

(define (part1 ht dirs #:c [c "O"])
  (for/sum ((b (find (move ht (car (find ht "@")) dirs) c))) (+ (* (first b) 100) (last b))))

(define (wide input)
  (define ht (make-hash))
  (for/list ([key (hash-keys input)])
    (match-define (list linen charn) key)
    (match-define (list left right)
      (match (hash-ref input key)
        ["#" (list "#" "#")]
        ["O" (list "[" "]")]
        ["." (list "." ".")]
        ["@" (list "@" ".")]))
    (hash-set! ht (list linen (* charn 2)) left)
    (hash-set! ht (list linen (add1 (* charn 2))) right))
  ht)

(define (part2 ht dirs)
  (part1 (wide ht) dirs #:c "["))

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
