#lang racket

(define (fmt fn)
  (filter integer? (map string->number (string-split (file->string fn) ""))))

(define (decode lst)
  (append* (for/list ([file-id (ceiling (/ (length lst) 2))])
             (define idx (* file-id 2))
             (define file-space (list-ref lst idx))
             (define free-space
               (let ([next-idx (add1 idx)])
                 (if (> (length lst) next-idx)
                     (list-ref lst next-idx)
                     0)))
             (append (make-list file-space file-id) (make-list free-space 'free)))))

(define (find-non-free-idx vec)
  (for/last ([el vec]
             [i (vector-length vec)]
             #:when (integer? el))
    i))

(define (move-by-id vec)
  (define first-free-idx (vector-member 'free vec))
  (define last-file-idx (find-non-free-idx vec))
  (if (< last-file-idx first-free-idx)
      vec
      (begin
        (vector-set! vec first-free-idx (vector-ref vec last-file-idx))
        (vector-set! vec last-file-idx 'free)
        (move-by-id vec))))

(define (part1 input)
  (define decoded (decode input))
  (define moved (move-by-id (list->vector decoded)))
  (for/sum ((file-id moved) (i (vector-length moved)) #:when (integer? file-id)) (* file-id i)))

(define (find-free-subset-with-length vec len)
  (define counting #f)
  (define idx 0)
  (define count 0)
  (define (exit? el i)
    (cond
      [(and (symbol? el) (false? counting))
       (begin
         (set! counting #t)
         (set! idx i)
         (set! count 1)
         (>= count len))]
      [(integer? el)
       (begin
         (set! counting #f)
         (set! idx 0)
         (set! count 0)
         #f)]
      [(symbol? el)
       (begin
         (set! count (add1 count))
         (>= count len))]))
  (for/first ([el vec]
              [i (vector-length vec)]
              #:when (exit? el i))
    idx))

(define (move-by-file ids hashcount vec)
  (if (empty? ids)
      vec
      (let* ([id (car ids)]
             [subset-len (hash-ref hashcount id)]
             [starting-subset-idx (vector-member id vec)]
             [starting-free-idx (find-free-subset-with-length vec subset-len)])
        (if (or (false? starting-free-idx) (> starting-free-idx starting-subset-idx))
            (move-by-file (cdr ids) hashcount vec)
            (begin
              (for/list ([i subset-len])
                (vector-set! vec (+ i starting-free-idx) id))
              (for/list ([i subset-len])
                (vector-set! vec (+ i starting-subset-idx) 'free))
              (move-by-file (cdr ids) hashcount vec))))))

(define (part2 input)
  (define decoded (decode input))
  (define hashcount
    (for/fold ([h (make-immutable-hash)]) ([el decoded])
      (hash-update h el add1 0)))
  (define ids (reverse (remove-duplicates (filter integer? decoded))))
  (define moved (move-by-file ids hashcount (list->vector decoded)))
  (for/sum ((file-id moved) (i (vector-length moved)) #:when (integer? file-id)) (* file-id i)))

(module+ test
  (require rackunit)
  (define example (fmt "static/day09example.txt"))
  (define input (fmt "static/day09input.txt"))
  (check-equal? (part1 example) 1928)
  (check-equal? (part2 example) 2858)
  (check-equal? (part1 input) 6200294120911)
  (check-equal? (part2 input) 6227018762750))

(module+ main
  (define input (fmt "static/day09input.txt"))
  (printf "day09\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
