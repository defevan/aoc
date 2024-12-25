#lang racket

(define hashpat #px"^#+$")
(define dotpat #px"^\\.+$")

(define (fmt fn)
  (for/list ([thing (string-split (file->string fn) "\r\n\r\n")])
    (define lines (string-split thing "\r\n"))
    (define type
      (cond
        [(and (regexp-match hashpat (first lines)) (regexp-match dotpat (last lines))) 'lock]
        [(and (regexp-match dotpat (first lines)) (regexp-match hashpat (last lines))) 'key]
        [else (raise 'badinput)]))
    (cons type
          (for/hash ([line lines]
                     [linen (length lines)]
                     #:do ((define chars (filter non-empty-string? (string-split line ""))))
                     #:when #t
                     [char chars]
                     [charn (length chars)])
            (values (cons linen charn) char)))))

(define (pinheight thing)
  (define maxline (car (argmax car (hash-keys (cdr thing)))))
  (define maxchar (cdr (argmax cdr (hash-keys (cdr thing)))))
  (for/list ([charn (add1 maxchar)])
    (sub1 (for/sum ((linen (add1 maxline)) #:do ((define value
                                                   (hash-ref (cdr thing) (cons linen charn))))
                                           #:when (equal? value "#"))
                   1))))

(define (day25 input)
  (define locks (filter (compose (curry equal? 'lock) car) input))
  (define keys (filter (compose (curry equal? 'key) car) input))
  (define max (car (argmax car (hash-keys (cdr (car input))))))
  (for*/sum ((key (map pinheight keys)) (lock (map pinheight locks))
                                        #:when (andmap identity
                                                       (for/list ([k key]
                                                                  [l lock])
                                                         (< (+ k l) max))))
            1))

(module+ test
  (require rackunit)
  (check-equal? (day25 (fmt "static/day25example.txt")) 3)
  (check-equal? (day25 (fmt "static/day25input.txt")) 2840))

(module+ main
  (define input (fmt "static/day25input.txt"))
  (printf "day25\n\tpart1: ~a\n" (day25 input)))
