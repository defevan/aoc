#lang racket/base

(require racket/bool racket/match racket/string racket/file racket/list racket/function racket/format threading)

(define AND (lambda (a b) (and a b)))
(define OR (lambda (a b) (or a b)))
(define XOR (lambda (a b) (xor a b)))

(define (string->op str)
  (match str
    ("AND" AND)
    ("OR" OR)
    ("XOR" XOR)))

(define (fmt fn)
  (match-define (list hd tl) (string-split (file->string fn) "\r\n\r\n"))
  (list 
   (for/list ((m (regexp-match* #rx"([a-z0-9]+): ([0-9]+)" hd #:match-select cdr)))
     (cons (first m) (string->number (last m))))
   (for/list ((m (regexp-match* #px"([a-z0-9]+) (AND|XOR|OR) ([a-z0-9]+) -> ([a-z0-9]+)" tl #:match-select cdr)))
     (list (string->op (second m)) (first m) (third m) (fourth m)))))

(define (part1 init ops)
  (define ht
    (hash-copy 
     (for/hash ((def init))
       (match-define (cons sym val) def)
       (values sym (not (zero? val))))))
  (let loop ((ht ht) (ops ops))
    (cond ((empty? ops) null)
          (else 
           (match-define (list f a b key) (car ops))
           (cond ((andmap (curry hash-has-key? ht) (list a b))
                  (hash-set! ht key (f (hash-ref ht a) (hash-ref ht b)))
                  (loop ht (cdr ops)))
                 (else (loop ht (append (cdr ops) (list (car ops)))))))))
  (define zkeys (sort (filter (curryr string-prefix? "z") (hash-keys ht)) string>?))
  (define zvalues (for/list ((k zkeys)) (number->string (if (hash-ref ht k) 1 0))))
  (~> zvalues
      (string-join "")
      (string->number 2)))

;; Find store target by operands and operation.
(define (find ops op a b)
  (for/first ((lst ops)
              #:when (and (equal? (first lst) op)
                          (or (and (equal? (second lst) a)
                                   (equal? (third lst) b))
                              (and (equal? (second lst) b)
                                   (equal? (third lst) a)))))
    (fourth lst)))

;; Find index of operation in ops lst by operands and operation.
(define (index ops op a b)
  (for/first ((lst ops) (i (length ops))
                        #:when (and (equal? (first lst) op)
                                    (or (and (equal? (second lst) a)
                                             (equal? (third lst) b))
                                        (and (equal? (second lst) b)
                                             (equal? (third lst) a)))))
    i))
  
(define (swap ops a b)
  (define i1 (apply (curry index ops) (take a 3)))
  (define i2 (apply (curry index ops) (take b 3)))
  (list-set (list-set ops i1 a) i2 b))

(define (findswaps I i ops swaplst)
  (define num (~a i #:min-width 2 #:align 'right #:left-pad-string "0"))
  (define X (format "x~a" num))
  (define Y (format "y~a" num))
  (define A (find ops XOR X Y))
  (define B (find ops AND X Y))
  (define C (find ops AND I A))
  (define Z (find ops XOR I A))
  (define O (find ops OR C B))
  (cond ((zero? i)
         (findswaps B (add1 i) ops swaplst)) ;; base case
        ((> i 43)
         swaplst) ;; done
        ((not C)
         (findswaps I i (swap ops (list XOR X Y B) (list AND X Y A)) (cons A (cons B swaplst))))
        ((string-prefix? A "z")
         (findswaps I i (swap ops (list XOR X Y Z) (list XOR I A A)) (cons A (cons Z swaplst))))
        ((string-prefix? B "z")
         (findswaps I i (swap ops (list AND X Y Z) (list XOR I A A)) (cons B (cons Z swaplst))))
        ((string-prefix? C "z")
         (findswaps I i (swap ops (list AND I A Z) (list XOR I A A)) (cons C (cons Z swaplst))))
        ((string-prefix? O "z")
         (findswaps I i (swap ops (list OR C B Z) (list XOR I A O)) (cons O (cons Z swaplst))))
        (else
         (findswaps O (add1 i) ops swaplst))))

(define (part2 _ ops)
  (~> (findswaps #f 0 ops '())
      (sort string<?)
      (string-join ",")))
  
(module+ test
  (require rackunit)
  (check-equal? (apply part1 (fmt "static/day24example.txt")) 4)
  (check-equal? (apply part1 (fmt "static/day24example02.txt")) 2024)
  (check-equal? (apply part1 (fmt "static/day24input.txt")) 69201640933606)
  (check-equal? (apply part2 (fmt "static/day24input.txt")) "dhq,hbs,jcp,kfp,pdg,z18,z22,z27"))

(module+ main
  (define input (fmt "static/day24input.txt"))
  (printf "day24\n\tpart1: ~a\n\tpart2: ~a\n" (apply part1 input) (apply part2 input)))
