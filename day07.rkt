#lang typed/racket

(define-type Op (-> Real Real Real))

(define (fmt (fn : String)) : (Listof (Listof Real))
  (for/list ([line : String (string-split (file->string fn) "\n")])
    (filter real? (map string->number (string-split line #rx":| ")))))

(define (op-perms (lst : (Listof Op)) (count : Real)) : (Listof (Listof Op))
  (if (equal? count 0)
      '(())
      (for*/list : (Listof (Listof Op))
        ([op : Op lst]
         [tl : (Listof Op) (op-perms lst (sub1 count))])
        (cons op tl))))

(define (valid? (available-ops : (Listof Op)) (line : (Listof Real))) : Boolean
  (for/or ([ops (op-perms available-ops (- (length line) 2))])
    (= (car line)
       (for/fold ([a : Real (car (cdr line))])
                 ([b : Real (cdr (cdr line))]
                  [f ops])
         (f a b)))))

(define (part1 (input : (Listof (Listof Real)))) : Real
  (for/sum : Real ((line : (Listof Real) input)
                   #:when (valid? (list + *) line))
    (car line)))

(define (divlog10 (b : Real)) : Real
  (define it (/ (log b) (log 10)))
  (if (real? it) it (raise 'forreal)))

(define (concat (a : Real) (b : Real)) : Real
  (+ (* a (expt 10 (divlog10 b))) b))

(define (part2 (input : (Listof (Listof Real)))): Real
  (for/sum : Real ((line : (Listof Real) input)
                   #:when (valid? (list + * concat) line))
    (car line)))

(module+ test
  (require typed/rackunit)
  (check-equal? (part1 (fmt "static/day07example.txt")) 3749)
  (check-equal? (part1 (fmt "static/day07input.txt")) 945512582195)
  (check-equal? (part2 (fmt "static/day07example.txt")) 11387)
  (check-equal? (part2 (fmt "static/day07input.txt")) 271691107779347))

(module+ main
  (define input (fmt "static/day07input.txt"))
  (printf "day07\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))