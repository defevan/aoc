(fn get-match-count [line]
  (var (left right) 
    (line:match "Card%s+%d+:%s*([%d%s]+)%s*|%s*([%d%s]+)"))
  (var winning
    (collect [str (left:gmatch "(%d+)")]
      (values (tonumber str) true)))
  (var received
    (icollect [str (right:gmatch "(%d+)")]
      (tonumber str)))
  (accumulate [match-count 0 _ n (ipairs received)]
    (if (. winning n) 
      (+ match-count 1)
      match-count)))

(fn p1 [match-counts]
  (accumulate [sum 0 _ match-count (ipairs match-counts)]
    (+ sum (if (= match-count 0) 0 (^ 2 (- match-count 1))))))

(fn p2 [match-counts]
  (fn inner [index match-count]
    (faccumulate [sum 1 i (+ index 1) (+ index match-count)]
      (+ sum (inner i (. match-counts i)))))
  (accumulate [sum 0 index card (ipairs match-counts)]
    (+ sum (inner index card))))

(print 
  (with-open [f (io.open "static/day04input01.txt")]
    (var match-counts 
      (icollect [line (f:lines)]
        (get-match-count line)))
    (values (p1 match-counts) (p2 match-counts))))
