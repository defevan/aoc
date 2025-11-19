(fn gmatch [str pat]
  "string:gmatch, but returns a tuple of... [str, start-index, end-index]"
  (var last-index 1)
  (icollect [m (str:gmatch pat)]
    (let [(start end) (string.find str m last-index true)]
      (set last-index (+ end 1))
      [m start end])))

(fn adjacent [symbols]
  "given a list of symbols, produce a list of adjacent positions for all symbols"
  (var positions [])
  (each [_ [symbol x y] (ipairs symbols)]
    (let [top         [(+ x 0) (- y 1)]
          topright    [(+ x 1) (- y 1)]
          topleft     [(- x 1) (- y 1)]
          right       [(+ x 1) (+ y 0)]
          left        [(- x 1) (+ y 0)]
          bottom      [(+ x 0) (+ y 1)]
          bottomright [(+ x 1) (+ y 1)]
          bottomleft  [(- x 1) (+ y 1)]]
      (each [_ [x y] (ipairs [top topright topleft right left bottom bottomright bottomleft])]
        (table.insert positions [x y]))))
  positions)

(print
  (with-open [f (io.open "static/day03input01.txt")]
    (var engines [])
    (var symbols [])
    (var p1 0)
    (var p2 0)
    (var y 0)

    ;; collect engine and symbol positions
    (each [line (f:lines)]
      (set y (+ y 1))
      (each [_ [str start end] (ipairs (gmatch line "(%d+)"))]
        (table.insert engines [(tonumber str) start end y]))
      (each [_ [symbol x] (ipairs (gmatch line "[^%d%.]"))]
        (table.insert symbols [symbol x y])))

    ;; sum engine numbers that are next to symbols
    (each [_ [engine-number engine-x-start engine-x-end engine-y] (ipairs engines)]
      (var found false)
      (each [_ [x y] (ipairs (adjacent symbols))]
        (set found (or found (and (<= engine-x-start x engine-x-end) (= engine-y y)))))
      (if found
        (set p1 (+ p1 engine-number))))

    ;; sum "gear ratios" of symbols that are next to exactly two engines.
    ;; "gear ratio" = product of engine numbers.
    (each [_ symbol-value (ipairs symbols)]
      (var engine-count 0)
      (var gear-ratio 1)
      (each [_ [engine-number engine-x-start engine-x-end engine-y] (ipairs engines)]
        (var found false)
        (each [_ [x y] (ipairs (adjacent [symbol-value]))]
          (set found (or found (and (<= engine-x-start x engine-x-end) (= engine-y y)))))
        (if found
          (do (set engine-count (+ engine-count 1))
              (set gear-ratio (* gear-ratio engine-number)))))
      (if (= engine-count 2)
        (set p2 (+ p2 gear-ratio))))

    (values p1 p2)))
