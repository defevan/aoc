(var rtotal 12)
(var gtotal 13)
(var btotal 14)

(print
  (with-open [f (io.open "static/day02input01.txt")]
    (accumulate [(p1 p2) (values 0 0) line (f:lines)]
      (let [id (tonumber (line:match "Game (%d+): "))
            (play? rmax gmax bmax)
              (accumulate [(play? rmax gmax bmax) (values true 0 0 0) round (line:gmatch "%s*([^;]+)")]
                (let [r (tonumber (or (round:match "(%d+) red") :0))
                      g (tonumber (or (round:match "(%d+) green") :0))
                      b (tonumber (or (round:match "(%d+) blue") :0))]
                  (values (and play? (<= r rtotal) (<= g gtotal) (<= b btotal))
                          (math.max r rmax)
                          (math.max g gmax)
                          (math.max b bmax))))]
        (values (if play? (+ p1 id) p1)
                (+ p2 (* rmax gmax bmax)))))))
