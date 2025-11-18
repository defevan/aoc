(var rtotal 12)
(var gtotal 13)
(var btotal 14)

(print
  (with-open [f (io.open "static/day02input01.txt")]
    (accumulate [(p1 p2) (values 0 0) line (f:lines)]
      (let [game-index (tonumber (line:match "Game (%d+): "))
            games (line:match ":%s*(.+)")
            (can-play r g b)
              (accumulate [(can-play r g b) (values true 0 0 0) round (games:gmatch "%s*([^;]+)")]
                (let [red (tonumber (or (round:match "(%d+) red") :0))
                      green (tonumber (or (round:match "(%d+) green") :0))
                      blue (tonumber (or (round:match "(%d+) blue") :0))]
                  (values (and can-play (<= red rtotal) (<= green gtotal) (<= blue btotal))
                          (math.max r red)
                          (math.max g green)
                          (math.max b blue))))]
      (values (if can-play (+ p1 game-index) p1)
              (+ p2 (* r g b)))))))
