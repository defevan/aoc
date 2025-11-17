(fn flat [tbl]
  (fn inner [tbl ret]
    (each [key value (ipairs tbl)]
      (match (type value)
        :table (inner value ret)
        _ (table.insert ret value)))
    ret)
  (inner tbl {}))

(fn digits [str]
  "Return 'digits' in a string, in string form. E.G. `1five6eigh2` -> `1`, `6`, `2`."
  (flat
    (fcollect [i 1 (length str)]
      (icollect [j n (ipairs [:1 :2 :3 :4 :5 :6 :7 :8 :9])]
        (if (= (string.find str n i) i) n)))))

(fn numbers [str]
  "Return 'numbers' in a string, in string form. E.G. `five6eightwo` -> `5`, `6`, `8`, `2`."
  (var strnumbers {:one :1 :two :2 :three :3 :four :4 :five :5 :six :6 :seven :7 :eight :8 :nine :9})
  (flat
    (fcollect [i 1 (length str)]
      (icollect [j n (ipairs [:1 :2 :3 :4 :5 :6 :7 :8 :9 :one :two :three :four :five :six :seven :eight :nine])]
        (if (= (string.find str n i) i)
          (or (. strnumbers n) n))))))

(print
  (with-open [f (io.open "static/day01input01.txt")]
    (accumulate [(x y) (values 0 0) line (f:lines)]
      (let [dx (digits line)
            dy (numbers line)]
        (values (+ x (tonumber (.. (. dx 1) (. dx (length dx)))))
                (+ y (tonumber (.. (. dy 1) (. dy (length dy))))))))))
