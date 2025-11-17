(var rex (require "rex_pcre"))

(var numstrings
	{:zero "0" :one "1" :two "2" :three "3" :four "4" 
         :five "5" :six "6" :seven "7" :eight "8" :nine "9"})

(fn lines [filepath]
	(with-open [f (io.open filepath)]
		(icollect [line (f:lines)] line)))

(fn digits [str]
	(icollect [num (rex.gmatch str "\\d")] num))

(fn numbers [str]
	(icollect [n (rex.gmatch str "(?=(\\d|one|two|three|four|five|six|seven|eight|nine))")]
		(or (. numstrings n) n)))

(fn day01 [lines numbers_func]
	(accumulate [ret 0 index line (pairs lines)]
		(do (var nums (numbers_func line))
		    (+ ret (tonumber (.. (. nums 1) (. nums (length nums))))))))

(var example01 (lines "static/day01example01.txt"))
(var example02 (lines "static/day01example02.txt"))
(var input (lines "static/day01input01.txt"))

(print "day01 part01 example" (day01 example01 digits))
(print "day01 part01 input" (day01 input digits))

(print "day01 part02 example" (day01 example02 numbers))
(print "day01 part02 input" (day01 input numbers))
