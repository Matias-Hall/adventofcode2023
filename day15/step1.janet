(def input (slurp "input"))

(defn parse-steps [text]
  (peg/match
    ~{
      :main (some (* :value (? ",")))
      :value (<- (some (if-not (set ",\n") 1)))}
    text))

# Named hash-aoc to avoid conflicting with hash function.
(defn hash-aoc
  [text]
  (var result 0)
  (each char text
    (+= result char)
    (*= result 17)
    (%= result 256))
  result)

(defn main [& args]
  (print (+ ;(map hash-aoc (parse-steps input)))))

