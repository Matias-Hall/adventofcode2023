(def input (slurp "input"))

(defn split-lines [text]
  (peg/match
    ~{
      :main (any (* (<- :line) 1))
      :line (any (if-not "\n" 1))
      }
    text))

# Returns the array:
# @[id [winning-numbers] [picked-numbers]]
(def line-peg (peg/compile
  ~{
    :main (* :id (/ :num-list ,tuple) "|" :ws (/ :num-list ,tuple))
    :id (* "Card" (any " ") :number ":" :ws)
    :num-list (some (* :number :ws))
    :number (/ (<- (some (range "09"))) ,scan-number)
    :ws (any " ")
    }))

(defn parse-game [line]
  (peg/match line-peg line))

(defn find-winning-picks [picked-numbers winning-numbers]
  (defn pick-wins [pick]
    (find |(= pick $) winning-numbers))
  (length (filter pick-wins picked-numbers)))

(defn main [& args]
  (var result 0)
  (each line (split-lines input)
    (def [id winning-numbers picked-numbers] (parse-game line))
    (def score (find-winning-picks picked-numbers winning-numbers))
    (unless (= score 0)
      # Total score is multiplied by 2 for every picked number
      (+= result (math/exp2 (- score 1)))))
  (print result))

