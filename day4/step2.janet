(use ../common/common)

(def input (slurp "input"))

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
  # Contrary to step1, we must first get all the games into a list to initialise card-table.
  (def games (map parse-game (split-lines input)))
  # Card table has the form:
  # @{ id instances }, where id is the id of the card and instances is how many copies (including original) exist.
  # Initially, instances is 1 for all cards.
  (def card-table (tabseq [i :range-to [1 (length games)]] i 1))
  (each game games
    (def [id winning-numbers picked-numbers] game)
    (def score (find-winning-picks picked-numbers winning-numbers))
    (def id-instance (card-table id))
    (each i (range (+ id 1) (+ id score 1))
      (+= (card-table i) id-instance)))
  (print (+ ;(values card-table))))
