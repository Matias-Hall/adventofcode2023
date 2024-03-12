(def input (slurp "input"))

(defn split-lines [text]
  (peg/match
    ~{
      :main (any (* (<- :line) 1))
      :line (any (if-not "\n" 1))
      }
    text))

# Produces a list where the first element is the color
# and the second is the number of balls of said color.
(defn make-pick [num color]
  [(keyword color) num])

# Combines picks into a draw, a dictionary of (:color number) pairs.
(defn make-draw [& picks]
  (def result @{})
  (each pick picks
    (set (result (first pick)) (last pick)))
  result)

# Gets the largest pick for each color and puts it into one draw.
(defn largest-draw [& draws]
  (def result @{})
  (each draw draws
    (eachp [color num] draw
      (cond
        (nil? (result color)) (set (result color) num)
        (< (result color) num) (set (result color) num)
        )))
  result)

# This PEG returns an array where
# the first element is the game id,
# and all other elements are tables with the color and ball number.
(def line-peg (peg/compile
  ~{
    :main (* :id ": " (some :draw))
    :id (* "Game " :number)
    :draw (/
           (* (some :pick) (? "; "))
           ,make-draw)
    :pick (/
           (* :number " " (<- (+ "red" "green" "blue")) (? ", "))
           ,make-pick)
    # Gets an integer number, returns as type :number.
    :number (/ (<- (some (range "09"))) ,scan-number)
    }))

(defn extract-game [line]
  (peg/match line-peg line))

(def draw-limit {:red 12 :green 13 :blue 14})
(defn possible-draw? [draw]
  (var result true)
  (eachp [color num] draw
    (if (> num (draw-limit color))
      (set result false)))
  result)

(defn main [& args]
  (var result 0)
  (each line (split-lines input)
    (def [id & draws] (extract-game line))
    (if (possible-draw? (largest-draw ;draws))
      (set result (+ result id))))
  (print result))
