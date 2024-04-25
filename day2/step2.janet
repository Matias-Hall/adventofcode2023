(use ../common/common)

(def input (slurp "input"))

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

# The power of a draw is defined as the product of its red, green, and blue elements.
(defn draw-power [draw]
  (reduce * 1 draw))

(defn main [& args]
  (var result 0)
  (each line (split-lines input)
    (def [id & draws] (extract-game line))
    (set result (+ result (draw-power (largest-draw ;draws)))))
  (print result))
