(def input (slurp "input"))

(defn get-stars [text]
  (peg/match
    ~{
      :main (some (+ :galaxy 1))
      :galaxy (group (* (line) (column) "#"))
      } text))

(defn expand-universe [stars]
  # Sorting by row.
  (sort-by first stars)
  (var next-row 1)
  # How much expansion (in total) to add to a star.
  # Each time a row expands this number increases by 1.
  (var row-expansion 0)
  (each star stars
    (def row (star 0))
    # If row larger than the next row, that means next-row is empty
    # So expansion must happen.
    (when (> row next-row)
      # Decreasing 1 from a million since one of the rows is already accounted for.
      # Consider in step1, rows are made twice as big by increasing _one_ to row-expansion.
      (+= row-expansion (dec 1_000_000)))
    # Setting the next expected row to be one more than the current row.
    (set next-row (inc (star 0)))
    (+= (star 0) row-expansion))
  # Sorting by column.
  (sort-by last stars)
  (var next-col 1)
  # How much expansion (in total) to add to a star.
  # Each time a column expands this number increases by 1.
  (var col-expansion 0)
  (each star stars
    (def col (star 1))
    # If col larger than the next column, that means next-col is empty
    # So expansion must happen.
    (when (> col next-col)
      # Decreasing 1 from a million since one of the columns is already accounted for.
      # Consider in step1, cols are made twice as big by increasing _one_ to col-expansion.
      (+= col-expansion (dec 1_000_000)))
    # Setting the next expected column to be one more than the current column.
    (set next-col (inc (star 1)))
    (+= (star 1) col-expansion))
  stars)

# Computes taxicab/Manhattan distance.
(defn distance [a b]
  (+
   (math/abs (- (b 1) (a 1)))
   (math/abs (- (b 0) (a 0)))))

(defn pair-distance-sum [stars]
  (var result 0)
  (var pairs 0)
  (while (not (empty? stars))
    # Popping each star-a so that it does not return to the array.
    # Each star-a gets its distance with every other star and then it is not considered again.
    (def star-a (array/pop stars))
    (each star-b stars
      (++ pairs)
      (+= result (distance star-a star-b))))
  result)

(defn main [& args]
  (-> input
    get-stars
    expand-universe
    pair-distance-sum
    pp))
