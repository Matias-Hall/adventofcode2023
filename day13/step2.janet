(use ../common/common)

(def input (slurp "input"))

# Divides the input on empty lines.
(def input-blocks
  (peg/match
    ~{
      :main (any (* (<- :block) (? :empty-line)))
      :block (any (if-not :empty-line 1))
      :empty-line "\n\n"
      } input))

(def grid-peg
  (peg/compile
    ~{
      :main (some (* :line (? "\n")))
      :line (group (some :char))
      :char (<- (set "#."))}))

(defn get-grid
  [text]
  (peg/match grid-peg text))

(defn row
  "Returns the nth row of the grid, or nil if it does not exist."
  [grid n]
  (match (get grid n)
    nil nil
    # Must be a tuple, not an array for equality to work.
    found [;found]))

(defn column
  "Returns the nth column of the grid, or nil if it does not exist."
  [grid n]
  (try
    # Must be a tuple, not an array for equality to work.
    [;(map n grid)]
    # If mapping fails (e.g. because the column does not exist), return nil.
    ([_ _] nil)))

(defn diff
  "Counts the differences between two data structures"
  [left right]
  # This function could always be extended to add any surplus length as diff,
  # but for our purposes left and right should have the same length.
  (assert (= (length left) (length right)))
  (+ ;(map |(if (= $0 $1) 0 1) left right)))

(defn horizontal-reflection
  [grid]
  (prompt :is-mirror
    (each i (range 1 (length grid))
      (def total-diff
        (+
          ;(map
            (fn [gap]
              (def left (row grid (- i 1 gap)))
              (def right (row grid (+ i gap)))
              (if (some nil? [left right])
                0
                (diff left right)))
            (range 0 (length grid)))))
      # Only one smudge, hence total-diff must be 1.
      (if (= 1 total-diff) (return :is-mirror i)))))

(defn vertical-reflection
  [grid]
  (prompt :is-mirror
    (each i (range 1 (length (first grid)))
      (def total-diff
        (+
         ;(map
            (fn [gap]
              (def left (column grid (- i 1 gap)))
              (def right (column grid (+ i gap)))
              (if (some nil? [left right])
                0
                (diff left right)))
            (range 0 (length (first grid))))))
      # Only one smudge, hence total-diff must be 1.
      (if (= 1 total-diff) (return :is-mirror i)))))

(defn main [& args]
  (var result 0)
  (each grid (map get-grid input-blocks)
    (def reflection
      (let [horizontal (horizontal-reflection grid)]
        (if (nil? horizontal)
          [:vertical (vertical-reflection grid)]
          [:horizontal horizontal])))
    (+= result
      (match reflection
        [:vertical val] val
        [:horizontal val] (* val 100))))
  (print result))
