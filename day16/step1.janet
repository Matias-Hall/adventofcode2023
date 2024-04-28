(def input (slurp "input"))

(defn make-map
  [token]
  (def vertical-dirs [:up :down])
  (def horizontal-dirs [:left :right])
  (def all-dirs [;horizontal-dirs ;vertical-dirs])
  (case token
    # Just like to keep all the types the same. Keeping this as a table would probably be fine.
    "." (struct ;(kvs (zipcoll all-dirs all-dirs)))
    "-" {:left :left :right :right :up horizontal-dirs :down horizontal-dirs}
    "|" {:left vertical-dirs :right vertical-dirs :up :up :down :down}
    "/" {:left :down :right :up :up :right :down :left}
    "\\" {:left :up :right :down :up :left :down :right}))

(defn make-cell
  [token]
  # :energized keeps a list of all directions where a beam of light has passed through the cell.
  # This tells us if it has been energized but, more importantly, helps avoid infinite recursion.
  {:energized @[] :map (make-map token)})

(def grid
  (peg/match
    ~{:main (some (* :row (? "\n")))
      :row (group (some :token))
      :token (/ (<- (if-not "\n" 1)) ,make-cell)}
    input))

(defn contains
  [ds value]
  (truthy? (find |(= value $) ds)))

(defn next-index
  [index direction]
  (case direction
    :left [(first index) (dec (last index))]
    :right [(first index) (inc (last index))]
    :up [(dec (first index)) (last index)]
    :down [(inc (first index)) (last index)]))

(defn spread-beam
  [grid index direction]
  (def cell (get-in grid index))
  (unless (or
            # nil cells happen when the index was outside the grid.
            # These are fine and can be ignored.
            (nil? cell)
            # Do not keep going if this cell has already been visited in this direction.
            (contains (cell :energized) direction))
    (array/push (cell :energized) direction)
    (def output ((cell :map) direction))
    (defn follow [dir]
      (spread-beam grid (next-index index dir) dir))
    (case (type output)
      :tuple (map follow output)
      :keyword (follow output)
      (error (string/format "Strange type for output: %t" output)))))

(defn count-energized
  [grid]
  (+
   ;(seq [row :in grid
          cell :in row
          :unless (empty? (cell :energized))]
      1)))

(defn main [& args]
  (spread-beam grid [0 0] :right)
  (print (count-energized grid)))
