(use ../common/performance)

(def input (slurp "input"))

# Platform as a matrix, each row contains strings of one character.
(def platform
  (seq [line :in (string/split "\n" input)
        :unless (empty? line)]
    (map string/from-bytes line)))

(defn transpose
  "Transposes the given matrix"
  [matrix]
  (map tuple ;matrix))

(defn arr-to-tuple
  "Convert an array to tuple. This will recurse into indexed structures inside arr and convert them as well."
  [arr]
  (case (type arr)
    :array [;(map arr-to-tuple arr)]
    :tuple [;(map arr-to-tuple arr)]
    arr))

# Common function used on all push-{dir} functions.
(defn push-row
  "Pushes all rolling stones to the left"
  [row]
  # Where spaces are kept to be added later.
  (def space @[])
  (def result @[])
  (each token row
    (case token
      "O" (array/push result token)
      "." (array/push space token)
      "#" (do
            # Once a cube-shaped rock is reached, all the space was effectively
            # "displaced" by the rolling stones, so we add it at the end of result
            # and clear space.
            (array/push result ;space token)
            (array/clear space))))
  # Any additional space is appended.
  (array/push result ;space))

(defn push-left
  [grid]
  (map push-row grid))

(defn push-right
  [grid]
  (map |(->> $ reverse push-row reverse) grid))

(defn push-up
  [grid]
  (->> grid
       (transpose)
       (push-left)
       (transpose)))

(defn push-down
  [grid]
  (->> grid
       (transpose)
       (push-right)
       (transpose)))

(defn cycle
  [grid]
  (->> grid
       push-up
       push-left
       push-down
       push-right))

(defn print-grid
  [grid]
  (loop [row :in grid
         :after (print)
         char :in row]
    (prin char)))

(defn count-load
  [grid]
  (defn row-load
    [row]
    (->> row
         (map (fn [[i ith]] (if (= "O" ith) (inc i) 0)))
         (reduce + 0)))
  (->> grid
       (transpose)
       (map reverse)
       (map pairs)
       (map row-load)
       (reduce + 0)))

(defn range-iter
  [& args]
  (defn create-fib
    [start end step]
    (fiber-fn :y
      (var i start)
      (while (< i end)
        (yield i)
        (+= i step))))
  (match args
    [start end step] (create-fib start end step)
    [start end] (create-fib start end 1)
    [end] (create-fib 0 end 1)
    error "No arguments given."))

(defn main [& args]
  (var grid platform)
  (def grid-history @{})
  (def cycle-length 1_000_000_000)
  (def time (measure
    (def cycle
      (prompt :cycle
        (each i (range-iter 1 (inc cycle-length))
          # arr-to-tuple because keys are compared with =,
          # which looks at references for arrays.
          (set grid (arr-to-tuple (cycle grid)))
          (if (nil? (get grid-history grid))
            (set (grid-history grid) i)
            (return :cycle [(get grid-history grid) i])))))
    (def [start end] cycle)))

  (def index
    (+ start
       # Getting the offset of cycle-length from start.
       (% (- cycle-length start) (- end start))))

  (->> index
       (get (invert grid-history))
       (count-load)
       (print))
  (print time))
