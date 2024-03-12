(def input (slurp "input"))

(defn split-lines [text]
  (peg/match
    ~{
      :main (any (* (<- :line) 1))
      :line (any (if-not "\n" 1))
      }
    text))

# Returns an array containing the number scanned from text,
# (length text) number of times.
(defn multi-scan-number [text]
  # Number is wrapped in a table to make it a reference value,
  # which will later be set to 0 when read once.
  # This prevents double-counting numbers.
  (def num @{:number (scan-number text)})
  (map (fn [_] num) (range (length text))))

(def line-peg (peg/compile
  ~{
    :main (some :token)
    :token (+
      (/ "." nil)
      (/ (<- (some (range "09"))) ,multi-scan-number)
      (/ (<- 1) ,keyword))
    }))

(defn parse-line [line]
  (flatten # Flatten is needed as the output will have numbers inside arrays.
    (peg/match line-peg line)))

(defn count-around [grid i j]
  (var nums-around @[])
  # Looks at the 3x3 square around a symbol
  (loop [di :range-to [-1 1]
         dj :range-to [-1 1]]
      (do
        (def item (get-in grid [(+ i di) (+ j dj)]))
        (when (= (type item) :table)
          (array/push nums-around (item :number))
          (set (item :number) 0)))) # Sets to 0 to avoid double-counting.
  nums-around)

(defn main [& args]
  (var result 0)
# Grid structure should look like
# [ [ item ... item ]
#   [ item ... item ]
#   [ item ... item ] ]
# where item = number | :symbol | nil
  (def grid (map parse-line (split-lines input)))
  (loop [[i row] :pairs grid
         [j item] :pairs row]
      (when (= item :*) # Only count around gears.
        (def gear-nums (filter
          |(not (= 0 $)) # Remove extra zeroes introduced to avoid double-counting.
          (count-around grid i j)))
        (if (= (length gear-nums) 2)
          (+= result (* ;gear-nums)))))
  (print result))
