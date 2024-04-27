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

(defn push-left
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
  (array/push result ;space)
  result)

(defn count-load
  [row]
  (defn load [[i ith]] (if (= "O" ith) (inc i) 0))
  (+ ;(map load (pairs row))))

(defn main [& args]
  (->> platform
       # Platform is transposed to make the columns easier to work with.
       (transpose)
       # All rolling stones are positioned correctly.
       (map push-left)
       # The rows are reversed so that what was originally the top-most row has the highest load.
       (map reverse)
       (map count-load)
       (reduce + 0)
       (print)))
