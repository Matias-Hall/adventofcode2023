(def input (slurp "input"))

# Null-coalescing operator stolen from Kotlin.
(defn ?: [nillable dflt]
  (if (nil? nillable)
    dflt
    nillable))

(defn parse-maze [text]
  (peg/match
    ~{
      :main (some :line)
      :line (* (group (some :token)) :s*)
      :token (/
        (+
          (/ "|" [:N :S])
          (/ "-" [:W :E])
          (/ "L" [:N :E])
          (/ "J" [:N :W])
          (/ "7" [:S :W])
          (/ "F" [:S :E])
          (/ "." [])
          (/ "S" :start))
          # (* "S"
          #    (group (*
          #      (line)
          #      (column)))))
        # Wrapping each value in a table with the directions and distance
        ,|@{:directions $ :distance 0})
      } text))

# Pair of opposite directions, defined for easy iteration.
(def direction-pairs
  [
   [:N :S]
   [:S :N]
   [:W :E]
   [:E :W]])

(defn tuple+ [a b]
  (assert (= (length a) (length b)))
  # Recasting as tuple.
  (tuple
    ;(seq [i :range [0 (length a)]]
       (+ (a i) (b i)))))

(defn get-direction [direction]
  (case direction
    :N [-1 0]
    :S [1 0]
    :E [0 1]
    :W [0 -1]))

# Converts all the directions to references to the correct pipes.
(defn link-pipes [maze]
  (loop [[i row] :pairs maze
         [j pipe] :pairs row
         :when (not= :start (pipe :directions))]
    (set
      (pipe :directions)
      (map
        (fn [direction]
          (get-in maze (tuple+ (get-direction direction) [i j])))
        (pipe :directions))))
  maze)

# from value passed so when know what value (not) to follow.
(defn follow-directions [pipe from]
  # Using an inner function for the tail call optimisation.
  # Otherwise the program had a stack overflow error.
  (defn inner-func [pipe from pipe-loop]
    (array/push pipe-loop pipe)
      (def [follow]
        (seq [follow :in (pipe :directions)
              # Making sure not to cycle forever by not following the pipe we come from.
              :when (not= follow from)
              :when (not= :start (follow :directions))]
          follow))
      (if (nil? follow)
        pipe-loop
        (do
          (set (follow :distance) (inc (pipe :distance)))
              # Add to the array of pipes in the main loop and return.
          (inner-func follow pipe pipe-loop))))
  (inner-func pipe from @[]))


(defn traverse-maze [maze]
  # Finds the (row column) of start.
  (def [start-location]
    (seq [[i row] :pairs maze
          [j value] :pairs row
          :when (= :start (value :directions))]
      [i j]))
  (def start (get-in maze start-location))
  (prompt :pipe-loop
    (each [towards must-have] direction-pairs
      (def adjacent (get-in maze (tuple+ (get-direction towards) start-location)))
      (when (find |(= $ start) (?: (get adjacent :directions) []))
        (set (adjacent :distance) 1)
        (return :pipe-loop (follow-directions adjacent start))))))


(defn main [& args]
  (->> input
    parse-maze
    link-pipes
    traverse-maze
    # The farthest distance from the start will be the length of the pipe loop divided by 2.
    length
    (* 0.5)
    (math/ceil)
    print))
