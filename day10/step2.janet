(def input (slurp "input"))

# Null-coalescing operator stolen from Kotlin.
(defn ?: [nillable dflt]
  (if (nil? nillable)
    dflt
    nillable))

(defn create-pipe [row column directions]
  # Rows and columns are 1-indexed, so we decrease both.
  @{:directions directions :location [(dec row) (dec column)]})

(defn parse-maze [text]
  (peg/match
    ~{
      :main (some :line)
      :line (* (group (some :token)) :s*)
      :token (/
        (*
         (line)
         (column)
         (+
          (/ "|" [:N :S])
          (/ "-" [:W :E])
          (/ "L" [:N :E])
          (/ "J" [:N :W])
          (/ "7" [:S :W])
          (/ "F" [:S :E])
          (/ "." [])
          (/ "S" :start)))
        ,create-pipe)
      } text))

# Pair of opposite directions, defined for easy iteration.
(def direction-pairs
  {
   :N :S
   :S :N
   :W :E
   :E :W})

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

(defn get-turn [from to]
  # All other combinations are nil (neither right nor left).
  (match [from to]
    [:N :E] :R
    [:N :W] :L
    [:S :E] :L
    [:S :W] :R
    [:E :N] :L
    [:E :S] :R
    [:W :N] :R
    [:W :S] :L))

(defn add-turns [pipe from]
  (set (pipe :L) @[])
  (set (pipe :R) @[])
  (var extra nil)
  (loop [direction :in direction-pairs
         # Go through directions that aren't part of the pipe.
         :when (not (find |(= direction $) (pipe :directions)))]
    (def turn (get-turn from direction))
    # If turn is nil, it is straight.
    # In this cases, we should put it wherever the other direction went.
    # Since this might be the second direction, we have to store it and put it later.
    (if (nil? turn)
      (set extra direction)
      (array/push (pipe turn) direction)))
  # Putting the extra in the array that is not empty.
  (unless (nil? extra)
    (array/push
      # Choose the array that is not empty.
      (if (empty? (pipe :L))
        (pipe :R)
        (pipe :L))
      extra)))

# This functions does three things:
# 1. It collects all the pipes in the main loop.
# 2. It adds which directions a pipe in the main loop considers to be left or right.
#    For example, coming northwards, "|" would consider :W to be left and :E to be right.
#    Coming westwards, "J" would consider :S and :E to be right, and nothing to be left.
# 3. It creates a list of :L and :R turns for the main loop.
#    This is later used to figure out if the inside is left or right.
(defn follow-directions [maze pipe from]
  (defn inner-func [maze pipe from result]
    (array/push (result :main-loop) pipe)
    (unless (= :start (pipe :directions))
      (add-turns pipe from))
    (set (pipe :tag) :loop)
    # We are only getting the first value, because there should _only be_ one value
    # (or none in which case follow is nil, which means the loop is done).
    (def [follow]
      (seq [:when (not= :start (pipe :directions))
            follow :in (pipe :directions)
            :when (not= follow (direction-pairs from))]
        follow))
    (if (nil? follow)
      result
      (do
        (array/push (result :turns) (get-turn from follow))
        (inner-func
          maze
          (get-in maze (tuple+ (pipe :location) (get-direction follow)))
          follow
          result))))
  (inner-func
    maze
    pipe
    from
    { :main-loop @[]
     :turns @[] }))


(defn traverse-maze [maze]
  # Finds start
  (def [[start start-location]]
    (seq [row :in maze
          value :in row
          :when (= :start (value :directions))
          :let [location (value :location)]]
      [value location]))
  (prompt :pipe-loop
    (eachp [towards must-have] direction-pairs
      (def adjacent (get-in maze (tuple+ (get-direction towards) start-location)))
      (when (find |(= $ must-have) (?: (get adjacent :directions) []))
        # Returning so that only the first exit from start is followed,
        # otherwise we would go over the loop twice.
        (return :pipe-loop (follow-directions maze adjacent towards))))))


(defn count-turns [turns]
  (def right (count |(= :R $) turns))
  (def left (count |(= :L $) turns))
  (if (> right left)
    :R
    :L))

# This function recursively goes over all pipes and tags them as inside.
(defn tag-areas [maze pipe-loop inside]
  # Spread tag recursively tags all untagged pipes around it.
  (defn spread-tag [pipe tag]
    (set (pipe :tag) tag)
    (each direction direction-pairs
      (def adjacent (get-in maze (tuple+ (pipe :location) (get-direction direction))))
      # nil adjacent gets replaced with a dummy "does not exist" pipe.
      # Simply most concise way to write this.
      (when (nil? ((?: adjacent {:tag :dne}) :tag))
        (spread-tag adjacent tag))))
  (loop [pipe :in pipe-loop
         # Make sure not to iterate over nil by replacing it with an empty tuple.
         direction :in (?: (pipe inside) [])]
    (def follow (get-in maze (tuple+ (get-direction direction) (pipe :location))))
    (when (nil? ((?: follow {:tag :dne}) :tag))
      (spread-tag follow :inside))))

(defn main [& args]
  (def maze (parse-maze input))
  (def {:main-loop main-loop :turns turns} (traverse-maze maze))
  (tag-areas maze main-loop (count-turns turns))
  (->> maze
    flatten # Flattening so that we can go through all pipes at once.
    (filter |(= :inside ($ :tag)))
    length
    print))
