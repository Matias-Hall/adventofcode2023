(use ../common/common)

(def input (slurp "input"))

(def node-parse
  (peg/compile
    ~{
      :main (* :token " = (" :token ", " :token ")")
      :token (/ (<- (repeat 3 :w)) ,keyword)
      }))

(defn get-node [line]
  (peg/match node-parse line))

# Gets an array of :L :R directions.
(defn get-directions [line]
  (peg/match ~(some (/ (<- (+ "L" "R")) ,keyword)) line))

# Creates a fiber that cycles through the directions forever.
(defn cycle-directions [directions]
  (fiber/new
    (fn []
      (forever (map yield directions)))))

# Returns the correct direction from the choices array.
(defn follow-direction [choices direction]
  (case direction
    :L (first choices)
    :R (last choices)))

(defn main [& args]
  # Underscore for ignoring the empty line.
  (def [direction-line _ & node-lines] (split-lines input))
  (def network @{})
  (each line node-lines
    (def [parent & children] (get-node line))
    (set (network parent) children))
  (def directions (get-directions direction-line))
  (var current :AAA)
  (var result 0)
  (prompt :foundZZZ
    (each direction (cycle-directions directions)
      (set current (follow-direction (network current) direction))
      (++ result)
      (if (= :ZZZ current)
        (return :foundZZZ))))
  (print result))
