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

# Finds the period from :__(A/Z) to :__Z,
# this period is a whole number multiple of the period of directions,
# otherwise the cycle might not actually repeat.
# We are assuming that the solution ends in a whole number multiple of
# directions, instead of halfway through a "directions cycle".
# This assumption is right for this input set,
# but I don't think it should be (in general).
(defn find-cycle [start-point network directions]
  (def period (length directions))
  (var steps 0)
  (var current start-point)
  (prompt :foundZ
    (each direction (cycle-directions directions)
      (set current (follow-direction (network current) direction))
      (++ steps)
      (if (and
            (= (chr "Z") (last current))
            (= (% steps period)))
        (return :foundZ))))
  steps)


(defn main [& args]
  # Underscore for ignoring the empty line.
  (def [direction-line _ & node-lines] (split-lines input))
  (def network @{})
  (each line node-lines
    (def [parent & children] (get-node line))
    (set (network parent) children))
  (def directions (get-directions direction-line))
  (def starts (filter |(= (chr "A") (last $)) (keys network)))
  (var result 1)
  (each start starts
    (set result
      # The result will be the least common multiple of all
      # periods for each start.
      (math/lcm
        result
        (find-cycle start network directions))))
  (print result))
