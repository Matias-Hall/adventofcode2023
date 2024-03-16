(def input (slurp "input"))
#######
# Very different approach to step 1. In step 1 each seed was mapped through each mapping.
# This is unfeasable as the number of seeds is around the billions.
# Instead, we combine all the mappings into one "master" mapping that converts seed-to-location.
# Then we find the seed range that works as an input to the smallest rule in the mapping, and that's the solution.
#######

# Divides the input on empty lines.
(def input-blocks
  (peg/match
    ~{
      :main (any (* (<- :block) (? :empty-line)))
      :block (any (if-not :empty-line 1))
      :empty-line "\n\n"
      } input))

(defn get-seeds [line]
  (def seeds (peg/match
    ~{
      :main (* "seeds: " (some (group (* :number :number))))
      :number (/ (* (<- :d+) :s*),scan-number)
      } line))
  # Converts seed tuples from [start length] to [start end]
  (map |[(first $) (+ (first $) (last $))] seeds))

# Creates an array of 3-value arrays with the form:
# @[ (destination-start source-start range-length) ]
# Header info is unimportant as each map happens in order
# (i.e seed-to-soil is followed by soil-to-fertilizer, etc)
(def block-parse
  (peg/compile
    ~{
      :main (* :header (some :rule))
      :header (* (any (if-not "\n" 1)) 1)
      :rule (* (group (repeat 3 :number)) (? "\n"))
      :number (* (/ (<- :d+) ,scan-number) :s*)
      }))

(defn get-mapping [block]
  (def rules (peg/match block-parse block))
  # This function converts a raw rule of the form:
  # [destination-start source-start range-length]
  # to a struct of the form: { :source [source-start source-end] :dest [dest-start dest-end] }
  (defn normalise-rule [rule]
    (def [dest-start source-start range-length] rule)
    {:source [source-start (+ source-start range-length)]
     :dest [dest-start (+ dest-start range-length)]})
  (def mapping (map normalise-rule rules))

  ## We will check that all ranges exist from [0,inf)
  # Sort by source to check for missing ranges.
  (sort-by |(first ($ :source)) mapping)
  # This variable holds the end of the previous source range.
  # Initially set to 0 so that the range [0,x) is added if it does not exist.
  (var previous-end 0)
  (eachp [i rule] mapping
    (def start (first (rule :source)))
    (if (= previous-end start)
      # If the two numbers match, no need to add any range, set previous-end to new end of range.
      (set previous-end (last (rule :source)))
      (do
        # If the two numbers don't match we must add a default rule (x->x) from previous-end to start.
        (array/insert mapping i
          {:source [previous-end start] :dest [previous-end start]})
        # We also set previous-end to start, which is the end of the new rule in the mapping that we just added.
        (set previous-end start))))

  # After the loop above, previous-end will end up being the largest value with a range.
  # Now we add a final element to take care of the range to infinity.
  (array/push mapping {:source [previous-end math/inf] :dest [previous-end math/inf]})
  mapping)

# Cheks if a range is valid (the lower bound is smaller than the upper bound.
(defn range-valid? [a]
  (< (first a) (last a)))

# Given two ranges [a b] and [c d], finds the intersection of these ranges,
# or nil if they do not intersect.
(defn range-intersection [left right]
  (def [a b] left)
  (def [c d] right)
  (def low-bound (max a c))
  (def high-bound (min b d))
  (def intersection [low-bound high-bound])
  (if (range-valid? intersection)
    intersection
    nil))

# Gives the difference of range left and range right.
# Returns an array of ranges, as there may be zero, one, or two ranges returned.
(defn range-difference [left right]
  (def result @[])
  # The (potential) left part of the subtraction.
  (def left-bit [(first left) (first right)])
  # The (potential) right part of the subtraction.
  (def right-bit [(last right) (last left)])
  # left-bit is pushed last so that they are correctly sorted (smallest last).
  (when (range-valid? right-bit)
    (array/push result right-bit))
  (when (range-valid? left-bit)
    (array/push result left-bit))
  result)

# Shifts range by increment
(defn range-shift [a increment]
  [(+ increment (first a)) (+ increment (last a))])

(defn find-lowest-location [mapping seed-ranges]
  # Sort mapping by its destination ranges.
  # Then, we iterate over each rule, checking if the rule applies to any seed range.
  # If it does we have found our solution.
  (sort-by |(first ($ :dest)) mapping)
  (sort-by |(first $) seed-ranges)
  # Point at which to return, exiting the loop.
  (prompt :result
    (each {:source map-source :dest map-dest} mapping
      # truthy? is simplest way to remove all nils.
      (def answer (filter truthy? (map |(range-intersection map-source $) seed-ranges)))
      (unless (empty? answer)
        (def increment (- (first map-dest) (first map-source)))
        (return :result
          (+
           increment
           # First first takes out the first range from answer, if there were multiple ranges
           # the first one should be the lowest, as seed-ranges is ordered low-to-high.
           # The second first gets the start of the range, as this is the smallest value that applies.
           (first (first answer))))))))

(defn combine-maps [xy-map yz-map]
  # Sort xy-map by its destinations and yz-map by its sources, making it easy to find the matching dest->source
  # Reverse is used to that the maps behave as stacks, and we use array/pop to get each rule.
  (reverse! (sort-by |(first ($ :dest)) xy-map))
  (reverse! (sort-by |(first ($ :source)) yz-map))
  (def xz-map @[])
  (var xy-rule (array/pop xy-map))
  (var yz-rule (array/pop yz-map))
  (while (not (and (nil? xy-rule) (nil? yz-rule)))
    # Unboxing each range for easy access.
    (def { :source xy-input :dest xy-output} xy-rule)
    (def { :source yz-input :dest yz-output} yz-rule)

    # The part where the output of xy and the input of yz are the same
    # is the only bit where we can actually create a new rule.
    (def intersection (range-intersection xy-output yz-input))

    # (pp xy-rule)
    # (pp yz-rule)
    # (pp intersection)

    ### Here we are getting the parts of xy-output not covered by intersection
    #   and adding those back to xy-map
    (def xy-extras (range-difference xy-output intersection))
    (def xy-increment (- (first xy-output) (first xy-input)))
    (array/push xy-map
      ;(map # Creates rules for the extra parts.
         |{
            :source (range-shift $ (- xy-increment))
            :dest $}
         xy-extras))

    ### Here we are getting the parts of yz-input not covered by intersection
    #   and adding those back to yz-map
    (def yz-extras (range-difference yz-input intersection))
    (def yz-increment (- (first yz-output) (first yz-input)))
    (array/push yz-map
      ;(map # Creates rules for the extra parts.
         |{
            :source $
            :dest (range-shift $ yz-increment)}
         yz-extras))

    # Now we have both increments and intersection, with these we work out the new rule.
    (array/push xz-map
      { :source (range-shift intersection (- xy-increment))
       :dest (range-shift intersection (+ yz-increment))})

    # Get the next rules (might be nil, that's fine it exits the loop).
    (set xy-rule (array/pop xy-map))
    (set yz-rule (array/pop yz-map)))
  xz-map)

(defn main [& args]
  (def [seed-block first-map-block & blocks] input-blocks)
  (def seeds (get-seeds seed-block))
  # Initially, xy-map holds the first mapping.
  # After each block is iterated, xy-map gets updated to the full rule list.
  # I.e. at first it defines seeds-to-soils, but then it defines seeds-to-fertilizer,
  # and so on until seeds-to-location.
  (var xy-map (get-mapping first-map-block))
  (each block blocks
    (def yz-map (get-mapping block))
    (set xy-map (combine-maps xy-map yz-map)))
  (pp (find-lowest-location xy-map seeds)))
