(def input (slurp "input"))

# Returns all elements after the first one in an array/tuple.
(defmacro tail [list]
  ~(array/slice ,list 1 -1))

# Divides the input on empty lines.
(def input-blocks
  (peg/match
    ~{
      :main (any (* (<- :block) (? :empty-line)))
      :block (any (if-not :empty-line 1))
      :empty-line "\n\n"
      } input))

(defn get-seeds [line]
  (peg/match
    ~{
      :main (* "seeds: " (some (* :number :s*)))
      :number (/ (<- :d+) ,scan-number)
      } line))

# Creates an array of 3-value arrays with the form:
# @[ (destination-range-start source-range-start range-length) ]
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

(defn parse-block [block]
  (peg/match block-parse block))

# Finds the corresponding rule in rules and adds the value.
(defn apply-rules [list rules]
  # Apparently, tables don't guarantee order of their keys/values,
  # so we must create an array of keys and sort it ourselves.
  (def rule-keys (sort (keys rules)))
  (defn apply-rule [value]
    (def increment (->
      # Finds the largest key that is smaller than value.
      # For example, if value = 5, and rules is {0 4 10 -2},
      # this will find 0, as the value 5 must be modified by that rule.
      # If the value is larger than all keys, then the last key is used.
      (- (find-index |(< value $) rule-keys (length rule-keys)) 1)
      rule-keys
      rules))
    (+ value increment))
  (map apply-rule list))

(defn main [& args]
  # Initially result holds seeds. This value will be iteratively transformed by the rules of each block.
  (var result (get-seeds (first input-blocks)))
  # Using tail since the first block contains the seeds, the rest are mappings.
  (each block (tail input-blocks)
    (def rules (parse-block block))
    # Capacity based on the fact that each rule should introduce two entries to the table,
    # One for the range's beginning, another for its end. This is an overestimate, but probably fine.
    # rule-list is a table of form:
    # { starting-range increment } where increment applies to all values beginning on starting-range,
    # until the next starting-range (next largest key in table).
    (def rule-list (table/new (* 2 (length rules))))
    # Table always starts with range from 0 onwards being +0.
    # This might be overriden by one of the rules (which is fine).
    (set (rule-list 0) 0)
    (each [dest source range-length] rules
      (set (rule-list source) (- dest source))
      # Checking so that we don't accidentally override a rule.
      (when (nil? (rule-list (+ source range-length)))
        # We create this additional rule because the mapping should only take effect withing range-length.
        (set (rule-list (+ source range-length)) 0)))
    (set result (apply-rules result rule-list)))
  (print (min ;result)))
