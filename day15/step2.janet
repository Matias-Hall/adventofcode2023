(def input (slurp "input"))

(defn create-step
  [lbl operation &opt strength]
  {:label lbl
   :op operation
   :strength strength})

(defn parse-steps [text]
  (peg/match
    ~{
      :main (some (* :step (? ",")))
      :step (/ (* :label (+ :assign-op :remove-op)) ,create-step)
      :label (<- (some :a))
      :remove-op (* "-" (constant :remove))
      :assign-op (* "=" (constant :assign) :strength)
      :strength (/ (<- (some :d)) ,scan-number)}
    text))

# Named hash-aoc to avoid conflicting with hash function.
(defn hash-aoc
  [text]
  (var result 0)
  (each char text
    (+= result char)
    (*= result 17)
    (%= result 256))
  result)

(defn assign-lens
  [box lbl strength]
  (def index (if (nil? (get box lbl)) (length box) ((box lbl) :index)))
  (set (box lbl) @{:strength strength :index index}))

(defn remove-lens
  [box lbl]
  (unless (nil? (get box lbl))
    (def popped-index ((box lbl) :index))
    (set (box lbl) nil)
    (each lens box
      (if (> (lens :index) popped-index)
        (-- (lens :index))))))

(defn perform-step
  [boxes step]
  (case (step :op)
    :assign (assign-lens (boxes (hash-aoc (step :label))) (step :label) (step :strength))
    :remove (remove-lens (boxes (hash-aoc (step :label))) (step :label))))

(defn focusing-power
  [boxes]
  (var result 0)
  (eachp [box-num box] boxes
    (each {:index lens-index :strength strength} box
      # box-num and lens-index are increased since they are zero indexed.
      (def power (* (inc box-num) (inc lens-index) strength))
      (+= result power)))
  result)

(defn main [& args]
  (def boxes (table/new 256))
  (each i (range 256)
    (set (boxes i) @{}))
  (each step (parse-steps input)
    (perform-step boxes step))
  (print (focusing-power boxes)))

