(use ../common/memoize)
(use ../common/performance)

(def input (slurp "input"))

(defn cell-index
  [row col]
  (defn expand-scalar [other]
    (if (number? other)
      (cell-index other other)
      other))

  (defn elementwise-op [a b op]
    (cell-index (op (a :row) (b :row)) (op (a :col) (b :col))))

  (def index-proto
    @{:+ (fn [self other] (elementwise-op self (expand-scalar other) +))
      :- (fn [self other] (elementwise-op self (expand-scalar other) -))
      :* (fn [self other] (elementwise-op self (expand-scalar other) *))
      :tuple (fn [self] [(self :row) (self :col)])})

  (table/setproto @{:row row :col col} index-proto))

(def grid-peg
  (peg/compile
    ~{:main (some (* :row (? "\n")))
      :row (group (some :cell))
      :cell (/ (<- :d) ,scan-number)}))

(defn opposite-direction [direction]
  (def opposites {:up :down
                  :down :up
                  :left :right
                  :right :left})
  (opposites direction))

(defn create-vector [x y]
  (defn add-v [self other]
    (create-vector
      (+ (self :x) (other :x))
      (+ (self :y) (other :y))))

  (defn sub-v [self &opt other]
    (default other (create-vector 0 0))
    (create-vector (- (self :x) (other :x)) (- (self :y) (other :y))))

  (defn mul-v [self other]
    (create-vector
      (* (self :x) other)
      (* (self :y) other)))

  (defn magnitude [self]
    (math/sqrt (+
                (* (self :x) (self :x))
                (* (self :y) (self :y)))))

  (defn normal [self]
    (def mag (:magnitude self))
    (create-vector (/ (self :x) mag) (/ (self :y) mag)))

  (def proto @{:+ add-v
               :- sub-v
               :* mul-v
               :magnitude magnitude
               :normal normal})

  (table/setproto @{:x x :y y} proto))

(defn to-vector [direction]
  (def transform {:up [0 -1]
                  :down [0 1]
                  :left [-1 0]
                  :right [1 0]})
  (create-vector ;(transform direction)))

(defn distance-between [grid u v]
  (assert (or (= (u :x) (v :x)) (= (u :y) (v :y))))
  (def direction (:normal (- v u)))
  (var dist 0)
  (each i (range 1 (inc (:magnitude (- v u))))
    (def cell (+ (* direction i) u))
    (+= dist (get-in grid [(cell :y) (cell :x)])))
  dist)

(defn get-adjacent [grid vertex]
  (def [row col dir] vertex)
  (def adj @[])
  (each d [:up :down :left :right]
    (when (and (not= dir d) (not= dir (opposite-direction d)))
      (each s (range 4 11)
        (def new-pos (+ (create-vector row col) (* (to-vector d) s)))
        (when (and
                (<= 0 (new-pos :x) (dec (length (grid 0))))
                (<= 0 (new-pos :y) (dec (length grid))))
          (def dist (distance-between grid (create-vector row col) new-pos))
          (array/push adj [[(new-pos :x) (new-pos :y) d] dist])))))
  adj)

(defn min-distance [vertices distances]
  (var min-key nil)
  (var min-dist math/inf)
  (loop [[vertex status] :pairs vertices
         :when (= status :unvisited)
         :let [dist (distances vertex)]]
    (when (< dist min-dist)
      (set min-dist dist)
      (set min-key vertex)))
  [min-key min-dist])

(defn dijkstra [grid initial target]
  (def distances @{initial 0})
  (def vertices @{initial :unvisited})
  (while true
    (def [v distance] (min-distance vertices distances))
    # (print "YY")
    # (pp v)
    (pp (length vertices))
    (match v
      nil (do (print "Not connected") (break))
      # YAY we are done
      ([x y & _] (= [x y] target)) (do (print distance) (break))
      (do
        (def adjacent (get-adjacent grid v))
        # (pp adjacent)
        (each [u delta] adjacent
          # (pp u)
          # (pp delta)
          (when (not= (vertices u) :visited)
            (set (vertices u) :unvisited)
            (def cur-dist (get distances u math/inf))
            (def new-dist (+ (distances v) delta))
            (when (< new-dist cur-dist)
              (set (distances u) new-dist))))
        # Set as visited
        (set (vertices v) :visited)))))

(defn a-star [grid initial target h]
  # distances* is the predicted distance f(v) = g(v) + h(v)
  (def distances* @{initial (h initial)})
  # distances is the actual distance g(v)
  (def distances @{initial 0})
  (def vertices @{initial :unvisited})
  (while true
    (def [v distance*] (min-distance vertices distances*))
    (def distance (distances v))
    (pp (length vertices))
    (match v
      nil (do (print "Not connected") (break))
      # YAY we are done
      ([x y & _] (= [x y] target)) (do (print distance) (break))
      (do
        (def adjacent (get-adjacent grid v))
        # (pp adjacent)
        (each [u delta] adjacent
          # (pp u)
          # (pp delta)
          (when (not= (vertices u) :visited)
            (set (vertices u) :unvisited)
            (def cur-dist (get distances u math/inf))
            (def new-dist (+ distance delta))
            (when (< new-dist cur-dist)
              (set (distances u) new-dist)
              (set (distances* u) (+ new-dist (h u))))))
        # Set as visited
        (set (vertices v) :visited)))))

(defn pythag-distance [a b]
  (let [dx (- (a 0) (b 0)) dy (- (a 1) (b 1))]
    (math/sqrt (+ (* dx dx) (* dy dy)))))

(defn main [& args]
  (def grid (peg/match grid-peg input))
  (def target [(dec (length (grid 0))) (dec (length grid))])
  (def time
    (measure
      (a-star
        grid
        [0 0] target
        |(* 2 (pythag-distance target $)))))
  (print time))

#input:
# 2pythag 1013 in 924.48
# dijkstra 1013 in 1043.85

# TODO: priority queue
