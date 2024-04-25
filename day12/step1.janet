(use ../common/performance)
(use ../common/common)

(def input (slurp "input"))

(def spring-parse
  (peg/compile
    ~{
      :main (* :springs " " :groups)
      :springs (group (some (+ :region :working)))
      :region (<- (some (set "#?")))
      # Not capturing working springs because we only care about them as a way of splitting regions.
      :working (some ".")
      :groups (group (some (* :digit (? ","))))
      :digit (/ (<- :d+) ,scan-number)}))

(defn get-springs [line]
  (peg/match spring-parse line))

# Checks if a region of springs (e.g. "###") is valid.
# Validity is achieved if the region has no question marks
# and its length matches the passed number n.
(defn region-valid? [region n]
  (and
    (all (complement nil?) [region n])
    (string/check-set "#" region)
    (= n (length region))))

# reduce-regions checks for any regions at the start/end of the springs array that are already valid.
# These do not affect the validity of the rest of the springs, so can be safely removed (simplifying problem).
(defn reduce-regions [springs groups]
  (when (region-valid? (first springs) (first groups))
    (array/remove springs 0)
    (array/remove groups 0)
    # Calling again because more reduction might now be possible.
    # (The second element becomes the first).
    (reduce-regions springs groups))

  (when (region-valid? (last springs) (last groups))
    (array/pop springs)
    (array/pop groups)
    # Calling again because more reduction might now be possible.
    # (The second-to-last element becomes the last).
    (reduce-regions springs groups)))

(defn take-range [region n]
  (cond
    (= (length region) 0) :skipped
    (< (length region) n) :invalid
    (= (length region) n) ""
    (> (length region) n)
      (if (= (chr "?") (region n))
        (string/slice region (inc n))
        :invalid)))

# Returns a tuple of all possible values for a region of broken springs to
# start within a region. This is all the indices up to (and including) the
# first # of the region. This is because skipping a broken spring means
# the regions and groups won't match.
(defn valid-starting-indices [region]
  (range (inc (?: (string/find "#" region) (length region)))))

(defn no-more-broken [springs]
  (all |(string/check-set "?" $) springs))

(defn find-arrangements [springs groups]
  (match [(length springs) (length groups)]
    [0 0] 1 # Both are empty so arrangement was successful.
    [_ 0] (if (no-more-broken springs) 1 0) # If all other regions can be converted to working springs, it was successful.
    [0 _] 0 # There are still groups to match, so arrangement failed.
    (let [[first-region & springs] springs
          [first-group & groups] groups]
      (var arrangements 0)
      (each skip (valid-starting-indices first-region)
        (def new-region (take-range (string/slice first-region skip) first-group))
        (+= arrangements
          (case new-region
            :skipped (find-arrangements springs [first-group ;groups])
            :invalid 0
            "" (find-arrangements springs groups)
            (find-arrangements [new-region ;springs] groups))))
      arrangements)))

(defn main [& args]
  (def time
    (measure
      (var result 0)
      (each line (split-lines input)
        (def [springs groups] (get-springs line))
        (reduce-regions springs groups)
        (+= result (find-arrangements springs groups)))
      (print result)))
  (print time))
