(def input (slurp "input"))

(defn get-field [id line]
  (peg/match
    ~{
      :main (* ,id :s+ :number)
      # Since all digits belong to the same number, we must first join each match, then scan it.
      :number (/ (/ (group (some (* (<- :d+) :s*))) ,string/join) ,scan-number)
      } line))

# We are using first, because there should only be one value in each array.
(def time (first
  (get-field "Time:" (first (string/split "\n" input)))))
(def distance (first
  (get-field "Distance:" ((string/split "\n" input) 1))))

# Gets the range of times that result in beating the race.
# If T is total race time (time) and t is the time spent pressing the button,
# Then v = t, as the velocity is equal (in magnitude) to the time spent pressing the button.
# So s = v(T-t) = t(T-t)
# We are trying to find s > r, where r is record distance (distance).
# This simplifies to the quadratic equation t^2 - Tt + r < 0.
# time-range simply solves this equation, constrains it to integers, and return the range of values.
(defn time-range [time distance]
    # Ceiling constrains the solutions to integers.
  # Discriminant
  (def disc (math/sqrt (- (* time time) (* 4 distance))))
  (var low-end (/ (- time disc) 2))
  (if (int? low-end)
    # Since inequality is < and not <=, we must increase low-end to the nearest integer.
    (+= low-end 1)
    (set low-end (math/ceil low-end)))
  (var high-end (/ (+ time disc) 2))
  (if (int? high-end)
    # Since inequality is < and not <=, we must decrease high-end to the nearest integer.
    (-= high-end 1)
    (set high-end (math/floor high-end)))
  (+ 1 (- high-end low-end)))

(defn main [& args]
  (print (time-range time distance)))
