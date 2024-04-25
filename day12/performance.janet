(defmacro measure
  "Measure the performance of function f in seconds + fractional seconds"
  [& forms]
  (with-syms [$before $after]
    ~(do
       (def ,$before (os/clock :monotonic))
       ,;forms
       (def ,$after (os/clock :monotonic))
       (- ,$after ,$before))))
