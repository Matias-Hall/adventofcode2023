(defmacro measure
  "Measure the performance of the macro body in seconds + fractional seconds"
  [& forms]
  (with-syms [$before $after]
    ~(upscope
       (def ,$before (os/clock :monotonic))
       ,;forms
       (def ,$after (os/clock :monotonic))
       (- ,$after ,$before))))
