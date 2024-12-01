(defmacro memoize
  ```Memoize the given forms.

  This macro will evaluate the forms and remember the result, associated with whatever structure is in args.```
  [args & forms]
  # We generate a unique name for the memoization table,
  # so that it doesn't interfere with anything else.
  (def $memo-table (gensym))
  # Initialising the table. It is a dynamic variable
  # so that it persists across calls to the function,
  # which of course is the behaviour we want from a
  # memoized function.
  (setdyn $memo-table @{ :value @{} })
  ~(do
     # If the passed arguments have not been memoized yet,
     # we call the given forms and put the result on the table.
     (when (nil? (,$memo-table ,args))
       # TODO: table values can't be set to nil, so if (do ,;forms)
       # evaluates to nil, this result is not memoized.
       # Potentially, we could choose some sentinel value like :nil,
       # which we convert nil _to_ before calling `put` and which
       # we convert nil _from_ after calling `get`.
       # However, this makes the value :nil itself impossible to represent.
       # We could also use the generated symbol in `$memo-table` as the sentinel value.
       # It is guaranteed not to exist anywhere else. Potentially convert to keyword
       # beforehand so that it is clear this is meant to be a value and not a symbol.
       # An alternative is to have an additional list of all values of args seen,
       # but this complicates the entire code.
       # Of course, the last alternative is to say that this is the intended behaviour.
       # Usually, nil is returned from functions that perform some side effect (e.g. print)
       # so by refusing to memoize that result, we still allow side effect.
       # Hence, nil could be used to explicitly tell the macro "do not memoize this".
       (put ,$memo-table ,args (do ,;forms)))
     (get ,$memo-table ,args)))

(defmacro fn-memo
  "Create a memoized function"
  [& more]
  (def args-index (find-index |(= :tuple (type $)) more))
  (def args (more args-index))
  (def name? (take args-index more))
  # -1 in subtraction so that args is not taken.
  (def forms (take (- args-index (length more) -1) more))
  # ``Inside a quasiquote, the idiom (as-macro ,my-custom-macro arg1 arg2...)
  # can be used to avoid unwanted variable capture of my-custom-macro.''
  ~(fn ,;name? ,args (as-macro ,memoize ,args ;forms)))

(defmacro defn-memo
  "Define a memoized function"
  [name & more]
  (def args-index (find-index |(= :tuple (type $)) more))
  (def args (more args-index))
  (def before (take args-index more))
  # -1 in subtraction so that args is not taken.
  (def forms (take (- args-index (length more) -1) more))
  # ``Inside a quasiquote, the idiom (as-macro ,my-custom-macro arg1 arg2...)
  # can be used to avoid unwanted variable capture of my-custom-macro.''
  ~(defn ,name ,;before ,args (as-macro ,memoize ,args ,;forms)))
