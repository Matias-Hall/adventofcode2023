(defmacro memoize
  ```Memoize the given forms.

  This macro will evaluate the forms and remember the result, associated with whatever structure is in args.```
  [args & forms]
  # We generate a unique name for the memoization table,
  # so that it doesn't interfere with anything else.
  (def memo-table-name (gensym))
  # Dynamic variables are created with keyword names,
  # so we are converting the name to a keyword.
  (def memo-table (keyword memo-table-name))
  # Initialising the table. It is a dynamic variable
  # so that it persists across calls to the function,
  # which of course is the behaviour we want from a
  # memoized function.
  (setdyn memo-table @{})
  ~(do
     # If the passed arguments have not been memoized yet,
     # we call the given forms and put the result on the table.
     (when (nil? ((dyn ,memo-table) ,args))
       (put (dyn ,memo-table) ,args (do ,;forms)))
     (get (dyn ,memo-table) ,args)))

(defmacro fn-memo
  "Create a memoized function"
  [& more]
  (def args-index (find-index |(= :tuple (type $)) more))
  (def args (more args-index))
  (def name? (take args-index more))
  # -1 in subtraction so that args is not taken.
  (def forms (take (- args-index (length more) -1) more))
  ~(fn ,;name? ,args (memoize ,args ,;forms)))

(defmacro defn-memo
  "Define a memoized function"
  [name & more]
  (def args-index (find-index |(= :tuple (type $)) more))
  (def args (more args-index))
  (def before (take args-index more))
  # -1 in subtraction so that args is not taken.
  (def forms (take (- args-index (length more) -1) more))
  ~(defn ,name ,;before ,args (memoize ,args ,;forms)))
