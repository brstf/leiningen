(ns leiningen.core.test.helper)

(defn abort-msg
  "Catches main/abort thrown by calling f on its args and returns its error
 message."
  [f & args]
  (with-out-str
    (binding [*err* *out*]
      (try
        (apply f args)
        (catch clojure.lang.ExceptionInfo e
          (when-not (:exit-code (ex-data e))
            (throw e)))))))

(defmacro with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh StringWriter.
   Returns the String created by any printing calls in the evaluated body.
   Equivalent to with-out-str, but re-binds *err* instead of *out*."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))
