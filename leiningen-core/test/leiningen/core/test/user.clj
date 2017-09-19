(ns leiningen.core.test.user
  (:use clojure.test
        leiningen.core.user)
  (:require [clojure.java.io :as io]))

(deftest resolving-repo-creds
  (with-redefs [credentials (constantly {#"^https://clojars\.org/.*"
                                         {:username "u" :password "p"
                                          :passphrase "looooong"
                                          :private-key-file "./somewhere"}})]
    (testing "Literal creds unmolested"
      (is (= (resolve-credentials {:url "https://clojars.org/repo"
                                   :username "easily" :password "stolen"})
             {:url "https://clojars.org/repo"
              :username "easily" :password "stolen"})))
    (testing "Lookup in environment"
      (with-redefs [getenv {"LEIN_USERNAME" "flynn"
                            "CUSTOMENV" "flotilla"}]
        (is (= (resolve-credentials {:url "https://clojars.org/repo"
                                     :username :env
                                     :password :env/customenv})
               {:url "https://clojars.org/repo"
                :username "flynn" :password "flotilla"}))))
    (testing "Check multiple locations"
      (with-redefs [getenv {"LEIN_USERNAME" "flynn"
                            "CUSTOMENV" "flotilla"}]
        (is (= (resolve-credentials {:url "https://clojars.org/repo"
                                     :username [:gpg :env]
                                     :password [:env/customenv :gpg]})
               {:url "https://clojars.org/repo"
                :username "u" :password "flotilla"}))))
    (testing "Custom keys unmolested (and :creds expanded)"
      (is (= (resolve-credentials {:url "https://clojars.org/repo"
                                   :creds :gpg
                                   :foo [:gpg "0x00D85767"]})
             {:url "https://clojars.org/repo"
              :username "u" :password "p"
              :passphrase "looooong" :private-key-file "./somewhere"
              :foo [:gpg "0x00D85767"]})))))

(defmacro with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh StringWriter.
   Returns the String created by any printing calls in the evaluated body.
   Equivalent to with-out-str, but re-binds *err* instead of *out*."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(deftest missing-credentials-file
  (with-redefs [leiningen-home (constantly "")
                io/file (constantly (java.io.File. ""))]
    (testing "Warning on missing GPG credentials file"
      (is (.contains (with-err-str (credentials-fn))
                     "WARNING: Missing GPG credentials file")))))

(deftest missing-credentials
  (with-redefs [getenv (constantly nil)]
    (alter-var-root #'leiningen.core.user/warn-missing-credentials-once
                    (constantly #'leiningen.core.user/warn-missing-credentials))
    (testing "Missing default environment credential"
      (is (.contains (with-err-str (resolve-credentials {:username :env}))
                     ":username as environment variable with name LEIN_USERNAME")))
    (testing "Missing specific environment credential"
      (is (.contains (with-err-str (resolve-credentials {:username :env/abc}))
                     ":username as environment variable with name ABC")))
    (testing "Missing GPG credential"
      (with-redefs [credentials (constantly {})]
        (is (.contains (with-err-str (resolve-credentials {:username :gpg}))
                       (str ":username as GPG credential with key :username "
                            "in credentials file:")))))))
