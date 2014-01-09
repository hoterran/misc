(ns my-test.demo
  (:gen-class
    :methods [^{:static true} [getMessage [String] String]]))

(defn -getMessage [name]
  (str "Hello, " name "!"))
