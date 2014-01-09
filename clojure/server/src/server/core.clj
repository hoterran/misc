(ns server.core
  (:use server.socket)
  (:gen-class)
)
(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])

(defn echo-server [port]
  (letfn 
    [(echo [in out] 
        (binding [*in* (BufferedReader. (InputStreamReader. in)) *out* (OutputStreamWriter. out)]
		  (loop []
		    (let [input (read-line)] (print input) (flush))
		    (recur)
		  )
		)
	)]
    (create-server port echo)
  )
)

(defn -main
  [& args]
  (println "args:" args)
  (let [c (count args) port (if (== c 0) 8080 (.intValue (Integer/valueOf (nth args 0))))]
	  (echo-server port)
  )
)
