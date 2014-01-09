(defproject my-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
   :repositories {"sonartype snapshots" "https://oss.sonatype.org/content/repositories/snapshots"}
   :dependencies [[org.clojure/clojure "1.5.1"] 
	[clj-http "0.5.5"] 
	[org.clojure/java.jdbc "0.0.4"]
	[mysql/mysql-connector-java "5.1.25"]
	[org.clojure/core.async "0.1.0-SNAPSHOT"]
	]	
  :main ^:skip-aot my-test.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
