(ns user
  "For REPL-based start/stop of the server."
  (:require
   [clojure.pprint]
   [clojure.string]
   [clojure.spec.alpha :as s]
   [clojure.tools.namespace.repl :as tools-ns :refer [disable-reload! refresh clear set-refresh-dirs]]
   [expound.alpha :as expound]
   [mount.core :as mount]
   [lambdaisland.classpath.watch-deps :as watch-deps]      ;; hot loading for deps
   [shop3-zmq.planner :refer [plan-server]]
   [taoensso.timbre :as log]))

;;; If you get stuck do: (clojure.tools.namespace.repl/refresh)

;; uncomment to enable hot loading for deps
(watch-deps/start! {:aliases [:dev :test]})

(alter-var-root #'s/*explain-out* (constantly expound/printer))
(add-tap (bound-fn* clojure.pprint/pprint))
(set-refresh-dirs "src/shop3_zmq" "test/shop3_zmq")  ; put as many as you need here

(defn start
  "Start the web server"
  []
  (let [res (mount/start)
        info (str "   " (clojure.string/join ",\n    " (:started res)))]
    (log/info "started:\n" info)))

(defn stop
  "Stop the web server"
  []
  (mount/stop))

(defn restart
  "Stop, reload code, and restart the server. If there is a compile error, use:

  ```
  (tools-ns/refresh)
  ```

  to recompile, and then use `start` once things are good."
  []
  (stop)
  (java.lang.Thread/sleep 2000) ; Planner needs this.
  (tools-ns/refresh :after 'user/start))
