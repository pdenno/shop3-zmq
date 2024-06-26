(ns shop3-zmq.util
  "Server utilities."
  (:require
   [clojure.core.unify      :as uni]
   [datahike.api            :as d]
   [datahike.pull-api       :as dp]
   [mount.core              :refer [defstate]]
   [taoensso.timbre         :as log]))

(defonce databases-atm (atom {}))

(defn register-db
  "Add a DB configuration."
  [k config]
  (log/info "Registering DB" k "config =" config)
  (swap! databases-atm #(assoc % k config)))

(defn deregister-db
  "Add a DB configuration."
  [k]
  (log/info "Deregistering DB" k)
  (swap! databases-atm #(dissoc % k)))

(def db-template
  "Hitchhiker file-based DBs follow this form."
  {:store {:backend :file :path "Provide a value!"} ; This is path to the database's root directory
   :keep-history? false
   :base-dir "Provide a value!"                    ; For convenience, this is just above the database's root directory.
   :recreate-dbs? false                             ; If true, it will recreate the system DB and project directories too.
   :schema-flexibility :write})

;;; https://cljdoc.org/d/io.replikativ/datahike/0.6.1545/doc/datahike-database-configuration
(defn db-cfg-map
  "Return a datahike configuration map for argument database (or its base).
     id   - a keyword uniquely identifying the DB in the scope of DBs.
     type - the type of DB configuration being make: (:project, :system, or :him, so far)"
  ([type] (if (= :project type)
            (throw (ex-info "projects need an ID." {}))
            (db-cfg-map type type)))
  ([type id]
   (let [base-dir (or (-> (System/getenv) (get "SCHEDULING_TBD_DB")) ; "/opt/scheduling" typically.
                      (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases."
                                           "\nCreate directories 'projects' and 'system' under it.") {})))
         db-dir (->> (case type
                       :system           "/system"
                       :project          (str "/projects/" (name id))
                       :planning-domains "/planning-domains"
                       :him              "/etc/other-dbs/him")
                     (str base-dir))]
     (-> db-template
         (assoc-in [:store :path] db-dir)
         (assoc    :base-dir base-dir)))))

(defn connect-atm
  "Return a connection atom for the DB."
  [k]
  (if-let [db-cfg (get @databases-atm k)]
    (if (d/database-exists? db-cfg)
      (d/connect db-cfg)
      (log/error "DB is registered but does not exist:" k))
    (throw (ex-info "No such DB" {:key k}))))

(defn datahike-schema
  "Create a Datahike-compatible schema from the above."
  [schema]
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             []
             schema))
;;; ToDo:
;;;  - cljs complains about not finding x/element-nss, which I don't see in the  0.2.0-alpha8 source at all.
;;;    (Yet it does work in clj!) I suppose reading xml isn't something I need in cljs, but it would be
;;;    nice to know what is going on here.
;;; ToDo: Get some more types in here, and in implementation generally.
(defn db-type-of
  "Return a Datahike schema :db/valueType object for the argument"
  [obj]
  (cond (string? obj)  :db.type/string
        (number? obj)  :db.type/number
        (keyword? obj) :db.type/keyword
        (map? obj)     :db.type/ref
        (boolean? obj) :db.type/boolean))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj))))

;;; {:db/id 3779}
(defn resolve-db-id
  "Return the form resolved, removing properties in filter-set,
   a set of db attribute keys, for example, #{:db/id}."
  [form conn-atm & {:keys [keep-set drop-set]
                    :or {drop-set #{:db/id}
                         keep-set #{}}}]
  (letfn [(resolve-aux [obj]
            (cond
              (db-ref? obj) (let [res (dp/pull @conn-atm '[*] (:db/id obj))]
                              (if (= res obj) nil (resolve-aux res)))
              (map? obj) (reduce-kv (fn [m k v]
                                      (cond (drop-set k)                                    m
                                            (and (not-empty keep-set) (not (keep-set k)))   m
                                            :else                                           (assoc m k (resolve-aux v))))
                                    {}
                                    obj)
              (vector? obj)      (mapv resolve-aux obj)
              (set? obj)    (set (mapv resolve-aux obj))
              (coll? obj)        (map  resolve-aux obj)
              :else  obj))]
    (resolve-aux form)))

(defn root-entities
  "Return a sorted vector of root entities (natural numbers) for all root entities of the DB."
  [conn-atm]
  (-> (d/q '[:find [?e ...] :where
             [?e]
             (not [_ _ ?e])]
           @conn-atm)
      sort
      vec))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

(defn not-nothing
  "Returns true if it is a collection and not empty or something else non-nil"
  [x]
  (if (seq? x)
    (not-empty x)
    x))

;;; https://gist.github.com/lnostdal/cc956e2a80dc49d8097b7c950f7213bd
(defn move-file
  "Move file/directory from source to target. Both are full pathnames.
   This will also replace the target file if it exists since REPLACE_EXISTING is included in the options at the end."
  [source target]
  (let [source-file (java.nio.file.Paths/get (java.net.URI/create (str "file://" source)))
        target-file (java.nio.file.Paths/get (java.net.URI/create (str "file://"  target)))]
    (java.nio.file.Files/move source-file target-file
                              (into-array java.nio.file.CopyOption
                                          [(java.nio.file.StandardCopyOption/ATOMIC_MOVE)
                                           (java.nio.file.StandardCopyOption/REPLACE_EXISTING)]))))

;;; Keep this around; it might get used eventually!
#_(defmacro report-long-running
  "Return the string from writing to *out* after this runs in a future."
  [[timeout] & body]
  `(-> (p/future (with-out-str ~@body))
       (p/await ~timeout)
       (p/then #(log/info "Long-running:" %))
       (p/catch #(log/warn "Long-running (exception):" %))))

(defn find-fact
  "Unify the fact (which need not be ground) to the fact-list"
  [fact fact-list]
  (some #(when (uni/unify fact %) %) fact-list))

;;; ============================ From util.cljc
;;; ToDo: The problem with output to log/debug might have to do with *err* not defined in cljs.
(defn custom-output-fn
  " - I don't want :hostname_ and :timestamp_ in the log output preface text.."
  ([data] (custom-output-fn nil data))
  ([opts data]
   (taoensso.timbre/default-output-fn opts (dissoc data :hostname_ :timestamp_))))

(defn config-log
  "Configure Timbre: set reporting levels and specify a custom :output-fn."
  [min-level]
  (if (#{:trace :debug :info :warn :error :fatal :report} min-level)
    (log/set-config!
     (-> log/*config*
         (assoc :output-fn #'custom-output-fn)
         (assoc :min-level [[#{"SHOP3-ZMQ-DEFAULT" "shop3-zmq.*" "user"} min-level]
                            [#{"datahike.*"} :error]
                            [#{"*"} :error]])))
     (log/error "Invalid timbre reporting level:" min-level)))

(defn default-min-log-level
  "Get the value of 'RM-DEFAULT' in (:min-level log/*config*), it designates
   the logging level for namespaces of shop3-zmq, and user."
  []
  (->> log/*config* :min-level (some #(when (contains? (first %) "SHOP3-ZMQ-DEFAULT") (second %)))))

(def max-duration
  "This is used in places where doing set-clock might not make sense."
  30000)

(def timeout-info "Used for calls to cljs-ajax and progress bar."
  (atom {:valid? false :max-millis max-duration :start-time nil :timeout-at nil}))

(defn invalidate-timeout-info
  []
  (swap! timeout-info #(assoc % :valid? false)))

(defn now [] (new java.util.Date))

(defn start-clock
  "Set the timeout-info object and return the argument."
  ([] (start-clock max-duration))
  ([max-millis]
   (swap! timeout-info
          #(let [now (now)]
             (assoc % :valid? true :max-millis max-millis :start-time (now) :timeout-at (+ now max-millis))))
   max-millis))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj))))

;;; {:db/id 3779}
(defn resolve-db-id
  "Return the form resolved, removing properties in filter-set,
   a set of db attribute keys, for example, #{:db/id}."
  ([form conn-atm] (resolve-db-id form conn-atm #{}))
  ([form conn-atm filter-set]
   (letfn [(resolve-aux [obj]
             (cond
               (db-ref? obj) (let [res (dp/pull @conn-atm '[*] (:db/id obj))]
                               (if (= res obj) nil (resolve-aux res)))
               (map? obj) (reduce-kv (fn [m k v] (if (filter-set k) m (assoc m k (resolve-aux v))))
                                     {}
                                     obj)
               (vector? obj)      (mapv resolve-aux obj)
               (set? obj)    (set (mapv resolve-aux obj))
               (coll? obj)        (map  resolve-aux obj)
               :else  obj))]
     (resolve-aux form))))

;;; -------------- Starting and stopping ----------------------
(defn init-util []
  (config-log :info))

(defstate util-state
  :start (init-util))
