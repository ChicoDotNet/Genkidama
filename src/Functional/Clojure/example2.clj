(ns example2)

(defprotocol DatabaseFactory
  (connect [this])
  (query [this]))

(defrecord PostgresFactory []
  DatabaseFactory
  (connect [_] (println "Connecting to PostgreSQL"))
  (query [_] (println "Querying PostgreSQL")))

(defrecord MySQLFactory []
  DatabaseFactory
  (connect [_] (println "Connecting to MySQL"))
  (query [_] (println "Querying MySQL")))

(defn use-database [factory]
  (connect factory)
  (query factory))

;; Usage
(use-database (->PostgresFactory))
(use-database (->MySQLFactory))
