(defn create-postgres []
  {:connect #(println "PostgreSQL connect")
   :query #(println "PostgreSQL query")})

(defn create-mysql []
  {:connect #(println "MySQL connect")
   :query #(println "MySQL query")})

(defn use-database [create-database]
  (let [database (create-database)]
    ((:connect database))
    ((:query database))))

(use-database create-postgres)
(use-database create-mysql)
