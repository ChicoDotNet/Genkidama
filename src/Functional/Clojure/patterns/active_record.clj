(defn active-record-pattern [] (let [t (atom {})] (swap! t assoc 7 "Ada")(= "Ada" (@t 7))))
