(defn null-object-pattern [] (let [null-log(constantly "") real-log #(str "log:" %)] (and (= ""(null-log "x"))(= "log:x"(real-log "x")))))
