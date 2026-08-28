(defn mvc-pattern [] (let [m (atom 0) view #(str "count=" @m) before (view)] (swap! m inc)(and (= before "count=0")(= (view) "count=1"))))
