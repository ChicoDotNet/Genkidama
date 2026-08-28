(defn mvp-pattern [] (let [m (atom 0) v (atom "")] (swap! m inc)(reset! v (str "count=" @m))(and (= @m 1)(= @v "count=1"))))
