(defn monitor-object-pattern [] (let [v (atom 0) lock (Object.) add #(locking lock (swap! v + %))] (add 2)(add 3)(= @v 5)))
