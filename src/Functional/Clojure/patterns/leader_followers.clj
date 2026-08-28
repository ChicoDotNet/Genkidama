(defn leader-followers-pattern [] (= ["worker-1:a" "worker-2:b" "worker-3:c"] (mapv #(str %1 ":" %2) ["worker-1" "worker-2" "worker-3"] ["a" "b" "c"])))
