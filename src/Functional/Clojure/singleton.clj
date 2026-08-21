(defonce registry (atom {:count 0}))

(defn registry-instance [] registry)

(let [first (registry-instance)
      second (registry-instance)]
  (swap! first update :count inc)
  (println (str "same=" (if (identical? first second) "true" "false")))
  (println (str "count=" (:count @second))))
