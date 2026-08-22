(defn clone-profile [profile]
  (assoc profile :features (vec (:features profile))))

(defn describe [profile]
  (str (:name profile) ": " (clojure.string/join "," (:features profile))))

(let [original {:name "orders" :features ["metrics"]}
      base-clone (clone-profile original)
      canary (-> base-clone
                 (assoc :name "orders-canary")
                 (update :features conj "tracing"))]
  (println (str "original=" (describe original)))
  (println (str "clone=" (describe canary))))
