(defn make-proxy [backend]
  (let [cache (atom {})
        subject-created (atom false)]
    (fn [id]
      (if-let [value (get @cache id)]
        value
        (do
          (when-not @subject-created
            (swap! backend update :created inc)
            (reset! subject-created true))
          (swap! backend update :fetches inc)
          (let [value (str "doc(" id ")")]
            (swap! cache assoc id value)
            value))))))

(let [backend (atom {:created 0 :fetches 0})
      get-document (make-proxy backend)
      first (get-document 42)
      second (get-document 42)]
  (println (format "backend=%d;fetches=%d;first=%s;second=%s"
                   (:created @backend) (:fetches @backend) first second)))
