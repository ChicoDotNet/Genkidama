(defn memento-pattern [] (let [state (atom "draft") snap @state] (reset! state "published")(reset! state snap)(= @state "draft")))
