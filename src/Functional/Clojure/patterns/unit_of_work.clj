(defn unit-of-work-pattern [] (let [store(atom[])pending(atom[2 3])] (swap! store into @pending)(reset! pending [])(and (= @store [2 3])(empty? @pending))))
