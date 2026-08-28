(defn command-pattern [] (let [cs [(partial + 50) #(- % 20)] b (reduce #(%2 %1) 100 cs)] (and (= b 130) (= ((second cs) 150) 130))))
