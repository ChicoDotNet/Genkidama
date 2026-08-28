(defn pac-pattern [] (= ["child:view=42" "root:view=42"] (mapv #(str % ":view=42") ["child" "root"])))
