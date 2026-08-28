(defn peer-to-peer-pattern [] (= ["peer-a>peer-b:block-42" "peer-a>peer-c:block-42"] (mapv #(str "peer-a>" % ":block-42") ["peer-b" "peer-c"])))
