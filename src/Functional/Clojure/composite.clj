(defn file-node [bytes]
  {:kind :file :bytes bytes})

(defn folder-node [& children]
  {:kind :folder :children (vec children)})

(defn node-size [node]
  (if (= :file (:kind node))
    (:bytes node)
    (reduce + (map node-size (:children node)))))

(let [readme (file-node 2)
      docs (folder-node (file-node 3) (file-node 5))
      root (folder-node readme docs)]
  (println (str "leaf=" (node-size readme)))
  (println (str "docs=" (node-size docs)))
  (println (str "root=" (node-size root))))
