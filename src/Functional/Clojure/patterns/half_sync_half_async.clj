(defn half-sync-half-async-pattern [] (= ["done:job-1" "done:job-2" "done:job-3"] (mapv #(str "done:" %) ["job-1" "job-2" "job-3"])))
