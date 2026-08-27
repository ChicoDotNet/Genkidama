(defn get-style [pool key style]
  (if-let [existing (get @pool key)]
    existing
    (do
      (swap! pool assoc key style)
      style)))

(let [pool (atom {})
      red1 (get-style pool ["Inter" 12 "red"] {:font "Inter" :size 12 :color "red"})
      red2 (get-style pool ["Inter" 12 "red"] {:font "Inter" :size 12 :color "red"})
      blue (get-style pool ["Inter" 12 "blue"] {:font "Inter" :size 12 :color "blue"})]
  (assert (= "blue" (:color blue)))
  (println (str "styles=" (count @pool)
                ";shared=" (if (identical? red1 red2) "true" "false")
                ";text=ABC")))
