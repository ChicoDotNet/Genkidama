(ns builder)

(defn text-builder []
  {:reset (fn [] [])
   :add-title (fn [title parts] (conj parts (str "# " title)))
   :add-section (fn [heading body parts] (conj parts (str "## " heading) body))
   :build (fn [parts] (clojure.string/join "\n" parts))})

(defn html-builder []
  {:reset (fn [] [])
   :add-title (fn [title parts] (conj parts (str "<h1>" title "</h1>")))
   :add-section (fn [heading body parts]
                  (conj parts (str "<h2>" heading "</h2>") (str "<p>" body "</p>")))
   :build (fn [parts] (apply str parts))})

(defn build-availability-report [builder]
  (let [parts ((:reset builder))
        parts ((:add-title builder) "Service status" parts)
        parts ((:add-section builder) "Availability" "99.95%" parts)]
    ((:build builder) parts)))

(println (build-availability-report (text-builder)))
(println "---")
(println (build-availability-report (html-builder)))
