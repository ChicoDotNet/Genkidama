(defn make-device [name]
  {:power-on #(str name ":on")
   :mute #(str name ":muted")})

(defn activate-basic [device]
  ((:power-on device)))

(defn activate-mute [device]
  ((:mute device)))

(let [tv (make-device "TV")
      radio (make-device "Radio")]
  (println (str "basic-tv=" (activate-basic tv)))
  (println (str "basic-radio=" (activate-basic radio)))
  (println (str "mute-tv=" (activate-mute tv)))
  (println (str "mute-radio=" (activate-mute radio))))
