(ns example1)

(defprotocol UIFactory
  (create-button [this])
  (create-checkbox [this]))

(defrecord DarkFactory []
  UIFactory
  (create-button [_] (println "Dark Button"))
  (create-checkbox [_] (println "Dark Checkbox")))

(defrecord LightFactory []
  UIFactory
  (create-button [_] (println "Light Button"))
  (create-checkbox [_] (println "Light Checkbox")))

(defn create-ui-components [factory]
  (create-button factory)
  (create-checkbox factory))

;; Usage
(create-ui-components (->DarkFactory))
(create-ui-components (->LightFactory))
