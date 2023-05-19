(ns nes.core
  (:gen-class))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "NES Emulator"
  [& args]
  (greet {:name (first args)}))
