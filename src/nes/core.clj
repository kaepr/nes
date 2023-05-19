(ns nes.core
  {:nextjournal.clerk/toc true})

;(require '[nextjournal.clerk :as clerk])
;
;(clerk/serve! {:browse? true})
;(clerk/serve! {:watch-paths ["src/notebooks" "src/nes"]})

(require '[io.github.humbleui.ui :as ui])

(def ui
  (ui/default-theme {}
                    (ui/center
                      (ui/label "Hello from Humble UI! 👋"))))

(ui/start-app!
  (ui/window
    {:title "Humble 🐝 UI"}
    #'ui))

;; # Emulation Core

