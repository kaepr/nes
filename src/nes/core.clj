(ns nes.core
  {:nextjournal.clerk/toc true})
(require '[nextjournal.clerk :as clerk])

(clerk/serve! {:browse? true})
(clerk/serve! {:watch-paths ["src/notebooks" "src/nes"]})

;; # Emulation Core

