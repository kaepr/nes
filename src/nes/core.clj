(ns nes.core
  (:require [nes.cpu :as cpu]
            [nes.bus :as bus]
            [nes.cartridge :as cart])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn -main
  "NES Emulator"
  [& args]
  (println "NES Emulator"))

(def state (bus/create-state))

(comment
  (defonce cartridge (cart/load-cartridge "../../downloads/nes-roms/Donkey Kong (World) (Rev A)/Donkey Kong (World) (Rev A).nes")))

(comment
  (cart/load-nrom-0 state cartridge))
