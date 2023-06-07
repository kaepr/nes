(ns nes.core
  (:require [cljfx.api :as fx]
            [nes.cpu :as cpu]
            [nes.bus :as bus]
            [nes.cartridge :as cart])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main
  "NES Emulator"
  [& args]
  (println "NES Emulator"))

(def state (bus/create-state))

(defonce cartridge (cart/load-cartridge "../../downloads/nes-roms/Donkey Kong (World) (Rev A)/Donkey Kong (World) (Rev A).nes"))

(cart/load-nrom-0 state cartridge)

(comment
  (fx/on-fx-thread
   (fx/create-component
    {:fx/type :stage
     :showing true
     :title "Cljfx example"
     :width 300
     :height 100
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :alignment :center
                    :children [{:fx/type :label
                                :text "Hello world"}]}}})))
