(ns nes.core
  (:require [cljfx.api :as fx]
            [nes.cpu :as cpu]
            [nes.cartridge :as cart])
  (:gen-class)
  (:import (java.io Writer)))

(set! *warn-on-reflection* true)

(defn -main
  "NES Emulator"
  [& args]
  (println "NES Emulator"))

(def test-cpu-state (cpu/create-cpu {:pc 0xC000 :cycles 7}))

(defonce cartridge (cart/load-cartridge "../nes-test-roms/other/nestest.nes"))

(cart/load-nestest (:memory test-cpu-state) cartridge)

(defn run [init-state]
  (with-open [^Writer w (clojure.java.io/writer "log.txt")]
    (loop [state init-state]
      (let [opcode (cpu/fetch state)
            decoded-inst (cpu/decode opcode)
            inst (cpu/fetch-operands state decoded-inst)
            operands (cpu/handle-addressing-mode state inst)])
        ;(.write ^Writer w ^String (log-nestest (:memory state))))
        ;(.write ^Writer w ^String (log-state state inst operands)))
      (recur (cpu/run-instruction state)))))

(comment
  (println "Running CPU...")
  (run test-cpu-state))

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
