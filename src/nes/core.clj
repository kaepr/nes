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
;(defonce cart-2 (cart/load-cartridge "../nes-test-roms/instr_test-v5/official_only.nes"))

(cart/load-nestest (:memory test-cpu-state) cartridge)
;(cart/load-rom (:memory test-cpu-state) cart-2)

(defn log-state [state inst operands]
  (let [pc (:pc state)
        ^bytes memory (:memory test-cpu-state)
        bytes-log (let [n-bytes (:bytes inst)]
                    (->> (range 0 n-bytes)
                         (map #(Byte/toUnsignedInt (aget memory (+ pc %))))
                         (map #(format "%02X" %))
                         (clojure.string/join " ")
                         (#(str % (clojure.string/join " " (repeat 10 ""))))
                         (#(subs % 0 9))))
        cycles (:cycles state)
        acc (Byte/toUnsignedInt (:reg-a state))
        x-reg (Byte/toUnsignedInt (:reg-x state))
        y-reg (Byte/toUnsignedInt (:reg-y state))
        status-reg (Byte/toUnsignedInt (cpu/status-to-byte state))
        sp (:sp state)]
    (format "%04X  %s %s     A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n" pc bytes-log (name (:name inst)) acc x-reg y-reg status-reg sp cycles)))

(defn log-nestest [^bytes memory]
  (let [val1 (Byte/toUnsignedInt (aget memory 0x0002))
        val2 (Byte/toUnsignedInt (aget memory 0x0003))]
    (format "%02X %02X\n" val1 val2)))

(defn run [init-state]
  (with-open [^Writer w (clojure.java.io/writer "log.txt")]
    (loop [state init-state]
      (let [opcode (cpu/fetch state)
            decoded-inst (cpu/decode opcode)
            inst (cpu/fetch-operands state decoded-inst)
            operands (cpu/handle-addressing-mode state inst)]
        (.write ^Writer w ^String (log-nestest (:memory state))))
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



