(ns nes.misc
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [nes.cpu :as cpu]
   [nes.utils :as utils])
  (:import
   [java.io Writer]))

(defn log-state [state inst _operands]
  (let [pc (:pc state)
        ^bytes memory (:memory state)
        bytes-log (let [n-bytes (:bytes inst)]
                    (->> (range 0 n-bytes)
                         (map #(Byte/toUnsignedInt (aget memory (+ pc %))))
                         (map #(format "%02X" %))
                         (string/join " ")
                         (#(str % (string/join " " (repeat 10 ""))))
                         (#(subs % 0 9))))
        cycles (:cycles state)
        acc (Byte/toUnsignedInt (:reg-a state))
        x-reg (Byte/toUnsignedInt (:reg-x state))
        y-reg (Byte/toUnsignedInt (:reg-y state))
        status-reg (Byte/toUnsignedInt (utils/status-to-byte state))
        sp (:sp state)]
    (format "%04X  %s %s     A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n" pc bytes-log (name (:name inst)) acc x-reg y-reg status-reg sp cycles)))

(defn log-nestest [^bytes memory]
  (let [val1 (Byte/toUnsignedInt (aget memory 0x0002))
        val2 (Byte/toUnsignedInt (aget memory 0x0003))]
    (format "%02x %02x\n" val1 val2)))

(defn run
  "Used for logging system state. Used to make logs to for nestest etc."
  [init-state]
  (with-open [^Writer w (io/writer "log.txt")]
    (loop [state init-state]
      (let [opcode (cpu/fetch state)
            decoded-inst (cpu/decode opcode)
            inst (cpu/fetch-operands state decoded-inst)
            operands (cpu/handle-addressing-mode state inst)]
        (.write ^Writer w ^String (log-nestest (:memory state)))
        (.write ^Writer w ^String (log-state state inst operands)))
      (recur (cpu/run-instruction state)))))

