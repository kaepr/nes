(ns nes.bus
  (:require [nes.utils :as utils]))

(def MEMORY-SIZE 0x10000)
(def PPU-MEMORY-SIZE 0x4000)
(def STACK-POINTER-DEFAULT 0xFD)
(def STATUS-REG-DEFAULT 0x34)

(def default-state
  "NES state after power up sequence."
  (merge {:sp                STACK-POINTER-DEFAULT
          :pc                0
          :reg-a             (byte 0)
          :reg-x             (byte 0)
          :reg-y             (byte 0)
          :cycles            0
          :cycles-elapsed    0
          :memory            (byte-array MEMORY-SIZE (unchecked-byte 0x00))
          :ppu-memory        (byte-array PPU-MEMORY-SIZE (unchecked-byte 0x00))}
         (utils/byte-to-status STATUS-REG-DEFAULT)))

(defn bus-read
  "Returns a byte at the given address."
  [state address])

(defn bus-write
  "Write a byte to a given address. Handles mirroring."
  [state address val])

(defn ppu-read
  "Returns a byte at the given address."
  [state address])

(defn ppu-write
  "Write a byte to a given address. Handles mirroring."
  [state address val])

(defn create-state
  ([] (create-state nil))
  ([kvs] (merge default-state kvs)))


