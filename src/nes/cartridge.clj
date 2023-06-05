(ns nes.cartridge
  (:require [clojure.java [io :as io]])
  (:import (java.io ByteArrayOutputStream)))

(defn load-cartridge
  "Returns byte array of the cartridge located by `path`."
  [path]
  (with-open [xin (io/input-stream path)
              xout (ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

(defonce PRG-ROM-BANK1 0x8000)
(defonce PRG-ROM-BANK2 0xC000)

(defn load-rom [memory cartridge]
  (let [offset 0x10]
    (doseq [idx (range 0 0x4000)]
      (let [val (aget cartridge (+ offset idx))]
        (do
          (aset-byte memory (+ PRG-ROM-BANK1 idx) val))))))

(defn load-nestest [memory cartridge]
  (let [offset 0x10]
    (doseq [idx (range 0 0x4000)]
      (let [val (aget cartridge (+ offset idx))]
        (do
          (aset-byte memory (+ PRG-ROM-BANK1 idx) val)
          (aset-byte memory (+ PRG-ROM-BANK2 idx) val))))))

(comment
  (def rom (load-cartridge "../nes-test-roms/nestest.nes")))
