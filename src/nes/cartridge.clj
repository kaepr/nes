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

(def PRG-ROM-BANK1 0x8000)
(def PRG-ROM-BANK2 0xC000)

(defn load-rom
  "Load cartridge into bus memory."
  [^bytes memory ^bytes cartridge]
  (let [offset 0x10]
    (doseq [idx (range 0 0x4000)]
      (let [val (aget cartridge (+ offset idx))]
        (aset-byte memory (+ PRG-ROM-BANK1 idx) val)))))

(defn load-nrom-0
  "Loads NROM0 based roms."
  [state ^bytes cartridge]
  (let [offset 0x10
        ^bytes memory (:memory state)
        ^bytes ppu-memory (:ppu-memory state)]
    (doseq [idx (range 0x2000)]
      (let [val (aget cartridge idx)]
        (aset-byte ppu-memory idx val)))
    (doseq [idx (range 0 0x4000)]
      (let [val (aget cartridge (+ offset idx))]
        (aset-byte memory (+ 0xC000 idx) val)))))

(defn load-nestest [^bytes memory ^bytes cartridge]
  (let [offset 0x10]
    (doseq [idx (range 0 0x4000)]
      (let [val (aget cartridge (+ offset idx))]
        (aset-byte memory (+ PRG-ROM-BANK1 idx) val)
        (aset-byte memory (+ PRG-ROM-BANK2 idx) val)))))

(comment
  (def rom (load-cartridge "../nes-test-roms/nestest.nes")))

