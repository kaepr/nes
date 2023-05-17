(ns nes.cartridge
  (:require [clojure.java [io :as io]])
  (:import (java.io ByteArrayOutputStream)))

(defn load-cartridge
  "Returns integer array of the cartridge located by `path`.
   These ints are unsigned values of the bytes read."
  [path]
  (with-open [xin (io/input-stream path)
              xout (ByteArrayOutputStream.)]
    (io/copy xin xout)
    (int-array (map #(Byte/toUnsignedInt %) (.toByteArray xout)))))

(comment
  (def rom (load-cartridge "../nes-test-roms/nestest.nes")))
