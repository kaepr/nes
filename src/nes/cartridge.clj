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

(comment
  (def rom (load-cartridge "../nes-test-roms/nestest.nes")))
