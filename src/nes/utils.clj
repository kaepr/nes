(ns nes.utils)

(defn bool-to-int [test]
  (if test 1 0))

(def status-bit-vec [:carry :zero :interrupt-disable :decimal-mode :break-command :unused :overflow :negative])

(defn status-to-byte [state]
  (let [coll (map-indexed (fn [idx itm]
                            (bit-shift-left (bool-to-int (itm state)) idx))
                          status-bit-vec)
        res (reduce bit-or coll)]
    (unchecked-byte res)))

(defn byte-to-status [status-byte]
  (let [coll (map (fn [idx]
                    {(nth status-bit-vec idx) (bit-test status-byte idx)}) (range 0 8))]
    (reduce merge {} coll)))

(defn bytes-to-word [msb lsb]
  (let [msb (Byte/toUnsignedInt msb)
        lsb (Byte/toUnsignedInt lsb)]
    (bit-or (bit-shift-left msb 8) lsb)))

(defn set-bit [val test pos]
  (if test
    (bit-set val pos)
    (bit-clear val pos)))

(defn word-lsb [val]
  (unchecked-byte (bit-and val 0xFF)))

(defn word-msb [val]
  (unchecked-byte (bit-and 0xFF (unsigned-bit-shift-right val 8))))

(defn negative? [val] (bit-test val 7))
