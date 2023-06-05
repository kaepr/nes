(ns nes.utils)

(defn bool-to-int [test]
  (if test 1 0))

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
