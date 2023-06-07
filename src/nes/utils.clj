(ns nes.utils)

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

(defn log-state [state inst _operands]
  (let [pc (:pc state)
        ^bytes memory (:memory state)
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
        status-reg (Byte/toUnsignedInt (status-to-byte state))
        sp (:sp state)]
    (format "%04X  %s %s     A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n" pc bytes-log (name (:name inst)) acc x-reg y-reg status-reg sp cycles)))

(defn log-nestest [^bytes memory]
  (let [val1 (Byte/toUnsignedInt (aget memory 0x0002))
        val2 (Byte/toUnsignedInt (aget memory 0x0003))]
    (format "%02x %02x\n" val1 val2)))

(defn word-lsb [val]
  (unchecked-byte (bit-and val 0xFF)))

(defn word-msb [val]
  (unchecked-byte (bit-and 0xFF (unsigned-bit-shift-right val 8))))

(defn negative? [val] (bit-test val 7))
