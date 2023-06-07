(ns nes.instructions
  (:require [nes.utils :as utils]))

(defn read-byte [^bytes memory address]
  (aget memory address))

(defn write-byte
  "Sets byte into memory. Data argument must be a byte"
  [^bytes memory address data]
  (aset-byte memory address data))

(defn overflowed-adc?
  "Returns whether overflow occurred or not for ADC instruction. Arguments must be long."
  [acc mem res]
  (bit-test
   (bit-and
    (bit-xor acc res)
    (bit-not (bit-xor acc mem)))
   7))

(defn execute-adc [state inst values]
  (let [extra-cycles (or (:extra-cycles values) 0)
        acc (Byte/toUnsignedInt (:reg-a state))
        fetched (Byte/toUnsignedInt (:value values))
        res (+ (utils/bool-to-int (:carry state))
               acc
               fetched)
        wrapped-res (bit-and 0xFF res)]
    {:pc (+ (:bytes inst) (:pc state))
     :cycles-elapsed (+ extra-cycles (:cycles inst))
     :reg-a (unchecked-byte wrapped-res)
     :carry (> res 0xFF)
     :zero (zero? wrapped-res)
     :negative (bit-test wrapped-res 7)
     :overflow (overflowed-adc? acc fetched res)}))

(defn execute-and [state inst values]
  (let [extra-cycles (or (:extra-cycles values) 0)
        res (bit-and (:reg-a state) (:value values))]
    {:reg-a (unchecked-byte res)
     :pc (+ (:bytes inst) (:pc state))
     :cycles-elapsed (+ extra-cycles (:cycles inst))
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-asl [state inst values]
  (let [val (:value values)
        res (unchecked-byte (bit-shift-left val 1))
        handle-acc-mem (fn [] (if (= (:mode inst) :acc)
                                {:reg-a res
                                 :zero (zero? res)}
                                (do (write-byte (:memory state) (:mem-address values) res)
                                    nil)))]
    (merge {:pc (+ (:bytes inst) (:pc state))
            :cycles-elapsed (:cycles inst)
            :carry (bit-test val 7)
            :negative (bit-test res 7)}
           (handle-acc-mem))))

(defn branch-instruction-handler [state inst values test]
  (let [pc (+ (:bytes inst) (:pc state))
        next-pc (if test (bit-and 0xFFFF (+ pc (:value values)))
                    (bit-and 0xFFFF pc))
        extra-cycles (+ (if test
                          (+ 1 (if (= (bit-and 0xFF00 next-pc) (bit-and 0xFF00 pc))
                                 0 1))
                          0))]
    {:pc next-pc
     :cycles-elapsed (+ (:cycles inst) extra-cycles)}))

(defn execute-bcc [state inst values]
  (branch-instruction-handler state inst values (false? (:carry state))))

(defn execute-bcs [state inst values]
  (branch-instruction-handler state inst values (true? (:carry state))))

(defn execute-beq [state inst values]
  (branch-instruction-handler state inst values (true? (:zero state))))

(defn execute-bit [state inst values]
  (let [val (:value values)
        acc (:reg-a state)
        res (bit-and val acc)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :zero (zero? res)
     :overflow (bit-test val 6)
     :negative (bit-test val 7)}))

(defn execute-bmi [state inst values]
  (branch-instruction-handler state inst values (true? (:negative state))))

(defn execute-bne [state inst values]
  (branch-instruction-handler state inst values (false? (:zero state))))

(defn execute-bpl [state inst values]
  (branch-instruction-handler state inst values (false? (:negative state))))

(defn execute-brk [state inst _values]
  (let [pc (+ 1 (:pc state))
        sp (:sp state)
        next-pc (utils/bytes-to-word
                 (read-byte (:memory state) 0xFFFF)
                 (read-byte (:memory state) 0xFFFE))
        status-byte (utils/status-to-byte (merge state {:interrupt-disable true
                                                        :break-command true}))]
    (write-byte (:memory state) (+ 0x100 sp) (utils/word-msb pc))
    (write-byte (:memory state) (+ 0x100 (- sp 1)) (utils/word-msb pc))
    (write-byte (:memory state) (+ 0x100 (- sp 2)) status-byte)
    {:pc next-pc
     :sp (- sp 3)
     :cycles-elapsed (:cycles inst)
     :interrupt-disable true
     :break-command false}))

(defn execute-bvc [state inst values]
  (branch-instruction-handler state inst values (false? (:overflow state))))

(defn execute-bvs [state inst values]
  (branch-instruction-handler state inst values (true? (:overflow state))))

(defn implied-instruction-handler [state inst kvs]
  (merge kvs {:pc (+ (:pc state) (:bytes inst))
              :cycles-elapsed (:cycles inst)}))

(defn execute-clc [state inst _values]
  (implied-instruction-handler state inst {:carry false}))

(defn execute-cld [state inst _values]
  (implied-instruction-handler state inst {:decimal-mode false}))

(defn execute-cli [state inst _values]
  (implied-instruction-handler state inst {:interrupt-disable false}))

(defn execute-clv [state inst _values]
  (implied-instruction-handler state inst {:overflow false}))

(defn execute-cmp [state inst values]
  (let [extra-cycles (or (:extra-cycles values) 0)
        val (Byte/toUnsignedInt (:value values))
        acc (Byte/toUnsignedInt (:reg-a state))
        res (- acc val)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (+ (:cycles inst) extra-cycles)
     :carry (>= acc val)
     :zero (zero? (bit-and 0xFF res))
     :negative (utils/negative? res)}))

(defn execute-cpx [state inst values]
  (let [val (Byte/toUnsignedInt (:value values))
        val-x (Byte/toUnsignedInt (:reg-x state))
        res (- val-x val)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :carry (>= val-x val)
     :zero (zero? (bit-and 0xFF res))
     :negative (utils/negative? res)}))

(defn execute-cpy [state inst values]
  (let [val (Byte/toUnsignedInt (:value values))
        val-y (Byte/toUnsignedInt (:reg-y state))
        res (- val-y val)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :carry (>= val-y val)
     :zero (zero? (bit-and 0xFF res))
     :negative (utils/negative? res)}))

(defn execute-dec [state inst values]
  (let [val (Byte/toUnsignedInt (:value values))
        res (bit-and 0xFF (- val 1))]
    (write-byte (:memory state) (:mem-address values) (unchecked-byte res))
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-dex [state inst _values]
  (let [reg-x (bit-and 0xFF (- (Byte/toUnsignedInt (:reg-x state)) 1))]
    (implied-instruction-handler state inst {:reg-x (unchecked-byte reg-x)
                                             :zero (zero? reg-x)
                                             :negative (bit-test reg-x 7)})))

(defn execute-dey [state inst _values]
  (let [reg-y (bit-and 0xFF (- (Byte/toUnsignedInt (:reg-y state)) 1))]
    (implied-instruction-handler state inst {:reg-y (unchecked-byte reg-y)
                                             :zero (zero? reg-y)
                                             :negative (bit-test reg-y 7)})))

(defn execute-eor [state inst values]
  (let [val (:value values)
        acc (:reg-a state)
        extra-cycles (or (:extra-cycles values) 0)
        res (bit-xor val acc)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (+ (:cycles inst) extra-cycles)
     :reg-a res
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-inc [state inst values]
  (let [val (Byte/toUnsignedInt (:value values))
        res (bit-and 0xFF (+ val 1))]
    (write-byte (:memory state) (:mem-address values) (unchecked-byte res))
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-inx [state inst _values]
  (let [reg-x (bit-and 0xFF (+ (Byte/toUnsignedInt (:reg-x state)) 1))]
    (implied-instruction-handler state inst {:reg-x (unchecked-byte reg-x)
                                             :zero (zero? reg-x)
                                             :negative (bit-test reg-x 7)})))

(defn execute-iny [state inst _values]
  (let [reg-y (bit-and 0xFF (+ (Byte/toUnsignedInt (:reg-y state)) 1))]
    (implied-instruction-handler state inst {:reg-y (unchecked-byte reg-y)
                                             :zero (zero? reg-y)
                                             :negative (bit-test reg-y 7)})))

(defn execute-jmp [_state inst values]
  (let [address (:mem-address values)]
    {:pc (bit-and 0xFFFF address)
     :cycles-elapsed (:cycles inst)}))

(defn execute-jsr [state inst values]
  (let [next-pc (:mem-address values)
        sp (:sp state)
        pc (- (+ (:pc state) (:bytes inst)) 1)
        low (utils/word-lsb pc)
        high (utils/word-msb pc)
        address (+ 0x100 sp)]
    (write-byte (:memory state) address high)
    (write-byte (:memory state) (- address 1) low)
    {:cycles-elapsed (:cycles inst)
     :pc (bit-and 0xFFFF next-pc)
     :sp (- sp 2)}))

(defn execute-lda [state inst values]
  (let [res (:value values)
        extra-cycles (or (:extra-cycles values) 0)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (+ (:cycles inst) extra-cycles)
     :reg-a res
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-ldx [state inst values]
  (let [res (:value values)
        extra-cycles (or (:extra-cycles values) 0)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (+ (:cycles inst) extra-cycles)
     :reg-x res
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-ldy [state inst values]
  (let [res (:value values)
        extra-cycles (or (:extra-cycles values) 0)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (+ (:cycles inst) extra-cycles)
     :reg-y res
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-lsr [state inst values]
  (let [val (:value values)
        res (unchecked-byte (unsigned-bit-shift-right (Byte/toUnsignedInt val) 1))
        handle-acc-mode (fn []
                          (if (= :acc (:mode inst))
                            {:reg-a res}
                            (do
                              (write-byte (:memory state) (:mem-address values) res)
                              nil)))]
    (merge {:pc (+ (:pc state) (:bytes inst))
            :cycles-elapsed (:cycles inst)
            :carry (bit-test val 0)
            :zero (zero? res)
            :negative (utils/negative? res)}
           (handle-acc-mode))))

(defn execute-nop [state inst _values]
  (implied-instruction-handler state inst {}))

(defn execute-ora [state inst values]
  (let [val (:value values)
        acc (:reg-a state)
        res (bit-or val acc)
        extra-cycles (or (:extra-cycles values) 0)]
    {:cycles-elapsed (+ extra-cycles (:cycles inst))
     :pc (+ (:bytes inst) (:pc state))
     :reg-a (unchecked-byte res)
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-pha [state inst _values]
  (let [acc (:reg-a state)
        sp (:sp state)
        address (+ 0x100 sp)]
    (write-byte (:memory state) address acc)
    (implied-instruction-handler state inst {:sp (- sp 1)})))

(defn execute-php [state inst _values]
  (let [sp (:sp state)
        address (+ 0x100 sp)]
    (write-byte (:memory state) address (utils/status-to-byte
                                         (merge state {:break-command true
                                                       :unused true})))
    (implied-instruction-handler state inst {:sp (- sp 1)
                                             :break-command false
                                             :unused true})))

(defn execute-pla [state inst _values]
  (let [sp (+ (:sp state) 1)
        val (read-byte (:memory state) (+ 0x100 sp))]
    (implied-instruction-handler state inst {:reg-a val
                                             :sp sp
                                             :zero (zero? val)
                                             :negative (bit-test val 7)})))

(defn execute-plp [state inst _values]
  (let [sp (+ (:sp state) 1)
        status-byte (read-byte (:memory state) (+ 0x100 sp))]
    (implied-instruction-handler state inst (merge
                                             (utils/byte-to-status status-byte)
                                             {:sp sp
                                              :break-command false
                                              :unused true}))))

(defn execute-rol [state inst values]
  (let [val (:value values)
        res (utils/set-bit (unchecked-byte (bit-shift-left val 1)) (:carry state) 0)
        handle-acc-mode (fn []
                          (if (= :acc (:mode inst))
                            {:reg-a res
                             :zero (zero? res)}
                            (do
                              (write-byte (:memory state) (:mem-address values) res)
                              nil)))]
    (merge {:pc (+ (:pc state) (:bytes inst))
            :cycles-elapsed (:cycles inst)
            :carry (bit-test val 7)
            :negative (bit-test res 7)}
           (handle-acc-mode))))

(defn execute-ror [state inst values]
  (let [val (:value values)
        res (unchecked-byte (utils/set-bit
                             (unchecked-byte (unsigned-bit-shift-right (Byte/toUnsignedInt val) 1))
                             (:carry state)
                             7))
        handle-acc-mode (fn []
                          (if (= :acc (:mode inst))
                            {:reg-a res
                             :zero (zero? res)}
                            (do
                              (write-byte (:memory state) (:mem-address values) res)
                              nil)))]
    (merge {:pc (+ (:pc state) (:bytes inst))
            :cycles-elapsed (:cycles inst)
            :carry (bit-test val 0)
            :negative (bit-test res 7)}
           (handle-acc-mode))))

(defn execute-rti [state inst _values]
  (let [sp (:sp state)
        status-byte (read-byte (:memory state) (+ 0x100 1 sp))
        pc-lsb (read-byte (:memory state) (+ 0x100 2 sp))
        pc-msb (read-byte (:memory state) (+ 0x100 3 sp))]
    (merge {:pc (utils/bytes-to-word pc-msb pc-lsb)
            :cycles-elapsed (:cycles inst)
            :sp (+ sp 3)}
           (merge (utils/byte-to-status status-byte)
                  {:break-command false
                   :unused true}))))

(defn execute-rts [state inst _values]
  (let [sp (:sp state)
        pc-lsb (read-byte (:memory state) (+ 0x100 1 sp))
        pc-msb (read-byte (:memory state) (+ 0x100 2 sp))]
    {:pc (+ 1 (utils/bytes-to-word pc-msb pc-lsb))
     :sp (+ 2 sp)
     :cycles-elapsed (:cycles inst)}))

(defn overflowed-sbc? 
  "Returns whether overflow occurred or not for SBC instruction. Arguments must be long."
  [acc mem res]
  (bit-test
   (bit-and
    (bit-xor res acc)
    (bit-xor mem res))
   7))

(defn execute-sbc [state inst values]
  (let [extra-cycles (or (:extra-cycles values) 0)
        fetched (Byte/toUnsignedInt (:value values))
        fetched-inv (bit-xor 0x00FF fetched)
        acc (Byte/toUnsignedInt (:reg-a state))
        res (+ (utils/bool-to-int (:carry state))
               acc
               fetched-inv)
        wrapped-res (bit-and res 0xFF)]
    {:reg-a (unchecked-byte wrapped-res)
     :cycles-elapsed (+ extra-cycles (:cycles inst))
     :pc (+ (:bytes inst) (:pc state))
     :carry (> res 0xFF)
     :zero (zero? wrapped-res)
     :overflow (overflowed-sbc? acc fetched-inv res)
     :negative (bit-test wrapped-res 7)}))

(defn execute-sec [state inst _values]
  (implied-instruction-handler state inst {:carry true}))

(defn execute-sed [state inst _values]
  (implied-instruction-handler state inst {:decimal-mode true}))

(defn execute-sei [state inst _values]
  (implied-instruction-handler state inst {:interrupt-disable true}))

(defn execute-sta [state inst values]
  (let [val (:reg-a state)
        address (:mem-address values)]
    (write-byte (:memory state) address val)
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)}))

(defn execute-stx [state inst values]
  (let [val (:reg-x state)
        address (:mem-address values)]
    (write-byte (:memory state) address val)
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)}))

(defn execute-sty [state inst values]
  (let [val (:reg-y state)
        address (:mem-address values)]
    (write-byte (:memory state) address val)
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)}))

(defn execute-tax [state inst _values]
  (let [acc (:reg-a state)]
    (implied-instruction-handler state inst {:reg-x acc
                                             :zero (zero? acc)
                                             :negative (utils/negative? acc)})))

(defn execute-tay [state inst _values]
  (let [acc (:reg-a state)]
    (implied-instruction-handler state inst {:reg-y acc
                                             :zero (zero? acc)
                                             :negative (utils/negative? acc)})))

(defn execute-tsx [state inst _values]
  (let [sp-val (unchecked-byte (:sp state))]
    (implied-instruction-handler state inst {:reg-x sp-val
                                             :zero (zero? sp-val)
                                             :negative (utils/negative? sp-val)})))

(defn execute-txa [state inst _values]
  (let [x-val (:reg-x state)]
    (implied-instruction-handler state inst {:reg-a x-val
                                             :zero (zero? x-val)
                                             :negative (utils/negative? x-val)})))

(defn execute-txs [state inst _values]
  (let [x-val (Byte/toUnsignedInt (:reg-x state))]
    (implied-instruction-handler state inst {:sp x-val})))

(defn execute-tya [state inst _values]
  (let [y-val (:reg-y state)]
    (implied-instruction-handler state inst {:reg-a y-val
                                             :zero (zero? y-val)
                                             :negative (utils/negative? y-val)})))
(defn execute-dop [state inst _values]
  {:pc (+ (:pc state) (:bytes inst))
   :cycles-elapsed (:cycles inst)})

(defn execute-top [state inst values]
  (let [extra-cycles (or (:extra-cycles values) 0)]
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (+ extra-cycles (:cycles inst))}))

(defn execute-lax [state inst values]
  (let [res (:value values)
        extra-cycles (or (:extra-cycles values) 0)]
    {:cycles-elapsed (+ extra-cycles (:cycles inst))
     :pc (+ (:bytes inst) (:pc state))
     :reg-x res
     :reg-a res
     :zero (zero? res)
     :negative (utils/negative? res)}))

(defn execute-sax [state inst values]
  (let [res (bit-and
             (Byte/toUnsignedInt (:reg-x state))
             (Byte/toUnsignedInt (:reg-a state)))
        address (:mem-address values)]
    (write-byte (:memory state) address (unchecked-byte res))
    {:cycles-elapsed (:cycles inst)
     :pc (+ (:bytes inst) (:pc state))}))

(defn execute-usbc [state inst values]
  (execute-sbc state inst values))

(defn execute-dcp [state inst values]
  (let [val (bit-and 0xFF (- (Byte/toUnsignedInt (:value values)) 1))
        acc (Byte/toUnsignedInt (:reg-a state))
        res (- acc val)]
    (write-byte (:memory state) (:mem-address values) (unchecked-byte val))
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :carry (>= acc val)
     :zero (zero? (bit-and 0xFF res))
     :negative (utils/negative? res)}))

(defn execute-isb [state inst values]
  (let [val (bit-and 0xFF (+ 1 (Byte/toUnsignedInt (:value values))))
        val-inv (bit-xor 0x00FF val)
        acc (Byte/toUnsignedInt (:reg-a state))
        res (+ (utils/bool-to-int (:carry state))
               acc
               val-inv)
        wrapped-res (bit-and res 0xFF)]
    (write-byte (:memory state) (:mem-address values) (unchecked-byte val))
    {:pc (+ (:bytes inst) (:pc state))
     :cycles-elapsed (:cycles inst)
     :reg-a (unchecked-byte wrapped-res)
     :carry (> res 0xFF)
     :zero (zero? wrapped-res)
     :overflow (overflowed-sbc? acc val-inv res)
     :negative (utils/negative? wrapped-res)}))

(defn execute-slo [state inst values]
  (let [val (:value values)
        acc (:reg-a state)
        val-shifted (unchecked-byte (bit-shift-left (Byte/toUnsignedInt val) 1))
        res (bit-or val-shifted acc)]
    (write-byte (:memory state) (:mem-address values) val-shifted)
    {:pc (+ (:bytes inst) (:pc state))
     :cycles-elapsed (:cycles inst)
     :reg-a (unchecked-byte res)
     :zero (zero? res)
     :negative (utils/negative? res)
     :carry (bit-test val 7)}))

(defn execute-rla [state inst values]
  (let [val (:value values)
        val-shifted (utils/set-bit
                     (unchecked-byte (bit-shift-left (Byte/toUnsignedInt val) 1))
                     (:carry state)
                     0)
        res (bit-and (:reg-a state) (unchecked-byte val-shifted))]
    (write-byte (:memory state) (:mem-address values) val-shifted)
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :reg-a (unchecked-byte res)
     :zero (zero? res)
     :negative (utils/negative? res)
     :carry (bit-test val 7)}))

(defn execute-sre [state inst values]
  (let [val (:value values)
        acc (:reg-a state)
        val-shifted (unchecked-byte (unsigned-bit-shift-right (Byte/toUnsignedInt val) 1))
        res (bit-xor val-shifted acc)]
    (write-byte (:memory state) (:mem-address values) val-shifted)
    {:pc (+ (:pc state) (:bytes inst))
     :cycles-elapsed (:cycles inst)
     :reg-a (unchecked-byte res)
     :zero (zero? res)
     :negative (utils/negative? res)
     :carry (bit-test val 0)}))

(defn execute-rra [state inst values]
  (let [val (:value values)
        val-shifted (unchecked-byte (utils/set-bit
                                     (unchecked-byte (unsigned-bit-shift-right (Byte/toUnsignedInt val) 1))
                                     (:carry state)
                                     7))
        acc (Byte/toUnsignedInt (:reg-a state))
        res (+ (utils/bool-to-int (bit-test val 0)) acc (Byte/toUnsignedInt val-shifted))
        wrapped-res (bit-and 0xff res)]
    (write-byte (:memory state) (:mem-address values) val-shifted)
    {:pc (+ (:bytes inst) (:pc state))
     :cycles-elapsed (:cycles inst)
     :reg-a (unchecked-byte wrapped-res)
     :carry (> res 0xff)
     :zero (zero? wrapped-res)
     :negative (utils/negative? wrapped-res)
     :overflow (overflowed-adc? acc (Byte/toUnsignedInt val-shifted) res)}))
