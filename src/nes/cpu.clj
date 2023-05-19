(ns nes.cpu)

(def memory-size 65536)

(def state {:sp                0
            :pc                0
            :reg-a             (byte 0)
            :reg-x             (byte 0)
            :reg-y             (byte 0)
            :carry             false
            :zero              false
            :interrupt-disable false
            :dec-mode          false
            :break-command     false
            :overflow          false
            :negative          false
            :memory            (byte-array memory-size)})

(defn fetch
  "Returns byte pointed by the program counter."
  [{^bytes memory :memory ; fixes reflection warning
    pc             :pc}]
  (aget memory pc))

(defn create-inst-map [op bytes cycles mode name]
  {:opcode op
   :bytes  bytes
   :cycles cycles
   :mode   mode
   :name   name})

(defn decode
  "Returns a map of the instruction data using given opcode.
  The input opcode is a signed byte.
  Source: https://www.nesdev.org/obelisk-6502-guide/reference.html"
  [opcode]
  (let [op (Byte/toUnsignedInt opcode)]
    (cond
      ;; ADC
      (= op 0x69) (create-inst-map op 2 2 :imm :ADC)
      (= op 0x65) (create-inst-map op 2 3 :zero :ADC)
      (= op 0x75) (create-inst-map op 2 4 :zero-x :ADC)
      (= op 0x6d) (create-inst-map op 3 4 :abs :ADC)
      (= op 0x7d) (create-inst-map op 3 4 :abs-x :ADC)
      (= op 0x79) (create-inst-map op 3 4 :abs-y :ADC)
      (= op 0x61) (create-inst-map op 2 6 :ind-x :ADC)
      (= op 0x71) (create-inst-map op 2 5 :ind-y :ADC)
      ;; AND
      (= op 0x29) (create-inst-map op 2 2 :imm :AND)
      (= op 0x25) (create-inst-map op 2 3 :zero :AND)
      (= op 0x35) (create-inst-map op 2 4 :zero-x :AND)
      (= op 0x2d) (create-inst-map op 3 4 :abs :AND)
      (= op 0x3d) (create-inst-map op 3 4 :abs-x :AND)
      (= op 0x39) (create-inst-map op 3 4 :abs-y :AND)
      (= op 0x21) (create-inst-map op 2 6 :ind-x :AND)
      (= op 0x31) (create-inst-map op 2 5 :ind-y :AND)
      ;; ASL
      (= op 0x0a) (create-inst-map op 1 2 :acc :ASL)
      (= op 0x06) (create-inst-map op 2 5 :zero :ASL)
      (= op 0x16) (create-inst-map op 2 6 :zero-x :ASL)
      (= op 0x0e) (create-inst-map op 3 6 :abs :ASL)
      (= op 0x1e) (create-inst-map op 3 7 :abs-x :ASL)
      ;; BCC
      (= op 0x90) (create-inst-map op 2 2 :rel :BCC)
      ;; BCS
      (= op 0xb0) (create-inst-map op 2 2 :rel :BCS)
      ;; BEQ
      (= op 0xf0) (create-inst-map op 2 2 :rel :BEQ)
      ;; BIT
      (= op 0x24) (create-inst-map op 2 3 :zero :BIT)
      (= op 0x2c) (create-inst-map op 3 4 :abs :BIT)
      ;; BMI
      (= op 0x30) (create-inst-map op 2 2 :rel :BMI)
      ;; BNE
      (= op 0xd0) (create-inst-map op 2 2 :rel :BNE)
      ;; BPL
      (= op 0x10) (create-inst-map op 2 2 :rel :BPL)
      ;; BRK
      (= op 0x00) (create-inst-map op 1 7 :imp :BRK)
      ;; BVC
      (= op 0x50) (create-inst-map op 2 2 :rel :BVC)
      ;; BVS
      (= op 0x70) (create-inst-map op 2 2 :rel :BVS)
      ;; CLC
      (= op 0x18) (create-inst-map op 1 2 :imp :CLC)
      ;; CLD
      (= op 0xd8) (create-inst-map op 1 2 :imp :CLD)
      ;; CLI
      (= op 0x58) (create-inst-map op 1 2 :imp :CLI)
      ;; CLV
      (= op 0xb8) (create-inst-map op 1 2 :imp :CLV)
      ;; CMP
      (= op 0xc9) (create-inst-map op 2 2 :imm :CMP)
      (= op 0xc5) (create-inst-map op 2 3 :zero :CMP)
      (= op 0xd5) (create-inst-map op 2 4 :zero-x :CMP)
      (= op 0xcd) (create-inst-map op 3 4 :abs :CMP)
      (= op 0xdd) (create-inst-map op 3 4 :abs-x :CMP)
      (= op 0xd9) (create-inst-map op 3 4 :abs-y :CMP)
      (= op 0xc1) (create-inst-map op 2 6 :ind-x :CMP)
      (= op 0xd1) (create-inst-map op 2 5 :ind-y :CMP)
      ;; CPX
      (= op 0xe0) (create-inst-map op 2 2 :imm :CPX)
      (= op 0xe4) (create-inst-map op 2 3 :zero :CPX)
      (= op 0xec) (create-inst-map op 3 4 :abs :CPX)
      ;; CPY
      (= op 0xc0) (create-inst-map op 2 2 :imm :CPY)
      (= op 0xc4) (create-inst-map op 2 3 :zero :CPY)
      (= op 0xcc) (create-inst-map op 3 4 :abs :CPY)
      ;; DEC
      (= op 0xc6) (create-inst-map op 2 5 :zero :DEC)
      (= op 0xd6) (create-inst-map op 2 6 :zero-x :DEC)
      (= op 0xce) (create-inst-map op 3 6 :abs :DEC)
      (= op 0xde) (create-inst-map op 3 7 :abs-x :DEC)
      ;; DEX
      (= op 0xca) (create-inst-map op 1 2 :imp :DEX)
      ;; DEY
      (= op 0x88) (create-inst-map op 1 2 :imp :DEY)
      ;; EOR
      (= op 0x49) (create-inst-map op 2 2 :imm :EOR)
      (= op 0x45) (create-inst-map op 2 3 :zero :EOR)
      (= op 0x55) (create-inst-map op 2 4 :zero-x :EOR)
      (= op 0x4d) (create-inst-map op 3 4 :abs :EOR)
      (= op 0x5d) (create-inst-map op 3 4 :abs-x :EOR)
      (= op 0x59) (create-inst-map op 3 4 :abs-y :EOR)
      (= op 0x41) (create-inst-map op 2 6 :ind-x :EOR)
      (= op 0x51) (create-inst-map op 2 5 :ind-y :EOR)
      ;; INC
      (= op 0xe6) (create-inst-map op 2 5 :zero :INC)
      (= op 0xf6) (create-inst-map op 2 6 :zero-x :INC)
      (= op 0xee) (create-inst-map op 3 6 :abs :INC)
      (= op 0xfe) (create-inst-map op 3 7 :abs-x :INC)
      ;; INX
      (= op 0xe8) (create-inst-map op 1 2 :imp :INX)
      ;; INY
      (= op 0xc8) (create-inst-map op 1 2 :imp :INY)
      ;; JMP
      (= op 0x4c) (create-inst-map op 3 5 :abs :JMP)
      (= op 0x6c) (create-inst-map op 3 5 :ind :JMP)
      ;; JSR
      (= op 0x20) (create-inst-map op 3 6 :abs :JSR)
      ;; LDA
      (= op 0xa9) (create-inst-map op 2 2 :imm :LDA)
      (= op 0xa5) (create-inst-map op 2 3 :zero :LDA)
      (= op 0xb5) (create-inst-map op 2 4 :zero-x :LDA)
      (= op 0xad) (create-inst-map op 3 4 :abs :LDA)
      (= op 0xbd) (create-inst-map op 3 4 :abs-x :LDA)
      (= op 0xb9) (create-inst-map op 3 4 :abs-y :LDA)
      (= op 0xa1) (create-inst-map op 2 6 :ind-x :LDA)
      (= op 0xb1) (create-inst-map op 2 5 :ind-y :LDA)
      ;; LDX
      (= op 0xa2) (create-inst-map op 2 2 :imm :LDX)
      (= op 0xa6) (create-inst-map op 2 3 :zero :LDX)
      (= op 0xb6) (create-inst-map op 2 4 :zero-y :LDX)
      (= op 0xae) (create-inst-map op 3 4 :abs :LDX)
      (= op 0xbe) (create-inst-map op 3 4 :abs-y :LDX)
      ;; LDY
      (= op 0xa0) (create-inst-map op 2 2 :imm :LDY)
      (= op 0xa4) (create-inst-map op 2 3 :zero :LDY)
      (= op 0xb4) (create-inst-map op 2 4 :zero-x :LDY)
      (= op 0xac) (create-inst-map op 3 4 :abs :LDY)
      (= op 0xbc) (create-inst-map op 3 4 :abs-x :LDY)
      ;; LSR
      (= op 0x4a) (create-inst-map op 1 2 :acc :LSR)
      (= op 0x46) (create-inst-map op 2 5 :zero :LSR)
      (= op 0x56) (create-inst-map op 2 6 :zero-x :LSR)
      (= op 0x4e) (create-inst-map op 3 6 :abs :LSR)
      (= op 0x5e) (create-inst-map op 3 7 :abs-x :LSR)
      ;; NOP
      (= op 0xea) (create-inst-map op 1 2 :imp :NOP)
      ;; ORA
      (= op 0x09) (create-inst-map op 2 2 :imm :ORA)
      (= op 0x05) (create-inst-map op 2 3 :zero :ORA)
      (= op 0x15) (create-inst-map op 2 4 :zero-x :ORA)
      (= op 0x0d) (create-inst-map op 3 4 :abs :ORA)
      (= op 0x1d) (create-inst-map op 3 4 :abs-x :ORA)
      (= op 0x19) (create-inst-map op 3 4 :abs-y :ORA)
      (= op 0x01) (create-inst-map op 2 6 :ind-x :ORA)
      (= op 0x11) (create-inst-map op 2 5 :ind-y :ORA)
      ;; PHA
      (= op 0x48) (create-inst-map op 1 3 :imp :PHA)
      ;; PHP
      (= op 0x08) (create-inst-map op 1 3 :imp :PHP)
      ;; PLA
      (= op 0x68) (create-inst-map op 1 4 :imp :PLA)
      ;; PLP
      (= op 0x28) (create-inst-map op 1 4 :imp :PLP)
      ;; ROL
      (= op 0x2a) (create-inst-map op 1 2 :acc :ROL)
      (= op 0x26) (create-inst-map op 2 5 :zero :ROL)
      (= op 0x36) (create-inst-map op 2 6 :zero-x :ROL)
      (= op 0x2e) (create-inst-map op 3 6 :abs :ROL)
      (= op 0x3e) (create-inst-map op 3 7 :abs-x :ROL)
      ;; ROR
      (= op 0x6a) (create-inst-map op 1 2 :acc :ROR)
      (= op 0x66) (create-inst-map op 2 5 :zero :ROR)
      (= op 0x76) (create-inst-map op 2 6 :zero-x :ROR)
      (= op 0x6e) (create-inst-map op 3 6 :abs :ROR)
      (= op 0x7e) (create-inst-map op 3 7 :abs-x :ROR)
      ;; RTI
      (= op 0x40) (create-inst-map op 1 6 :imp :RTI)
      ;; RTS
      (= op 0x60) (create-inst-map op 1 6 :imp :RTS)
      ;; SBC
      (= op 0xe9) (create-inst-map op 2 2 :imm :SBC)
      (= op 0xe5) (create-inst-map op 2 3 :zero :SBC)
      (= op 0xf5) (create-inst-map op 2 4 :zero-x :SBC)
      (= op 0xed) (create-inst-map op 3 4 :abs :SBC)
      (= op 0xfd) (create-inst-map op 3 4 :abs-x :SBC)
      (= op 0xf9) (create-inst-map op 3 4 :abs-y :SBC)
      (= op 0xe1) (create-inst-map op 2 6 :ind-x :SBC)
      (= op 0xf1) (create-inst-map op 2 5 :ind-y :SBC)
      ;; SEC
      (= op 0x38) (create-inst-map op 1 2 :imp :SEC)
      ;; SED
      (= op 0xf8) (create-inst-map op 1 2 :imp :SED)
      ;; SEI
      (= op 0x78) (create-inst-map op 1 2 :imp :SEI)
      ;; STA
      (= op 0x85) (create-inst-map op 2 3 :zero :STA)
      (= op 0x95) (create-inst-map op 2 4 :zero-x :STA)
      (= op 0x8d) (create-inst-map op 3 4 :abs :STA)
      (= op 0x9d) (create-inst-map op 3 5 :abs-x :STA)
      (= op 0x99) (create-inst-map op 3 5 :abs-y :STA)
      (= op 0x81) (create-inst-map op 2 6 :ind-x :STA)
      (= op 0x91) (create-inst-map op 2 6 :ind-y :STA)
      ;; STX
      (= op 0x86) (create-inst-map op 2 3 :zero :STX)
      (= op 0x96) (create-inst-map op 2 4 :zero-y :STX)
      (= op 0x8e) (create-inst-map op 3 4 :abs :STX)
      ;; STY
      (= op 0x84) (create-inst-map op 2 3 :zero :STY)
      (= op 0x94) (create-inst-map op 2 4 :zero-x :STY)
      (= op 0x8c) (create-inst-map op 3 4 :abs :STY)
      ;; TAX
      (= op 0xaa) (create-inst-map op 1 2 :imp :TAX)
      ;; TAY
      (= op 0xa8) (create-inst-map op 1 2 :imp :TAY)
      ;; TSX
      (= op 0xba) (create-inst-map op 1 2 :imp :TSX)
      ;; TXA
      (= op 0x8a) (create-inst-map op 1 2 :imp :TXA)
      ;; TXS
      (= op 0x9a) (create-inst-map op 1 2 :imp :TXS)
      ;; TYA
      (= op 0x98) (create-inst-map op 1 2 :imp :TYA)
      ;; Return NOP for illegal operations
      :else (create-inst-map 0xea 1 2 :imp :NOP))))

(defn fetch-operands
  "Returns operands calculated based on the addressing mode.
   Operands are in signed byte representation."
  [{^bytes memory :memory
    pc :pc} inst]
  (let [mode (:mode inst)
        get-nth-operand (fn [n] (aget memory (+ pc n)))]
    (condp = mode
      :imp (assoc inst :operands [])
      :acc (assoc inst :operands [])
      :imm (assoc inst :operands [(get-nth-operand 1)])
      :zero (assoc inst :operands [(get-nth-operand 1)])
      :zero-x (assoc inst :operands [(get-nth-operand 1)])
      :zero-y (assoc inst :operands [(get-nth-operand 1)])
      :rel (assoc inst :operands [(get-nth-operand 1)])
      :abs (assoc inst :operands [(get-nth-operand 1) (get-nth-operand 2)])
      :abs-x (assoc inst :operands [(get-nth-operand 1) (get-nth-operand 2)])
      :abs-y (assoc inst :operands [(get-nth-operand 1) (get-nth-operand 2)])
      :ind (assoc inst :operands [(get-nth-operand 1) (get-nth-operand 2)])
      :ind-x (assoc inst :operands [(get-nth-operand 1)])
      :ind-y (assoc inst :operands [(get-nth-operand 1)]))))

(defn read-byte [{^bytes memory :memory} address]
  (aget memory address))

(defn write-byte [{^bytes memory :memory} address val]
  (aset-byte memory address (byte val)))

(defn handle-addressing-mode [state inst]
  "Returns the value to be used in execution using an Integer.
   Also returns whether to add extra cycles or not."
  (let [^byte memory (:memory state)
        reg-a (:reg-a state)
        reg-x (:reg-x state)
        reg-y (:reg-y state)
        mode (:mode inst)
        operands (:operands inst)
        read (fn [address] (aget memory address))
        make-address (fn [hi lo] (bit-or (bit-shift-left hi 8) lo))
        get-address-from-operands (fn []
                                    (let [low (Byte/toUnsignedInt (first operands))
                                          high (Byte/toUnsignedInt (second operands))]
                                         (bit-or (bit-shift-left high 8) low)))
        high-bits-same (fn [addr-1 addr-2]
                         (cond
                           (= (bit-and addr-1 0xFF00) (bit-and addr-2 0xFF00)) 0
                           :else 1))]
    (condp = mode
      :imp {}
      :acc {:value reg-a}
      :imm {:value (Byte/toUnsignedInt (first operands))}
      :zero {:value (read (first operands))}
      :zero-x {:value (read (bit-and 0x00FF (+ (Byte/toUnsignedInt reg-x) (Byte/toUnsignedInt (first operands)))))}
      :zero-y {:value (read (bit-and 0x00FF (+ (Byte/toUnsignedInt reg-y) (Byte/toUnsignedInt (first operands)))))}
      :rel {:value (first operands)}
      :abs (let [address (get-address-from-operands)]
             {:value (read address) :mem-address address})
      :abs-x (let [address (get-address-from-operands)
                   x-val (Byte/toUnsignedInt reg-x)
                   new-address (+ address x-val)
                   extra-cycles (high-bits-same address new-address)]
               {:value (read new-address) :mem-address new-address :extra-cycles extra-cycles})
      :abs-y (let [address (get-address-from-operands)
                   y-val (Byte/toUnsignedInt reg-y)
                   new-address (+ address y-val)
                   extra-cycles (high-bits-same address new-address)]
               {:value (read new-address) :mem-address new-address :extra-cycles extra-cycles})
      :ind (let [lsb-address (get-address-from-operands)
                 msb-address (if (= 0x00FF (bit-and 0x00FF lsb-address))
                               (bit-and 0xFF00 lsb-address)
                               (+ 1 lsb-address))
                 low (read lsb-address)
                 high (read msb-address)]
             {:value (bit-or (bit-shift-left high 8) low)})
      :ind-x (let [offset (Byte/toUnsignedInt (first operands))
                   x-val (Byte/toUnsignedInt reg-x)
                   low (bit-and 0x00FF (+ offset x-val))
                   high (bit-and 0x00FF (+ offset x-val 1))
                   address (make-address high low)]
               {:value (read address) :mem-address address})
      :ind-y (let [offset (Byte/toUnsignedInt (first operands))
                   low (read offset)
                   high (read (+ offset 1))
                   base-address (make-address high low)
                   address (+ base-address (Byte/toUnsignedInt reg-y))
                   extra-cycles (high-bits-same base-address address)]
               {:value (read address) :mem-address address :extra-cycles extra-cycles}))))


(defn execute-adc [state inst]
  (let [mode (:mode inst)
        acc (:reg-a state)]))

(defn execute-and [state inst])

(defn execute-asl [state inst])

(defn execute-bcc [state inst])

(defn execute-bcs [state inst])

(defn execute-beq [state inst])

(defn execute-bit [state inst])

(defn execute-bmi [state inst])

(defn execute-bne [state inst])

(defn execute-bpl [state inst])

(defn execute-brk [state inst])

(defn execute-bvc [state inst])

(defn execute-bvs [state inst])

(defn execute-clc [state inst])

(defn execute-cld [state inst])

(defn execute-cli [state inst])

(defn execute-clv [state inst])

(defn execute-cmp [state inst])

(defn execute-cpx [state inst])

(defn execute-cpy [state inst])

(defn execute-dec [state inst])

(defn execute-dex [state inst])

(defn execute-dey [state inst])

(defn execute-eor [state inst])

(defn execute-inc [state inst])

(defn execute-inx [state inst])

(defn execute-iny [state inst])

(defn execute-jmp [state inst])

(defn execute-jsr [state inst])

(defn execute-lda [state inst])

(defn execute-ldx [state inst])

(defn execute-ldy [state inst])

(defn execute-lsr [state inst])

(defn execute-nop [state inst])

(defn execute-ora [state inst])

(defn execute-pha [state inst])

(defn execute-php [state inst])

(defn execute-pla [state inst])

(defn execute-plp [state inst])

(defn execute-rol [state inst])

(defn execute-ror [state inst])

(defn execute-rti [state inst])

(defn execute-rts [state inst])

(defn execute-sbc [state inst])

(defn execute-sec [state inst])

(defn execute-sed [state inst])

(defn execute-sei [state inst])

(defn execute-sta [state inst])

(defn execute-stx [state inst])

(defn execute-sty [state inst])

(defn execute-tax [state inst])

(defn execute-tay [state inst])

(defn execute-tsx [state inst])

(defn execute-txa [state inst])

(defn execute-txs [state inst])

(defn execute-tya [state inst])

(defn execute
  "Returns new emulation state, and cycles elapsed after executing one instruction."
  [state inst]
  (let [name (:name inst)
        ^bytes memory (:memory state)]
    (condp = name
      :ADC (execute-adc state inst)
      :AND (execute-and state inst)
      :ASL (execute-asl state inst)
      :BCC (execute-bcc state inst)
      :BCS (execute-bcs state inst)
      :BEQ (execute-beq state inst)
      :BIT (execute-bit state inst)
      :BMI (execute-bmi state inst)
      :BNE (execute-bne state inst)
      :BPL (execute-bpl state inst)
      :BRK (execute-brk state inst)
      :BVC (execute-bvc state inst)
      :BVS (execute-bvs state inst)
      :CLC (execute-clc state inst)
      :CLD (execute-cld state inst)
      :CLI (execute-cli state inst)
      :CLV (execute-clv state inst)
      :CMP (execute-cmp state inst)
      :CPX (execute-cpx state inst)
      :CPY (execute-cpy state inst)
      :DEC (execute-dec state inst)
      :DEX (execute-dex state inst)
      :DEY (execute-dey state inst)
      :EOR (execute-eor state inst)
      :INC (execute-inc state inst)
      :INX (execute-inx state inst)
      :INY (execute-iny state inst)
      :JMP (execute-jmp state inst)
      :JSR (execute-jsr state inst)
      :LDA (execute-lda state inst)
      :LDX (execute-ldx state inst)
      :LDY (execute-ldy state inst)
      :LSR (execute-lsr state inst)
      :NOP (execute-nop state inst)
      :ORA (execute-ora state inst)
      :PHA (execute-pha state inst)
      :PHP (execute-php state inst)
      :PLA (execute-pla state inst)
      :PLP (execute-plp state inst)
      :ROL (execute-rol state inst)
      :ROR (execute-ror state inst)
      :RTI (execute-rti state inst)
      :RTS (execute-rts state inst)
      :SBC (execute-sbc state inst)
      :SEC (execute-sec state inst)
      :SED (execute-sed state inst)
      :SEI (execute-sei state inst)
      :STA (execute-sta state inst)
      :STX (execute-stx state inst)
      :STY (execute-sty state inst)
      :TAX (execute-tax state inst)
      :TAY (execute-tay state inst)
      :TSX (execute-tsx state inst)
      :TXA (execute-txa state inst)
      :TXS (execute-txs state inst)
      :TYA (execute-tya state inst))))

(fetch-operands state (decode (fetch state)))



