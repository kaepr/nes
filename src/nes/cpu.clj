(ns nes.cpu
  (:require
   [nes.instructions :refer :all]))

(defn fetch
  "Returns byte pointed by the program counter."
  [{^bytes memory :memory
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
  Source: https://www.nesdev.org/obelisk-6502-guide/reference.html
  Source (undocumented opcodes): https://www.masswerk.at/6502/6502_instruction_set.html#illegals"
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
      (= op 0x4c) (create-inst-map op 3 3 :abs :JMP)
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
      (= op 0x1a) (create-inst-map op 1 2 :imp :NOP)
      (= op 0x3a) (create-inst-map op 1 2 :imp :NOP)
      (= op 0x5a) (create-inst-map op 1 2 :imp :NOP)
      (= op 0x7a) (create-inst-map op 1 2 :imp :NOP)
      (= op 0xda) (create-inst-map op 1 2 :imp :NOP)
      (= op 0xfa) (create-inst-map op 1 2 :imp :NOP)
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
      ;; DOP
      (= op 0x04) (create-inst-map op 2 3 :zero :DOP)
      (= op 0x14) (create-inst-map op 2 4 :zero-x :DOP)
      (= op 0x34) (create-inst-map op 2 4 :zero-x :DOP)
      (= op 0x44) (create-inst-map op 2 3 :zero :DOP)
      (= op 0x54) (create-inst-map op 2 4 :zero-x :DOP)
      (= op 0x64) (create-inst-map op 2 3 :zero :DOP)
      (= op 0x74) (create-inst-map op 2 4 :zero-x :DOP)
      (= op 0x80) (create-inst-map op 2 2 :imm :DOP)
      (= op 0x82) (create-inst-map op 2 2 :imm :DOP)
      (= op 0x89) (create-inst-map op 2 2 :imm :DOP)
      (= op 0xc2) (create-inst-map op 2 2 :imm :DOP)
      (= op 0xd4) (create-inst-map op 2 4 :zero-x :DOP)
      (= op 0xe2) (create-inst-map op 2 2 :imm :DOP)
      (= op 0xf4) (create-inst-map op 2 4 :zero-x :DOP)
      ;; TOP
      (= op 0x0c) (create-inst-map op 3 4 :abs :TOP)
      (= op 0x1c) (create-inst-map op 3 4 :abs-x :TOP)
      (= op 0x3c) (create-inst-map op 3 4 :abs-x :TOP)
      (= op 0x5c) (create-inst-map op 3 4 :abs-x :TOP)
      (= op 0x7c) (create-inst-map op 3 4 :abs-x :TOP)
      (= op 0xdc) (create-inst-map op 3 4 :abs-x :TOP)
      (= op 0xfc) (create-inst-map op 3 4 :abs-x :TOP)
      ;; LAX
      (= op 0xa7) (create-inst-map op 2 3 :zero :LAX)
      (= op 0xb7) (create-inst-map op 2 4 :zero-y :LAX)
      (= op 0xaf) (create-inst-map op 3 4 :abs :LAX)
      (= op 0xbf) (create-inst-map op 3 4 :abs-y :LAX)
      (= op 0xa3) (create-inst-map op 2 6 :ind-x :LAX)
      (= op 0xb3) (create-inst-map op 2 5 :ind-y :LAX)
      ;; SAX
      (= op 0x87) (create-inst-map op 2 3 :zero :SAX)
      (= op 0x97) (create-inst-map op 2 4 :zero-y :SAX)
      (= op 0x83) (create-inst-map op 2 6 :ind-x :SAX)
      (= op 0x8f) (create-inst-map op 3 4 :abs :SAX)
      ;; USBC
      (= op 0xeb) (create-inst-map op 2 2 :imm :USBC)
      ;; DCP
      (= op 0xc7) (create-inst-map op 2 5 :zero :DCP)
      (= op 0xd7) (create-inst-map op 2 6 :zero-x :DCP)
      (= op 0xcf) (create-inst-map op 3 6 :abs :DCP)
      (= op 0xdf) (create-inst-map op 3 7 :abs-x :DCP)
      (= op 0xdb) (create-inst-map op 3 7 :abs-y :DCP)
      (= op 0xc3) (create-inst-map op 2 8 :ind-x :DCP)
      (= op 0xd3) (create-inst-map op 2 8 :ind-y :DCP)
      ;; ISB ( ISC, INS )
      (= op 0xe7) (create-inst-map op 2 5 :zero :ISB)
      (= op 0xf7) (create-inst-map op 2 6 :zero-x :ISB)
      (= op 0xef) (create-inst-map op 3 6 :abs :ISB)
      (= op 0xff) (create-inst-map op 3 7 :abs-x :ISB)
      (= op 0xfb) (create-inst-map op 3 7 :abs-y :ISB)
      (= op 0xe3) (create-inst-map op 2 8 :ind-x :ISB)
      (= op 0xf3) (create-inst-map op 2 8 :ind-y :ISB)
      ;; SLO (ASO)
      (= op 0x07) (create-inst-map op 2 5 :zero :SLO)
      (= op 0x17) (create-inst-map op 2 6 :zero-x :SLO)
      (= op 0x0f) (create-inst-map op 3 6 :abs :SLO)
      (= op 0x1f) (create-inst-map op 3 7 :abs-x :SLO)
      (= op 0x1b) (create-inst-map op 3 7 :abs-y :SLO)
      (= op 0x03) (create-inst-map op 2 8 :ind-x :SLO)
      (= op 0x13) (create-inst-map op 2 8 :ind-y :SLO)
      ;; RLA
      (= op 0x27) (create-inst-map op 2 5 :zero :RLA)
      (= op 0x37) (create-inst-map op 2 6 :zero-x :RLA)
      (= op 0x2f) (create-inst-map op 3 6 :abs :RLA)
      (= op 0x3f) (create-inst-map op 3 7 :abs-x :RLA)
      (= op 0x3b) (create-inst-map op 3 7 :abs-y :RLA)
      (= op 0x23) (create-inst-map op 2 8 :ind-x :RLA)
      (= op 0x33) (create-inst-map op 2 8 :ind-y :RLA)
      ;; SRE (LSE)
      (= op 0x47) (create-inst-map op 2 5 :zero :SRE)
      (= op 0x57) (create-inst-map op 2 6 :zero-x :SRE)
      (= op 0x4f) (create-inst-map op 3 6 :abs :SRE)
      (= op 0x5f) (create-inst-map op 3 7 :abs-x :SRE)
      (= op 0x5b) (create-inst-map op 3 7 :abs-y :SRE)
      (= op 0x43) (create-inst-map op 2 8 :ind-x :SRE)
      (= op 0x53) (create-inst-map op 2 8 :ind-y :SRE)
      ;; RRA
      (= op 0x67) (create-inst-map op 2 5 :zero :RRA)
      (= op 0x77) (create-inst-map op 2 6 :zero-x :RRA)
      (= op 0x6f) (create-inst-map op 3 6 :abs :RRA)
      (= op 0x7f) (create-inst-map op 3 7 :abs-x :RRA)
      (= op 0x7b) (create-inst-map op 3 7 :abs-y :RRA)
      (= op 0x63) (create-inst-map op 2 8 :ind-x :RRA)
      (= op 0x73) (create-inst-map op 2 8 :ind-y :RRA))))

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

(defn handle-addressing-mode [state inst]
  "Returns values after handling the different addressing modes.
   Also returns extra cycles present or memory address if required"
  (let [memory (:memory state)
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
      :imm {:value (first operands)}
      :zero {:value (read (Byte/toUnsignedInt (first operands))) 
             :mem-address (Byte/toUnsignedInt (first operands))}
      :zero-x (let [address (bit-and 0x00FF
                                     (+ (Byte/toUnsignedInt reg-x)
                                        (Byte/toUnsignedInt (first operands))))]
                {:value (read address) :mem-address address})
      :zero-y (let [address (bit-and 0x00FF
                                     (+ (Byte/toUnsignedInt reg-y)
                                        (Byte/toUnsignedInt (first operands))))]
                {:value (read address) :mem-address address})
      :rel {:value (first operands)}
      :abs (let [address (get-address-from-operands)]
             {:value (read address) :mem-address address})
      :abs-x (let [address (get-address-from-operands)
                   x-val (Byte/toUnsignedInt reg-x)
                   new-address (bit-and 0xFFFF (+ address x-val))
                   extra-cycles (high-bits-same address new-address)]
               {:value (read new-address) :mem-address new-address :extra-cycles extra-cycles})
      :abs-y (let [address (get-address-from-operands)
                   y-val (Byte/toUnsignedInt reg-y)
                   new-address (bit-and 0xFFFF (+ address y-val))
                   extra-cycles (high-bits-same address new-address)]
               {:value (read new-address) :mem-address new-address :extra-cycles extra-cycles})
      :ind (let [lsb-address (get-address-from-operands)
                 msb-address (if (= 0x00FF (bit-and 0x00FF lsb-address))
                               (bit-and 0xFF00 lsb-address)
                               (+ 1 lsb-address))
                 low (Byte/toUnsignedInt (read lsb-address))
                 high (Byte/toUnsignedInt (read msb-address))
                 address (bit-or (bit-shift-left high 8) low)]
             {:value address :mem-address address})
      :ind-x (let [offset (Byte/toUnsignedInt (first operands))
                   x-val (Byte/toUnsignedInt reg-x)
                   lo (read (bit-and 0x00FF (+ offset x-val)))
                   hi (read (bit-and 0x00FF (+ offset x-val 1)))
                   address (make-address (Byte/toUnsignedInt hi) (Byte/toUnsignedInt lo))]
               {:value (read address) :mem-address address})
      :ind-y (let [offset (Byte/toUnsignedInt (first operands))
                   lo (Byte/toUnsignedInt (read (bit-and 0x00FF offset)))
                   hi (Byte/toUnsignedInt (read (bit-and 0x00FF (+ offset 1))))
                   base-address (bit-and 0xFFFF (make-address hi lo))
                   address (bit-and 0xFFFF (+ base-address (Byte/toUnsignedInt reg-y)))
                   extra-cycles (high-bits-same base-address address)]
               {:value (read address) :mem-address address :extra-cycles extra-cycles}))))

(defn execute
  "Returns the next cpu state, and cycles elapsed after executing instruction."
  [state inst]
  (let [name (:name inst)
        values (handle-addressing-mode state inst)]
    (condp = name
      :ADC (execute-adc state inst values)
      :AND (execute-and state inst values)
      :ASL (execute-asl state inst values)
      :BCC (execute-bcc state inst values)
      :BCS (execute-bcs state inst values)
      :BEQ (execute-beq state inst values)
      :BIT (execute-bit state inst values)
      :BMI (execute-bmi state inst values)
      :BNE (execute-bne state inst values)
      :BPL (execute-bpl state inst values)
      :BRK (execute-brk state inst values)
      :BVC (execute-bvc state inst values)
      :BVS (execute-bvs state inst values)
      :CLC (execute-clc state inst values)
      :CLD (execute-cld state inst values)
      :CLI (execute-cli state inst values)
      :CLV (execute-clv state inst values)
      :CMP (execute-cmp state inst values)
      :CPX (execute-cpx state inst values)
      :CPY (execute-cpy state inst values)
      :DEC (execute-dec state inst values)
      :DEX (execute-dex state inst values)
      :DEY (execute-dey state inst values)
      :EOR (execute-eor state inst values)
      :INC (execute-inc state inst values)
      :INX (execute-inx state inst values)
      :INY (execute-iny state inst values)
      :JMP (execute-jmp state inst values)
      :JSR (execute-jsr state inst values)
      :LDA (execute-lda state inst values)
      :LDX (execute-ldx state inst values)
      :LDY (execute-ldy state inst values)
      :LSR (execute-lsr state inst values)
      :NOP (execute-nop state inst values)
      :ORA (execute-ora state inst values)
      :PHA (execute-pha state inst values)
      :PHP (execute-php state inst values)
      :PLA (execute-pla state inst values)
      :PLP (execute-plp state inst values)
      :ROL (execute-rol state inst values)
      :ROR (execute-ror state inst values)
      :RTI (execute-rti state inst values)
      :RTS (execute-rts state inst values)
      :SBC (execute-sbc state inst values)
      :SEC (execute-sec state inst values)
      :SED (execute-sed state inst values)
      :SEI (execute-sei state inst values)
      :STA (execute-sta state inst values)
      :STX (execute-stx state inst values)
      :STY (execute-sty state inst values)
      :TAX (execute-tax state inst values)
      :TAY (execute-tay state inst values)
      :TSX (execute-tsx state inst values)
      :TXA (execute-txa state inst values)
      :TXS (execute-txs state inst values)
      :TYA (execute-tya state inst values)
      :DOP (execute-dop state inst values)
      :TOP (execute-top state inst values)
      :LAX (execute-lax state inst values)
      :SAX (execute-sax state inst values)
      :USBC (execute-usbc state inst values)
      :DCP (execute-dcp state inst values)
      :ISB (execute-isb state inst values)
      :SLO (execute-slo state inst values)
      :RLA (execute-rla state inst values)
      :SRE (execute-sre state inst values)
      :RRA (execute-rra state inst values))))

(defn run-instruction [state]
  (let [opcode (fetch state)
        inst (fetch-operands state (decode opcode))
        next-state (execute state inst)]
    (merge state next-state {:pc (bit-and 0xffff (:pc next-state))
                             :cycles (+ (:cycles state)
                                        (:cycles-elapsed next-state))})))

