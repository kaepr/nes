(ns notebooks.cpu
 {:nextjournal.clerk/toc true}
 (:require [nes.cpu :as cpu]))

;; ## NES CPU
;; CPU is based on a 6502 processor, and runs at 1.79 Mhz.

;; - 8 Bit CPU
;; - Little Endian ( Addresses in memory stored as least significant byte first )
;; - 16 Bit Address Bus ( 64Kb addressable memory )

;; Supports only legal CPU instructions for the NES.
;;
;; - Read Byte Using Program Counter
;; - Decode Instruction from Byte
;; - Read Operands
;; - Execute
;; - Wait cycles
;;
;;




