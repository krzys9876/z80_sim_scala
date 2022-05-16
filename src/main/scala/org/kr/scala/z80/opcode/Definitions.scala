package org.kr.scala.z80.opcode

import org.kr.scala.z80.opcode.handler.JumpCondition
import org.kr.scala.z80.system.Regs

//Arithmetic 8b
// generator for ADD A,x - TBC if this is an efficient and readable way of defining opcodes
class Arithmetic8bDef(main:Int, supp:Int, val destination:Location, val size:Int, val t:Int, val label:String)
  extends OpCode(main,supp) with HandleArithmetic8Bit with OpCodeDestLocation with OpCodeSize with OpCodeTCycles with Label

//ADD
object ADD_A_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x80)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"ADD A,${op._2.label}") with Arith8bAdd)
}
object ADD_A_n extends OpCode(0xC6) with HandleArithmetic8Bit with Arith8bAdd with DestinationN
  with Size2 with T7 with Label {override val label:String="ADD A,n"}
//ADC
object ADC_A_reg {
  val codes: List[Arithmetic8bDef] =
    OpCode.generateOpCodesType1(OpCode(0x88)).map(op=>
      new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"ADC A,${op._2.label}") with Arith8bAddC)
}
object ADC_A_n extends OpCode(0xCE) with HandleArithmetic8Bit with Arith8bAddC with DestinationN
  with Size2 with T7 with Label {override val label:String="ADC A,n"}
//SUB
object SUB_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x90)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"SUB ${op._2.label}") with Arith8bSub)
}
object SUB_n extends OpCode(0xD6) with HandleArithmetic8Bit with Arith8bSub with DestinationN
  with Size2 with T7 with Label {override val label:String="SUB n"}
//SBC
object SBC_A_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x98)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"SBC A,${op._2.label}") with Arith8bSubC)
}
object SBC_A_n extends OpCode(0xDE) with HandleArithmetic8Bit with Arith8bSubC with DestinationN
  with Size2 with T7 with Label {override val label:String="SBC A,n"}
//AND
object AND_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xA0)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"AND ${op._2.label}") with Arith8bAnd)
}
object AND_n extends OpCode(0xE6) with HandleArithmetic8Bit with Arith8bAnd with DestinationN
  with Size2 with T7 with Label {override val label:String="AND n"}
//XOR
object XOR_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xA8)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"XOR ${op._2.label}") with Arith8bXor)
}
object XOR_n extends OpCode(0xEE) with HandleArithmetic8Bit with Arith8bXor with DestinationN
  with Size2 with T7 with Label {override val label:String="XOR n"}
//OR
object OR_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xB0)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"OR ${op._2.label}") with Arith8bOr)
}
object OR_n extends OpCode(0xF6) with HandleArithmetic8Bit with Arith8bOr with DestinationN
  with Size2 with T7 with Label {override val label:String="OR n"}
//CP
object CP_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xB8)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"CP ${op._2.label}") with Arith8bCp)
}
object CP_n extends OpCode(0xFE) with HandleArithmetic8Bit with Arith8bCp with DestinationN
  with Size2 with T7 with Label {override val label:String="CP n"}
//INC
object INC_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x04),3).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"INC ${op._2.label}") with SourceFromDestination with Arith8bInc)
}
//DEC
object DEC_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x05),3).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"DEC ${op._2.label}") with SourceFromDestination with Arith8bDec)
}
object CPL extends OpCode(0x2F) with HandleArithmetic8Bit with Arith8bCpl with Size1 with T4 with Label {override val label:String="CPL"}
object NEG extends OpCode(0xED,0x44) with HandleArithmetic8Bit with Arith8bNeg with Size2 with T8 with Label {override val label:String="NEG"}
object SCF extends OpCode(0x37) with HandleArithmetic8Bit with Arith8bScf with Size1 with T4 with Label {override val label:String="SCF"}
object CCF extends OpCode(0x3F) with HandleArithmetic8Bit with Arith8bCcf with Size1 with T4 with Label {override val label:String="CCF"}
// Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
object RLD extends OpCode(0xED,0x6F) with RotateDigitLeft with HandleRotateDigit with Size2 with T18 with Label {override val label:String="RLD"}
object RRD extends OpCode(0xED,0x67) with RotateDigitRight with HandleRotateDigit with Size2 with T18 with Label {override val label:String="RRD"}

//Arithmetic 16b
object ADD_HL_BC extends OpCode(0x09) with HandleArithmetic16Bit with Arith16bAdd with SourceBC with DestinationHL with Size1 with T11 with Label {override val label:String="ADD HL,BC"}
object ADD_HL_DE extends OpCode(0x19) with HandleArithmetic16Bit with Arith16bAdd with SourceDE with DestinationHL with Size1 with T11 with Label {override val label:String="ADD HL,DE"}
object ADD_HL_HL extends OpCode(0x29) with HandleArithmetic16Bit with Arith16bAdd with SourceHL with DestinationHL with Size1 with T11 with Label {override val label:String="ADD HL,HL"}
object ADD_HL_SP extends OpCode(0x39) with HandleArithmetic16Bit with Arith16bAdd with SourceSP with DestinationHL with Size1 with T11 with Label {override val label:String="ADD HL,SP"}
object ADD_IX_BC extends OpCode(0xDD,0x09) with HandleArithmetic16Bit with Arith16bAdd with DestinationIX with SourceBC with Size2 with T15 with Label {override val label:String="ADD IX,BC"}
object ADD_IX_DE extends OpCode(0xDD,0x19) with HandleArithmetic16Bit with Arith16bAdd with DestinationIX with SourceDE with Size2 with T15 with Label {override val label:String="ADD IX,DE"}
object ADD_IX_IX extends OpCode(0xDD,0x29) with HandleArithmetic16Bit with Arith16bAdd with DestinationIX with SourceIX with Size2 with T15 with Label {override val label:String="ADD IX,IX"}
object ADD_IX_SP extends OpCode(0xDD,0x39) with HandleArithmetic16Bit with Arith16bAdd with DestinationIX with SourceSP with Size2 with T15 with Label {override val label:String="ADD IX,SP"}
object ADD_IY_BC extends OpCode(0xFD,0x09) with HandleArithmetic16Bit with Arith16bAdd with DestinationIY with SourceBC with Size2 with T15 with Label {override val label:String="ADD IY,BC"}
object ADD_IY_DE extends OpCode(0xFD,0x19) with HandleArithmetic16Bit with Arith16bAdd with DestinationIY with SourceDE with Size2 with T15 with Label {override val label:String="ADD IY,DE"}
object ADD_IY_IY extends OpCode(0xFD,0x29) with HandleArithmetic16Bit with Arith16bAdd with DestinationIY with SourceIY with Size2 with T15 with Label {override val label:String="ADD IY,IX"}
object ADD_IY_SP extends OpCode(0xFD,0x39) with HandleArithmetic16Bit with Arith16bAdd with DestinationIY with SourceSP with Size2 with T15 with Label {override val label:String="ADD IY,SP"}
object ADC_HL_BC extends OpCode(0xED,0x4A) with HandleArithmetic16Bit with Arith16bAddC with DestinationHL with SourceBC with Size2 with T15 with Label {override val label:String="ADC HL,BC"}
object ADC_HL_DE extends OpCode(0xED,0x5A) with HandleArithmetic16Bit with Arith16bAddC with DestinationHL with SourceDE with Size2 with T15 with Label {override val label:String="ADC HL,DE"}
object ADC_HL_HL extends OpCode(0xED,0x6A) with HandleArithmetic16Bit with Arith16bAddC with DestinationHL with SourceHL with Size2 with T15 with Label {override val label:String="ADC HL,HL"}
object ADC_HL_SP extends OpCode(0xED,0x7A) with HandleArithmetic16Bit with Arith16bAddC with DestinationHL with SourceSP with Size2 with T15 with Label {override val label:String="ADC HL,SP"}
object SBC_HL_BC extends OpCode(0xED,0x42) with HandleArithmetic16Bit with Arith16bSubC with DestinationHL with SourceBC with Size2 with T15 with Label {override val label:String="SBC HL,BC"}
object SBC_HL_DE extends OpCode(0xED,0x52) with HandleArithmetic16Bit with Arith16bSubC with DestinationHL with SourceDE with Size2 with T15 with Label {override val label:String="SBC HL,DE"}
object SBC_HL_HL extends OpCode(0xED,0x62) with HandleArithmetic16Bit with Arith16bSubC with DestinationHL with SourceHL with Size2 with T15 with Label {override val label:String="SBC HL,HL"}
object SBC_HL_SP extends OpCode(0xED,0x72) with HandleArithmetic16Bit with Arith16bSubC with DestinationHL with SourceSP with Size2 with T15 with Label {override val label:String="SBC HL,SP"}
object INC_BC extends OpCode(0x03) with HandleArithmetic16Bit with Arith16bInc with SourceDestBC with Size1 with T6 with Label {override val label:String="INC BC"}
object INC_DE extends OpCode(0x13) with HandleArithmetic16Bit with Arith16bInc with SourceDestDE with Size1 with T6 with Label {override val label:String="INC DE"}
object INC_HL_16 extends OpCode(0x23) with HandleArithmetic16Bit with Arith16bInc with SourceDestHL with Size1 with T6 with Label {override val label:String="INC HL"}
object INC_SP extends OpCode(0x33) with HandleArithmetic16Bit with Arith16bInc with SourceDestSP with Size1 with T6 with Label {override val label:String="INC SP"}
object INC_IX extends OpCode(0xDD,0x23) with HandleArithmetic16Bit with Arith16bInc with SourceDestIX with Size2 with T10 with Label {override val label:String="INC IX"}
object INC_IY extends OpCode(0xFD,0x23) with HandleArithmetic16Bit with Arith16bInc with SourceDestIY with Size2 with T10 with Label {override val label:String="INC IY"}
object DEC_BC extends OpCode(0x0B) with HandleArithmetic16Bit with Arith16bDec with SourceDestBC with Size1 with T6 with Label {override val label:String="DEC BC"}
object DEC_DE extends OpCode(0x1B) with HandleArithmetic16Bit with Arith16bDec with SourceDestDE with Size1 with T6 with Label {override val label:String="DEC DE"}
object DEC_HL_16 extends OpCode(0x2B) with HandleArithmetic16Bit with Arith16bDec with SourceDestHL with Size1 with T6 with Label {override val label:String="DEC HL"}
object DEC_SP extends OpCode(0x3B) with HandleArithmetic16Bit with Arith16bDec with SourceDestSP with Size1 with T6 with Label {override val label:String="DEC SP"}
object DEC_IX extends OpCode(0xDD,0x2B) with HandleArithmetic16Bit with Arith16bDec with SourceDestIX with Size2 with T10 with Label {override val label:String="DEC IX"}
object DEC_IY extends OpCode(0xFD,0x2B) with HandleArithmetic16Bit with Arith16bDec with SourceDestIY with Size2 with T10 with Label {override val label:String="DEC IY"}

// Exchange
object EX_DE_HL extends OpCode(0xEB) with HandleExchange with ExchangeDEHL with Size1 with T4 with Label {override val label:String="EX DE,HL"}
object EX_AF_AF1 extends OpCode(0x08) with HandleExchange with ExchangeAF1 with Size1 with T4 with Label {override val label:String="EX AF,AF'"}
object EXX extends OpCode(0xD9) with HandleExchange with ExchangeAll1 with Size1 with T4 with Label {override val label:String="EXX"}
object EX_SP_HL extends OpCode(0xE3) with HandleExchange with ExchangeSPHL with Size1 with T19 with Label {override val label:String="EX (SP),HL"}
object EX_SP_IX extends OpCode(0xDD,0xE3) with HandleExchange with ExchangeSPIX with Size2 with T23 with Label {override val label:String="EX (SP),IX"}
object EX_SP_IY extends OpCode(0xFD,0xE3) with HandleExchange with ExchangeSPIY with Size2 with T23 with Label {override val label:String="EX (SP),IY"}

//Bit manipulation
// generator for BIT, RES, SET
class BitManipulationDef(main:Int, supp:Int, supp2:Int, val source:Location, val bit:Int, val size:Int, val t:Int, val label:String)
  extends OpCode(main,supp,supp2) with HandleBitManipulation with OpCodeSourceLocation with OpCodeSize with OpCodeTCycles with Label
//BIT
object BIT_b_reg {
  val codes: List[BitManipulationDef] = OpCode.generateOpCodesType2(OpCode(0xCB,0x40),setOrRes = false).map(op=>
    new BitManipulationDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,op._5,f"BIT ${op._3},${op._2.label}") with BitTest)
}
//RESET
object RES_b_reg {
  val codes: List[BitManipulationDef] = OpCode.generateOpCodesType2(OpCode(0xCB,0x80),setOrRes = true).map(op=>
    new BitManipulationDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,op._5,f"RES ${op._3},${op._2.label}") with BitReset)
}
//SET
object SET_b_reg {
  val codes: List[BitManipulationDef] = OpCode.generateOpCodesType2(OpCode(0xCB,0xC0),setOrRes = true).map(op=>
    new BitManipulationDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,op._5,f"SET ${op._3},${op._2.label}") with BitSet)
}

//Load 8 bit
// Z80 manual page 42
// generator for LD (8 bit)
class Load8bitDef(main:Int, supp:Int, val source:Location, val destination:Location, val size:Int, val t:Int, val label:String)
  extends OpCode(main,supp) with HandleLoad8Bit with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles with Label
//LD A-L,A-L/(HL)/(IX+d)/(IY+d)
object LD_reg_all {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType3(OpCode(0x40)).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,op._3,op._4,op._5,f"LD ${op._3},${op._2.label}"))
}
object LD_HL_reg {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType4(OpCode(0x70),1).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,RegisterAddrLocation(Regs.HL),op._3,7,f"LD (HL),${op._2.label}"))
}
object LD_IXd_reg {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType4(OpCode(0xDD,0x70),3).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,RegisterAddrIndirOffsetLocation(Regs.IX,2),op._3,19,f"LD (IX+d),${op._2.label}"))
}
object LD_IYd_reg {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType4(OpCode(0xFD,0x70),3).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,RegisterAddrIndirOffsetLocation(Regs.IY,2),op._3,19,f"LD (IY+d),${op._2.label}"))
}
object LD_all_n {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType5(OpCode(0x06)).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,RegisterAddrDirOffsetLocation(Regs.PC,op._3-1),op._2,op._3,op._4,f"LD ${op._2.label},n"))
}
object LD_A_I extends Load8bitDef(0xED,0x57,RegisterLocation(Regs.I),RegisterLocation(Regs.A),2,9,"LD A,I")
object LD_A_R extends Load8bitDef(0xED,0x5F,RegisterLocation(Regs.R),RegisterLocation(Regs.A),2,9,"LD A,R")
object LD_I_A extends Load8bitDef(0xED,0x47,RegisterLocation(Regs.A),RegisterLocation(Regs.I),2,9,"LD I,A")
object LD_R_A extends Load8bitDef(0xED,0x4F,RegisterLocation(Regs.A),RegisterLocation(Regs.R),2,9,"LD R,A")
object LD_A_BC extends Load8bitDef(0x0A,OpCode.ANY,RegisterAddrLocation(Regs.BC),RegisterLocation(Regs.A),1,7,"LD A,(BC)")
object LD_A_DE extends Load8bitDef(0x1A,OpCode.ANY,RegisterAddrLocation(Regs.DE),RegisterLocation(Regs.A),1,7,"LD A,(DE)")
object LD_BC_A extends Load8bitDef(0x02,OpCode.ANY,RegisterLocation(Regs.A),RegisterAddrLocation(Regs.BC),1,7,"LD (BC),A")
object LD_DE_A extends Load8bitDef(0x12,OpCode.ANY,RegisterLocation(Regs.A),RegisterAddrLocation(Regs.DE),1,7,"LD (DE),A")
object LD_A_nn extends Load8bitDef(0x3A,OpCode.ANY,IndirectAddrLocation(1),RegisterLocation(Regs.A),3,13,"LD A,(nn)")
object LD_nn_A extends Load8bitDef(0x32,OpCode.ANY,RegisterLocation(Regs.A),IndirectAddrLocation(1),3,13,"LD (nn),A")

//Load 16 bit
// Z80 manual page 45 (NOTE: PUSH qq are X5, not X6!)
// generator for LD (8 bit)
class Load16bitDef(main:Int, supp:Int, val source:Location, val destination:Location, val size:Int, val t:Int, val label:String)
  extends OpCode(main,supp) with HandleLoad16Bit with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles with Label
object PUSH_AF extends Load16bitDef(0xF5,OpCode.ANY,RegisterLocation(Regs.AF),RegisterAddrDirOffsetLocation(Regs.SP, -2, isWord = true),1,11,"PUSH AF") with PushStack
object PUSH_BC extends Load16bitDef(0xC5,OpCode.ANY,RegisterLocation(Regs.BC),RegisterAddrDirOffsetLocation(Regs.SP, -2, isWord = true),1,11,"PUSH BC") with PushStack
object PUSH_DE extends Load16bitDef(0xD5,OpCode.ANY,RegisterLocation(Regs.DE),RegisterAddrDirOffsetLocation(Regs.SP, -2, isWord = true),1,11,"PUSH DE") with PushStack
object PUSH_HL extends Load16bitDef(0xE5,OpCode.ANY,RegisterLocation(Regs.HL),RegisterAddrDirOffsetLocation(Regs.SP, -2, isWord = true),1,11,"PUSH HL") with PushStack
object PUSH_IX extends Load16bitDef(0xDD,0xE5,RegisterLocation(Regs.IX),RegisterAddrDirOffsetLocation(Regs.SP, -2, isWord = true),2,15,"PUSH IX") with PushStack
object PUSH_IY extends Load16bitDef(0xFD,0xE5,RegisterLocation(Regs.IY),RegisterAddrDirOffsetLocation(Regs.SP, -2, isWord = true),2,15,"PUSH IY") with PushStack
object POP_AF extends Load16bitDef(0xF1,OpCode.ANY,RegisterAddrLocation(Regs.SP,isWord = true),RegisterLocation(Regs.AF),1,10,"POP AF") with PopStack
object POP_BC extends Load16bitDef(0xC1,OpCode.ANY,RegisterAddrLocation(Regs.SP,isWord = true),RegisterLocation(Regs.BC),1,10,"POP BC") with PopStack
object POP_DE extends Load16bitDef(0xD1,OpCode.ANY,RegisterAddrLocation(Regs.SP,isWord = true),RegisterLocation(Regs.DE),1,10,"POP DE") with PopStack
object POP_HL extends Load16bitDef(0xE1,OpCode.ANY,RegisterAddrLocation(Regs.SP,isWord = true),RegisterLocation(Regs.HL),1,10,"POP HL") with PopStack
object POP_IX extends Load16bitDef(0xDD,0xE1,RegisterAddrLocation(Regs.SP,isWord = true),RegisterLocation(Regs.IX),2,14,"POP IX") with PopStack
object POP_IY extends Load16bitDef(0xFD,0xE1,RegisterAddrLocation(Regs.SP,isWord = true),RegisterLocation(Regs.IY),2,14,"POP IY") with PopStack
object LD_SP_HL extends Load16bitDef(0xF9,OpCode.ANY,RegisterLocation(Regs.HL),RegisterLocation(Regs.SP),1,6,"LD SP,HL")
object LD_SP_IX extends Load16bitDef(0xDD,0xF9,RegisterLocation(Regs.IX),RegisterLocation(Regs.SP),2,10,"LD SP,IX")
object LD_SP_IY extends Load16bitDef(0xFD,0xF9,RegisterLocation(Regs.IY),RegisterLocation(Regs.SP),2,10,"LD SP,IY")
object LD_nn_BC extends Load16bitDef(0xED,0x43,RegisterLocation(Regs.BC),IndirectAddrLocation(2,isWord = true),4,20,"LD (nn),BC")
object LD_nn_DE extends Load16bitDef(0xED,0x53,RegisterLocation(Regs.DE),IndirectAddrLocation(2,isWord = true),4,20,"LD (nn),DE")
object LD_nn_HL extends Load16bitDef(0x22,OpCode.ANY,RegisterLocation(Regs.HL),IndirectAddrLocation(1,isWord = true),3,16,"LD (nn),HL")
object LD_nn_SP extends Load16bitDef(0xED,0x73,RegisterLocation(Regs.SP),IndirectAddrLocation(2,isWord = true),4,20,"LD (nn),SP")
object LD_nn_IX extends Load16bitDef(0xDD,0x22,RegisterLocation(Regs.IX),IndirectAddrLocation(2,isWord = true),4,20,"LD (nn),IX")
object LD_nn_IY extends Load16bitDef(0xFD,0x22,RegisterLocation(Regs.IY),IndirectAddrLocation(2,isWord = true),4,20,"LD (nn),IY")
object LD_BC_nn extends Load16bitDef(0xED,0x4B,IndirectAddrLocation(2,isWord = true),RegisterLocation(Regs.BC),4,20,"LD BC,(nn)")
object LD_DE_nn extends Load16bitDef(0xED,0x5B,IndirectAddrLocation(2,isWord = true),RegisterLocation(Regs.DE),4,20,"LD DE,(nn)")
object LD_HL_nn extends Load16bitDef(0x2A,OpCode.ANY,IndirectAddrLocation(1,isWord = true),RegisterLocation(Regs.HL),3,16,"LD HL,(nn)")
object LD_SP_nn extends Load16bitDef(0xED,0x7B,IndirectAddrLocation(2,isWord = true),RegisterLocation(Regs.SP),4,20,"LD SP,(nn)")
object LD_IX_nn extends Load16bitDef(0xDD,0x2A,IndirectAddrLocation(2,isWord = true),RegisterLocation(Regs.IX),4,20,"LD IX,(nn)")
object LD_IY_nn extends Load16bitDef(0xFD,0x2A,IndirectAddrLocation(2,isWord = true),RegisterLocation(Regs.IY),4,20,"LD IY,(nn)")
object LD_BC_i extends Load16bitDef(0x01,OpCode.ANY,RegisterAddrDirOffsetLocation(Regs.PC, 1, isWord = true),RegisterLocation(Regs.BC),3,10,"LD BC,nn")
object LD_DE_i extends Load16bitDef(0x11,OpCode.ANY,RegisterAddrDirOffsetLocation(Regs.PC, 1, isWord = true),RegisterLocation(Regs.DE),3,10,"LD DE,nn")
object LD_HL_i extends Load16bitDef(0x21,OpCode.ANY,RegisterAddrDirOffsetLocation(Regs.PC, 1, isWord = true),RegisterLocation(Regs.HL),3,10,"LD HL,nn")
object LD_SP_i extends Load16bitDef(0x31,OpCode.ANY,RegisterAddrDirOffsetLocation(Regs.PC, 1, isWord = true),RegisterLocation(Regs.SP),3,10,"LD SP,nn")
object LD_IX_i extends Load16bitDef(0xDD,0x21,RegisterAddrDirOffsetLocation(Regs.PC, 2, isWord = true),RegisterLocation(Regs.IX),4,14,"LD IX,nn")
object LD_IY_i extends Load16bitDef(0xFD,0x21,RegisterAddrDirOffsetLocation(Regs.PC, 2, isWord = true),RegisterLocation(Regs.IY),4,14,"LD IY,nn")

//Rotate and shift
// Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
// generator for rotate and shift
class RotateShiftDef(main:Int, supp:Int, supp2:Int, val source:Location, val size:Int, val t:Int, val label:String)
  extends OpCode(main,supp,supp2) with OpCodeSourceLocation with HandleRotateShift with OpCodeSize with OpCodeTCycles with Label
object RLC_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x00)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"RLC ${op._2.label}") with RotateShiftRlc)
}
object RRC_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x08)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"RRC ${op._2.label}") with RotateShiftRrc)
}
object RL_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x10)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"RL ${op._2.label}") with RotateShiftRl)
}
object RR_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x18)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"RR ${op._2.label}") with RotateShiftRr)
}
object SLA_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x20)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"SLA ${op._2.label}") with RotateShiftSla)
}
object SRA_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x28)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"SRA ${op._2.label}") with RotateShiftSra)
}
object SRL_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x38)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"SRL ${op._2.label}") with RotateShiftSrl)
}
object RLCA extends OpCode(0x07) with RotateShiftRlca with SourceA with HandleRotateShift with Size1 with T4 with Label {override val label:String="RLCA"}
object RRCA extends OpCode(0x0F) with RotateShiftRrca with SourceA with HandleRotateShift with Size1 with T4 with Label {override val label:String="RRCA"}
object RLA extends OpCode(0x17) with RotateShiftRla with SourceA with HandleRotateShift with Size1 with T4 with Label {override val label:String="RLA"}
object RRA extends OpCode(0x1F) with RotateShiftRra with SourceA with HandleRotateShift with Size1 with T4 with Label {override val label:String="RRA"}

//Jump
//Z80 manual p.59
// generator for jump group
class JumpDef(main:Int, val condition:JumpCondition, val size:Int, val label:String)
  extends OpCode(main) with OpCodeJumpCondition with HandleJump with OpCodeSize with Label
object JP_cond {
  val codes:List[JumpDef] = OpCode.generateOpCodesType7(OpCode(0xC2),3).map(op=>
    new JumpDef(op._1.main,op._2,op._3,f"JP ${if(op._2.value==0) "N" else ""}${op._2.flag.symbol},nn") with T10 with JumpOper with SourceNw )
}
object CALL_cond {
  val codes:List[JumpDef] = OpCode.generateOpCodesType7(OpCode(0xC4),3).map(op=>
    new JumpDef(op._1.main,op._2,op._3,f"CALL ${if(op._2.value==0) "N" else ""}${op._2.flag.symbol},nn") with T10T17 with CallOper with SourceNw)
}
object RET_cond {
  val codes:List[JumpDef] = OpCode.generateOpCodesType7(OpCode(0xC0),1).map(op=>
    new JumpDef(op._1.main,op._2,op._3,f"RET ${if(op._2.value==0) "N" else ""}${op._2.flag.symbol}") with T5T11 with ReturnOper with SourceStack)
}
object JP_nn extends OpCode(0xC3) with JumpUnconditional with JumpOper with SourceNw with HandleJump with Size3 with T10 with Label {override val label:String="JP nn"}
object JR_n extends OpCode(0x18) with JumpUnconditional with JumpRelativeOper with SourceN with HandleJump with Size2 with T12 with Label {override val label:String="JR n"}
object JR_NZ_n extends OpCode(0x20) with JumpNZ with JumpRelativeOper with SourceN with HandleJump with Size2 with T7T12 with Label {override val label:String="JR NZ,n"}
object JR_Z_n extends OpCode(0x28) with JumpZ with JumpRelativeOper with SourceN with HandleJump with Size2 with T7T12 with Label {override val label:String="JR Z,n"}
object JR_NC_n extends OpCode(0x30) with JumpNC with JumpRelativeOper with SourceN with HandleJump with Size2 with T7T12 with Label {override val label:String="JR NC,n"}
object JR_C_n extends OpCode(0x38) with JumpC with JumpRelativeOper with SourceN with HandleJump with Size2 with T7T12 with Label {override val label:String="JR C,n"}
object JP_HL extends OpCode(0xE9) with JumpUnconditional with JumpOper with SourceHL with HandleJump with Size1 with T4 with Label {override val label:String="JP (HL)"}
object JP_IX extends OpCode(0xDD,0xE9) with JumpUnconditional with JumpOper with SourceIX with HandleJump with Size2 with T8 with Label {override val label:String="JP (IX)"}
object JP_IY extends OpCode(0xFD,0xE9) with JumpUnconditional with JumpOper with SourceIY with HandleJump with Size2 with T8 with Label {override val label:String="JP (IY)"}
object CALL_nn extends OpCode(0xCD) with JumpUnconditional with CallOper with SourceNw with HandleJump with Size3 with T17 with Label {override val label:String="CALL nn"}
object DJNZ extends OpCode(0x10) with JumpB0 with DecrJumpRelativeOper with SourceN with HandleJump with Size2 with T13T8 with Label {override val label:String="DJNZ"}
object RET extends OpCode(0xC9) with JumpUnconditional with ReturnOper with SourceStack with HandleJump with Size1 with T10 with Label {override val label:String="RET"}
object RETI extends OpCode(0xED,0x4D) with JumpUnconditional with ReturnOper with HandleJump with SourceStack with Size2 with T14 with Label {override val label:String="RETI"}

object RST_all {
  val codes:List[JumpDef]={
    for(
      locCode<-List.range(0,8).map(_ << 3) // list of actual addresses to call
    ) yield {
      new JumpDef(0xC7+locCode,JumpCondition.empty,1,f"RST ${OpCode.num2hex(locCode)}")
        with T11 with HandleJump with CallOper with OpCodeSourceLocation {
        override val source: Location = ImmediateLocation(locCode)}
    }
  }
}

//IN/OUT
object IN_A_n extends OpCode(0xDB) with DestinationN with SourceA with InOper with Size2 with T11 with HandleInOut with Label {override val label:String="IN A,(n)"}
object IN_A_C extends OpCode(0xED,0x78) with DestinationC with SourceA with InOper with HandleInOut with Size2 with T12 with Label {override val label:String="IN A"}
object IN_B_C extends OpCode(0xED,0x40) with DestinationC with SourceB with InOper with HandleInOut with Size2 with T12 with Label {override val label:String="IN B,(C)"}
object IN_C_C extends OpCode(0xED,0x48) with DestinationC with SourceC with InOper with HandleInOut with Size2 with T12 with Label {override val label:String="IN C,(C)"}
object IN_D_C extends OpCode(0xED,0x50) with DestinationC with SourceD with InOper with HandleInOut with Size2 with T12 with Label {override val label:String="IN D,(C)"}
object IN_E_C extends OpCode(0xED,0x58) with DestinationC with SourceE with InOper with HandleInOut with Size2 with T12 with Label {override val label:String="IN E,(C)"}
object IN_H_C extends OpCode(0xED,0x60) with DestinationC with SourceH with InOper with HandleInOut with Size2 with T12 with Label {override val label:String="IN H,(C)"}
object IN_L_C extends OpCode(0xED,0x68) with DestinationC with SourceL with InOper with HandleInOut with Size2 with T12 with Label {override val label:String="IN L,(C)"}
object OUT_n_A extends OpCode(0xD3) with DestinationN with SourceA with OutOper with Size2 with T11 with HandleInOut with Label {override val label:String="OUT (n),A"}
object OUT_C_A extends OpCode(0xED,0x79) with DestinationC with SourceA with OutOper with HandleInOut with Size2 with T12 with Label {override val label:String="OUT (C),A"}
object OUT_C_B extends OpCode(0xED,0x41) with DestinationC with SourceB with OutOper with HandleInOut with Size2 with T12 with Label {override val label:String="OUT (C),B"}
object OUT_C_C extends OpCode(0xED,0x49) with DestinationC with SourceC with OutOper with HandleInOut with Size2 with T12 with Label {override val label:String="OUT (C),C"}
object OUT_C_D extends OpCode(0xED,0x51) with DestinationC with SourceD with OutOper with HandleInOut with Size2 with T12 with Label {override val label:String="OUT (C),D"}
object OUT_C_E extends OpCode(0xED,0x59) with DestinationC with SourceE with OutOper with HandleInOut with Size2 with T12 with Label {override val label:String="OUT (C),E"}
object OUT_C_H extends OpCode(0xED,0x61) with DestinationC with SourceH with OutOper with HandleInOut with Size2 with T12 with Label {override val label:String="OUT (C),H"}
object OUT_C_L extends OpCode(0xED,0x69) with DestinationC with SourceL with OutOper with HandleInOut with Size2 with T12 with Label {override val label:String="OUT (C),L"}

//NOP
object NOP extends  OpCode(0x00) with Size1 with T4 with HandleNop with Label {override val label:String="NOP"}

//UNKNOWN
object UNKNOWN extends OpCode(OpCode.ANY) with HandleUnknown
class UNKNOWN_WITH_CODE(opcode:OpCode) extends OpCode(opcode.main,opcode.supp,opcode.supp2) with HandleUnknown