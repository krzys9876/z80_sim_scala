package org.kr.scala.z80.opcode

case class OpCode(main:Int,supp:Int=OpCode.ANY,supp2:Int=OpCode.ANY) {
  /* OpCode format:
  main (supp) (d) (supp2)
  main - primary OpCode for 1-byte opcodes (1st byte of any operation)
  supp - supplementary OpCode for 2-byte opcodes (2nd byte)
  d - offset for opcodes using indexed notation (IX/IY+d) (3rd byte)
  supp2 - second supplementary OpCode for 4-byte opcodes (some IX+d/IY+d) (4th byte)
   */
  override def toString: String =
    f"OpCode(${num2hex(main)}${if(supp!=OpCode.ANY) ","+num2hex(supp) else ""}${if(supp2!=OpCode.ANY) ","+num2hex(supp2) else ""})"

  private def num2hex(num:Int):String= f"0x$num%02X"

  def replaceCode(codeNo:Int,value:Int):OpCode=
    codeNo match {
      case 1 => OpCode(value,supp,supp2)
      case 2 => OpCode(main,value,supp2)
      case 3 => OpCode(main,supp,value)
    }

  def mainSupp1Only:OpCode= replaceCode(3,OpCode.ANY)
  def mainOnly:OpCode= mainSupp1Only.replaceCode(2,OpCode.ANY)
  def equalsAny(opCode:OpCode):Boolean= this.equals(opCode) || this.equals(opCode.mainSupp1Only) || this.equals(opCode.mainOnly)

  def getCode(codeNo:Int):Int=
    codeNo match {
      case 1 => main
      case 2 => supp
      case 3 => supp2
    }
}

object OpCode {
  val ANY:Int = Int.MinValue
  val registerMap:Map[Int,LoadLocation]=Map(
    7->LoadLocation.register("A"),
    0->LoadLocation.register("B"),
    1->LoadLocation.register("C"),
    2->LoadLocation.register("D"),
    3->LoadLocation.register("E"),
    4->LoadLocation.register("H"),
    5->LoadLocation.register("L"))

  private def codeGenReg(base:Int,bit:Int):Map[Int,LoadLocation]={
    val baseOper=base & (~(0x07 << bit))
    registerMap.foldLeft(Map[Int,LoadLocation]())((m,entry)=>m++Map(baseOper+(entry._1 << bit)->entry._2))
  }

  private def codeGenBit(base:Int,bit:Int):Map[Int,Int]={
    val baseOper=base & (~(0x07 << bit))
    List.range(0,8).foldLeft(Map[Int,Int]())((m,elem)=>m++Map(baseOper+(elem << bit)->elem))
  }

  private def mapToOpCodeMap[To](base:OpCode,codeNo:Int,baseMap:Map[Int,To]):Map[List[OpCode],To]=
    baseMap.map(entry=>List(base.replaceCode(codeNo,entry._1))->entry._2)

  def generateMapByReg(base:OpCode, codeNo:Int, bit:Int):Map[List[OpCode],LoadLocation]={
    val regMap=codeGenReg(base.getCode(codeNo),bit)
    mapToOpCodeMap[LoadLocation](base,codeNo,regMap)
  }

  def generateMapByBit(base:OpCode, codeNo:Int, bit:Int):Map[List[OpCode],Int]={
    val bitMap=codeGenBit(base.getCode(codeNo),bit)
    mapToOpCodeMap[Int](base,codeNo,bitMap)
  }

  def generateListByReg(base:OpCode, codeNo:Int, bit:Int):List[OpCode]=
    generateMapByReg(base, codeNo, bit).keys.flatten.toList

  def generateListByBit(base:OpCode, codeNo:Int, bit:Int):List[OpCode]=
    generateMapByBit(base, codeNo, bit).keys.flatten.toList

  def getOpCodeObject(opCode:OpCode):OpCode=
    OpCodes.list.find(elem=>elem.equalsAny(opCode)).getOrElse(opCode)
}

class UnknownOperationException(message : String) extends Exception(message) {}

trait OpCodePrinter {
  val label:String
  override def toString:String=label
}

trait TestOp {}

object OpCodes {
  val list:List[OpCode]=List(
    ADD_A_A,ADD_A_B,ADD_A_C,ADD_A_D,ADD_A_E,ADD_A_H,ADD_A_L,ADD_A_HL,ADD_A_IX_d,ADD_A_IY_d,ADD_A_n,
    ADC_A_A,ADC_A_B,ADC_A_C,ADC_A_D,ADC_A_E,ADC_A_H,ADC_A_L,ADC_A_HL,ADC_A_IX_d,ADC_A_IY_d,ADC_A_n,
    SUB_A,SUB_B,SUB_C,SUB_D,SUB_E,SUB_H,SUB_L,SUB_HL,SUB_IX_d,SUB_IY_d,SUB_n,
    SBC_A_A,SBC_A_B,SBC_A_C,SBC_A_D,SBC_A_E,SBC_A_H,SBC_A_L,SBC_A_HL,SBC_A_IX_d,SBC_A_IY_d,SBC_A_n)
}

//ADD
object ADD_A_A extends OpCode(0x87) with OpCodePrinter {override val label:String="ADD A,A"}
object ADD_A_B extends OpCode(0x80) with OpCodePrinter {override val label:String="ADD A,B"}
object ADD_A_C extends OpCode(0x81) with OpCodePrinter with TestOp {override val label:String="ADD A,C"}
object ADD_A_D extends OpCode(0x82) with OpCodePrinter {override val label:String="ADD A,D"}
object ADD_A_E extends OpCode(0x83) with OpCodePrinter with TestOp {override val label:String="ADD A,E"}
object ADD_A_H extends OpCode(0x84) with OpCodePrinter {override val label:String="ADD A,H"}
object ADD_A_L extends OpCode(0x85) with OpCodePrinter {override val label:String="ADD A,L"}
object ADD_A_HL extends OpCode(0x86) with OpCodePrinter {override val label:String="ADD A,(HL)"}
object ADD_A_IX_d extends OpCode(0xDD,0x86) with OpCodePrinter {override val label:String="ADD A,(IX+d)"}
object ADD_A_IY_d extends OpCode(0xFD,0x86) with OpCodePrinter {override val label:String="ADD A,(IY+d)"}
object ADD_A_n extends OpCode(0xC6) with OpCodePrinter {override val label:String="ADD A,n"}
//ADC
object ADC_A_A extends OpCode(0x8F) with OpCodePrinter {override val label:String="ADC A,A"}
object ADC_A_B extends OpCode(0x88) with OpCodePrinter {override val label:String="ADC A,B"}
object ADC_A_C extends OpCode(0x89) with OpCodePrinter with TestOp {override val label:String="ADC A,C"}
object ADC_A_D extends OpCode(0x8A) with OpCodePrinter {override val label:String="ADC A,D"}
object ADC_A_E extends OpCode(0x8B) with OpCodePrinter with TestOp {override val label:String="ADC A,E"}
object ADC_A_H extends OpCode(0x8C) with OpCodePrinter {override val label:String="ADC A,H"}
object ADC_A_L extends OpCode(0x8D) with OpCodePrinter {override val label:String="ADC A,L"}
object ADC_A_HL extends OpCode(0x8E) with OpCodePrinter {override val label:String="ADC A,(HL)"}
object ADC_A_IX_d extends OpCode(0xDD,0x8E) with OpCodePrinter {override val label:String="ADC A,(IX+d)"}
object ADC_A_IY_d extends OpCode(0xFD,0x8E) with OpCodePrinter {override val label:String="ADC A,(IY+d)"}
object ADC_A_n extends OpCode(0xCE) with OpCodePrinter {override val label:String="ADC A,n"}
//SUB
object SUB_A extends OpCode(0x97) with OpCodePrinter {override val label:String="SUB A"}
object SUB_B extends OpCode(0x90) with OpCodePrinter {override val label:String="SUB B"}
object SUB_C extends OpCode(0x91) with OpCodePrinter with TestOp {override val label:String="SUB C"}
object SUB_D extends OpCode(0x92) with OpCodePrinter {override val label:String="SUB D"}
object SUB_E extends OpCode(0x93) with OpCodePrinter with TestOp {override val label:String="SUB E"}
object SUB_H extends OpCode(0x94) with OpCodePrinter {override val label:String="SUB H"}
object SUB_L extends OpCode(0x95) with OpCodePrinter {override val label:String="SUB L"}
object SUB_HL extends OpCode(0x96) with OpCodePrinter {override val label:String="SUB (HL)"}
object SUB_IX_d extends OpCode(0xDD,0x96) with OpCodePrinter {override val label:String="SUB (IX+d)"}
object SUB_IY_d extends OpCode(0xFD,0x96) with OpCodePrinter {override val label:String="SUB (IY+d)"}
object SUB_n extends OpCode(0xD6) with OpCodePrinter {override val label:String="SUB n"}
//SBC
object SBC_A_A extends OpCode(0x9F) with OpCodePrinter {override val label:String="SBC A,A"}
object SBC_A_B extends OpCode(0x98) with OpCodePrinter {override val label:String="SBC A,B"}
object SBC_A_C extends OpCode(0x99) with OpCodePrinter with TestOp {override val label:String="SBC A,C"}
object SBC_A_D extends OpCode(0x9A) with OpCodePrinter {override val label:String="SBC A,D"}
object SBC_A_E extends OpCode(0x9B) with OpCodePrinter with TestOp {override val label:String="SBC A,E"}
object SBC_A_H extends OpCode(0x9C) with OpCodePrinter {override val label:String="SBC A,H"}
object SBC_A_L extends OpCode(0x9D) with OpCodePrinter {override val label:String="SBC A,L"}
object SBC_A_HL extends OpCode(0x9E) with OpCodePrinter {override val label:String="SBC A,(HL)"}
object SBC_A_IX_d extends OpCode(0xDD,0x9E) with OpCodePrinter {override val label:String="SBC A,(IX+d)"}
object SBC_A_IY_d extends OpCode(0xFD,0x9E) with OpCodePrinter {override val label:String="SBC A,(IY+d)"}
object SBC_A_n extends OpCode(0xDE) with OpCodePrinter {override val label:String="SBC A,n"}
//AND
object AND_A extends OpCode(0xA7) with OpCodePrinter {override val label:String="AND A"}
object AND_B extends OpCode(0xA0) with OpCodePrinter {override val label:String="AND B"}
object AND_C extends OpCode(0xA1) with OpCodePrinter with TestOp {override val label:String="AND C"}
object AND_D extends OpCode(0xA2) with OpCodePrinter {override val label:String="AND D"}
object AND_E extends OpCode(0xA3) with OpCodePrinter with TestOp {override val label:String="AND E"}
object AND_H extends OpCode(0xA4) with OpCodePrinter {override val label:String="AND H"}
object AND_L extends OpCode(0xA5) with OpCodePrinter {override val label:String="AND L"}
object AND_HL extends OpCode(0xA6) with OpCodePrinter {override val label:String="AND (HL)"}
object AND_IX_d extends OpCode(0xDD,0xA6) with OpCodePrinter {override val label:String="AND (IX+d)"}
object AND_IY_d extends OpCode(0xFD,0xA6) with OpCodePrinter {override val label:String="AND (IY+d)"}
object AND_n extends OpCode(0xE6) with OpCodePrinter {override val label:String="AND n"}
