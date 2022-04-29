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
    ADD_A_A,ADD_A_B,ADD_A_C,ADD_A_D,ADD_A_E,ADD_A_H,ADD_A_L,ADD_A_HL,ADD_A_IX_d,ADD_A_IY_d,ADD_A_n)
}

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
