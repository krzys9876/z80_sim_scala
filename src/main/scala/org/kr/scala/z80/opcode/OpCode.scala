package org.kr.scala.z80.opcode

case class OpCode(main:Int,supp:Int=OpCode.ANY,supp2:Int=OpCode.ANY) {
  /* OpCode format:
  main (supp) (d) (supp2)
  main - primary OpCode for 1-byte opcodes (1st byte of any operation)
  supp - supplementary OpCode for 2-byte opcodes (2nd byte)
  d - offset for opcodes using indexed notation (IX/IY+d) (3rd byte)
  supp2 - second supplementary OpCode for 4-byte opcodes (some IX+d/IY+d) (4th byte)
   */
  override def toString: String = f"OpCode(${num2hex(main)}${if(supp!=OpCode.ANY) ","+num2hex(supp)}${if(supp2!=OpCode.ANY) ","+num2hex(supp2)})"

  private def num2hex(num:Int):String= f"0x$num%02X"

  def replaceCode(codeNo:Int,value:Int):OpCode=
    codeNo match {
      case 1 => OpCode(value,supp,supp2)
      case 2 => OpCode(main,value,supp2)
      case 3 => OpCode(main,supp,value)
    }

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
}

class UnknownOperationException(message : String) extends Exception(message) {}

trait OpCodePrinter {
  val label:String
  override def toString:String=label
}

class ADD_A(val label:String="ADD A,A") extends OpCode(0x87) with OpCodePrinter {}
class ADD_B(val label:String="ADD A,B") extends OpCode(0x80) with OpCodePrinter {}
class ADD_C(val label:String="ADD A,C") extends OpCode(0x81) with OpCodePrinter {}
class ADD_D(val label:String="ADD A,D") extends OpCode(0x82) with OpCodePrinter {}
class ADD_E(val label:String="ADD A,E") extends OpCode(0x83) with OpCodePrinter {}
class ADD_H(val label:String="ADD_A,H") extends OpCode(0x84) with OpCodePrinter {}
class ADD_L(val label:String="ADD A,L") extends OpCode(0x85) with OpCodePrinter {}






