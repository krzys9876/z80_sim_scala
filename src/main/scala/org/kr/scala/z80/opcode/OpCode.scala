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

trait Label {
  val label:String
  override def toString:String=label
}

//Building blocks for OpCode definition
trait OpCodeSourceLocation {
  val source:LoadLocation
}
trait SourceA extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("A")}
trait SourceB extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("B")}
trait SourceC extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("C")}
trait SourceD extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("D")}
trait SourceE extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("E")}
trait SourceH extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("H")}
trait SourceL extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("L")}
trait SourceBC extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("BC")}
trait SourceDE extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("DE")}
trait SourceHL extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.registerAddr("HL")}
trait SourceHLr extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("HL")}
trait SourceSP extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("SP")}
trait SourceIX extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("IX")}
trait SourceIY extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.register("IY")}
trait SourceIXd extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.registerAddrIndirOffset("IX", 2)}
trait SourceIYd extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.registerAddrIndirOffset("IY", 2)}
trait SourceN extends OpCodeSourceLocation {override val source:LoadLocation=LoadLocation.registerAddrDirOffset("PC", 1)}

trait OpCodeDestLocation {
  val destination:LoadLocation
}
trait DestinationA extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.register("A")}
trait DestinationB extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.register("B")}
trait DestinationC extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.register("C")}
trait DestinationD extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.register("D")}
trait DestinationE extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.register("E")}
trait DestinationH extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.register("H")}
trait DestinationL extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.register("L")}
trait DestinationHL extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.registerAddr("HL")}
trait DestinationIXd extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.registerAddrIndirOffset("IX", 2)}
trait DestinationIYd extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.registerAddrIndirOffset("IY", 2)}
trait DestinationN extends OpCodeDestLocation {override val destination:LoadLocation=LoadLocation.registerAddrDirOffset("PC", 1)}

trait SourceDestA extends SourceA with DestinationA
trait SourceDestB extends SourceB with DestinationB
trait SourceDestC extends SourceC with DestinationC
trait SourceDestD extends SourceD with DestinationD
trait SourceDestE extends SourceE with DestinationE
trait SourceDestH extends SourceH with DestinationH
trait SourceDestL extends SourceL with DestinationL
trait SourceDestHL extends SourceHL with DestinationHL
trait SourceDestIXd extends SourceIXd with DestinationIXd
trait SourceDestIYd extends SourceIYd with DestinationIYd

trait OpCodeOperandLocation {
  val operand:LoadLocation
}
trait OperandA extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.register("A")}
trait OperandB extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.register("B")}
trait OperandC extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.register("C")}
trait OperandD extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.register("D")}
trait OperandE extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.register("E")}
trait OperandH extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.register("H")}
trait OperandL extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.register("L")}
trait OperandHL extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.registerAddr("HL")}
trait OperandIXd extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.registerAddrIndirOffset("IX", 2)}
trait OperandIYd extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.registerAddrIndirOffset("IY", 2)}
trait OperandN extends OpCodeOperandLocation {override val operand:LoadLocation=LoadLocation.registerAddrDirOffset("PC", 1)}

trait OpCodeArithmetic8b {
  val operation:ArithmeticOperation
}
trait Arith8bAdd extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Add8b}
trait Arith8bAddC extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=AddC8b}
trait Arith8bSub extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Sub8b}
trait Arith8bSubC extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=SubC8b}
trait Arith8bAnd extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=And8b}
trait Arith8bXor extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Xor8b}
trait Arith8bOr extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Or8b}
trait Arith8bCp extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Comp8b}
trait Arith8bInc extends OpCodeArithmetic8b {override val operation:ArithmeticOperation=Inc8b}
trait Arith8bDec extends OpCodeArithmetic8b {override val operation:ArithmeticOperation=Dec8b}
trait Arith8bCpl extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Cpl8b}
trait Arith8bNeg extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Neg8b}
trait Arith8bCcf extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Ccf8b}
trait Arith8bScf extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Scf8b}

trait OpCodeSize {
  val size:Int
}

trait OpCodeArithmetic16b {
  val operation:ArithmeticOperation
}
trait Arith16bAdd extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Add16b}
trait Arith16bAddC extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=AddC16b}
trait Arith16bSubC extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=SubC16b}
trait Arith16bInc extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Inc16b}
trait Arith16bDec extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Dec16b}

trait Size1 extends OpCodeSize {override val size:Int=1}
trait Size2 extends OpCodeSize {override val size:Int=2}
trait Size3 extends OpCodeSize {override val size:Int=3}
trait Size4 extends OpCodeSize {override val size:Int=4}

object OpCodes {
  val list:List[OpCode]=List(
    //Arithmetic8b
    ADD_A_A,ADD_A_B,ADD_A_C,ADD_A_D,ADD_A_E,ADD_A_H,ADD_A_L,ADD_A_HL,ADD_A_IX_d,ADD_A_IY_d,ADD_A_n,
    ADC_A_A,ADC_A_B,ADC_A_C,ADC_A_D,ADC_A_E,ADC_A_H,ADC_A_L,ADC_A_HL,ADC_A_IX_d,ADC_A_IY_d,ADC_A_n,
    SUB_A,SUB_B,SUB_C,SUB_D,SUB_E,SUB_H,SUB_L,SUB_HL,SUB_IX_d,SUB_IY_d,SUB_n,
    SBC_A_A,SBC_A_B,SBC_A_C,SBC_A_D,SBC_A_E,SBC_A_H,SBC_A_L,SBC_A_HL,SBC_A_IX_d,SBC_A_IY_d,SBC_A_n,
    AND_A,AND_B,AND_C,AND_D,AND_E,AND_H,AND_L,AND_HL,AND_IX_d,AND_IY_d,AND_n,
    XOR_A,XOR_B,XOR_C,XOR_D,XOR_E,XOR_H,XOR_L,XOR_HL,XOR_IX_d,XOR_IY_d,XOR_n,
    OR_A,OR_B,OR_C,OR_D,OR_E,OR_H,OR_L,OR_HL,OR_IX_d,OR_IY_d,OR_n,
    CP_A,CP_B,CP_C,CP_D,CP_E,CP_H,CP_L,CP_HL,CP_IX_d,CP_IY_d,CP_n,
    INC_A,INC_B,INC_C,INC_D,INC_E,INC_H,INC_L,INC_HL,INC_IX_d,INC_IY_d,
    DEC_A,DEC_B,DEC_C,DEC_D,DEC_E,DEC_H,DEC_L,DEC_HL,DEC_IX_d,DEC_IY_d,
    CPL,SCF,CCF,NEG,
    //Arithmetic16b
    ADD_HL_BC,ADD_HL_DE,ADD_HL_HL,ADD_HL_SP,ADD_IX_BC,ADD_IX_DE,ADD_IX_IX,ADD_IX_SP,
    ADD_IY_BC,ADD_IY_DE,ADD_IY_IY,ADD_IY_SP,ADC_HL_BC,ADC_HL_DE,ADC_HL_HL,ADC_HL_SP,
    SBC_HL_BC,SBC_HL_DE,SBC_HL_HL,SBC_HL_SP,INC_BC,INC_DE,INC_HL_16,INC_SP,INC_IX,INC_IY,
    DEC_BC,DEC_DE,DEC_HL_16,DEC_SP,DEC_IX,DEC_IY
  )

  //private def filterTo(pred:OpCode=>Boolean):List[OpCode]=list.filter(pred(_))

  private def opCodeListToMap[OpCodeAttr,Attr](getAttr:OpCodeAttr=>Attr):Map[List[OpCode],Attr]=
    list.filter(_.isInstanceOf[OpCodeAttr]).map(op=>List(op)->getAttr(op.asInstanceOf[OpCodeAttr])).toMap

  //TODO: flatten list - refactor OpCodeMap
  //NOTE: cannot use generics in vals (only defs) - these maps are used in vals in other classes
  val operandMap:Map[List[OpCode],LoadLocation]= //opCodeListToMap[OpCodeOperandLocation,LoadLocation](op=>op.operand)
  //filterTo(op=>op.isInstanceOf[OpCodeOperandLocation]).map(op=> List(op)->op.asInstanceOf[OpCodeOperandLocation].operand).toMap
    list
    .filter(_.isInstanceOf[OpCodeOperandLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeOperandLocation].operand).toMap
  val sourceMap:Map[List[OpCode],LoadLocation]= list
    .filter(_.isInstanceOf[OpCodeSourceLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeSourceLocation].source).toMap
  val destinationMap:Map[List[OpCode],LoadLocation]= list
    .filter(_.isInstanceOf[OpCodeDestLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeDestLocation].destination).toMap
  val operation8bMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeArithmetic8b])
    .map(op=> List(op)->op.asInstanceOf[OpCodeArithmetic8b].operation).toMap
  val operation16bMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeArithmetic16b])
    .map(op=> List(op)->op.asInstanceOf[OpCodeArithmetic16b].operation).toMap
  val sizeMap:Map[List[OpCode],Int]= list
    .filter(_.isInstanceOf[OpCodeSize])
    .map(op=> List(op)->op.asInstanceOf[OpCodeSize].size).toMap
}

//Arithmetic 8b
//ADD
object ADD_A_A extends OpCode(0x87) with Arith8bAdd with OperandA with Size1 with Label {override val label:String="ADD A,A"}
object ADD_A_B extends OpCode(0x80) with Arith8bAdd with OperandB with Size1 with Label {override val label:String="ADD A,B"}
object ADD_A_C extends OpCode(0x81) with Arith8bAdd with OperandC with Size1 with Label {override val label:String="ADD A,C"}
object ADD_A_D extends OpCode(0x82) with Arith8bAdd with OperandD with Size1 with Label {override val label:String="ADD A,D"}
object ADD_A_E extends OpCode(0x83) with Arith8bAdd with OperandE with Size1 with Label {override val label:String="ADD A,E"}
object ADD_A_H extends OpCode(0x84) with Arith8bAdd with OperandH with Size1 with Label {override val label:String="ADD A,H"}
object ADD_A_L extends OpCode(0x85) with Arith8bAdd with OperandL with Size1 with Label {override val label:String="ADD A,L"}
object ADD_A_HL extends OpCode(0x86) with Arith8bAdd with OperandHL with Size1 with Label {override val label:String="ADD A,(HL)"}
object ADD_A_IX_d extends OpCode(0xDD,0x86) with Arith8bAdd with OperandIXd with Size3 with Label {override val label:String="ADD A,(IX+d)"}
object ADD_A_IY_d extends OpCode(0xFD,0x86) with Arith8bAdd with OperandIYd with Size3 with Label {override val label:String="ADD A,(IY+d)"}
object ADD_A_n extends OpCode(0xC6) with Arith8bAdd with OperandN with Size2 with Label {override val label:String="ADD A,n"}
//ADC
object ADC_A_A extends OpCode(0x8F) with Arith8bAddC with OperandA with Size1 with Label {override val label:String="ADC A,A"}
object ADC_A_B extends OpCode(0x88) with Arith8bAddC with OperandB with Size1 with Label {override val label:String="ADC A,B"}
object ADC_A_C extends OpCode(0x89) with Arith8bAddC with OperandC with Size1 with Label {override val label:String="ADC A,C"}
object ADC_A_D extends OpCode(0x8A) with Arith8bAddC with OperandD with Size1 with Label {override val label:String="ADC A,D"}
object ADC_A_E extends OpCode(0x8B) with Arith8bAddC with OperandE with Size1 with Label {override val label:String="ADC A,E"}
object ADC_A_H extends OpCode(0x8C) with Arith8bAddC with OperandH with Size1 with Label {override val label:String="ADC A,H"}
object ADC_A_L extends OpCode(0x8D) with Arith8bAddC with OperandL with Size1 with Label {override val label:String="ADC A,L"}
object ADC_A_HL extends OpCode(0x8E) with Arith8bAddC with OperandHL with Size1 with Label {override val label:String="ADC A,(HL)"}
object ADC_A_IX_d extends OpCode(0xDD,0x8E) with Arith8bAddC with OperandIXd with Size3 with Label {override val label:String="ADC A,(IX+d)"}
object ADC_A_IY_d extends OpCode(0xFD,0x8E) with Arith8bAddC with OperandIYd with Size3 with Label {override val label:String="ADC A,(IY+d)"}
object ADC_A_n extends OpCode(0xCE) with Arith8bAddC with OperandN with Size2 with Label {override val label:String="ADC A,n"}
//SUB
object SUB_A extends OpCode(0x97) with Arith8bSub with OperandA with Size1 with Label {override val label:String="SUB A"}
object SUB_B extends OpCode(0x90) with Arith8bSub with OperandB with Size1 with Label {override val label:String="SUB B"}
object SUB_C extends OpCode(0x91) with Arith8bSub with OperandC with Size1 with Label {override val label:String="SUB C"}
object SUB_D extends OpCode(0x92) with Arith8bSub with OperandD with Size1 with Label {override val label:String="SUB D"}
object SUB_E extends OpCode(0x93) with Arith8bSub with OperandE with Size1 with Label {override val label:String="SUB E"}
object SUB_H extends OpCode(0x94) with Arith8bSub with OperandH with Size1 with Label {override val label:String="SUB H"}
object SUB_L extends OpCode(0x95) with Arith8bSub with OperandL with Size1 with Label {override val label:String="SUB L"}
object SUB_HL extends OpCode(0x96) with Arith8bSub with OperandHL with Size1 with Label {override val label:String="SUB (HL)"}
object SUB_IX_d extends OpCode(0xDD,0x96) with Arith8bSub with OperandIXd with Size3 with Label {override val label:String="SUB (IX+d)"}
object SUB_IY_d extends OpCode(0xFD,0x96) with Arith8bSub with OperandIYd with Size3 with Label {override val label:String="SUB (IY+d)"}
object SUB_n extends OpCode(0xD6) with Arith8bSub with OperandN with Size2 with Label {override val label:String="SUB n"}
//SBC
object SBC_A_A extends OpCode(0x9F) with Arith8bSubC with OperandA with Size1 with Label {override val label:String="SBC A,A"}
object SBC_A_B extends OpCode(0x98) with Arith8bSubC with OperandB with Size1 with Label {override val label:String="SBC A,B"}
object SBC_A_C extends OpCode(0x99) with Arith8bSubC with OperandC with Size1 with Label {override val label:String="SBC A,C"}
object SBC_A_D extends OpCode(0x9A) with Arith8bSubC with OperandD with Size1 with Label {override val label:String="SBC A,D"}
object SBC_A_E extends OpCode(0x9B) with Arith8bSubC with OperandE with Size1 with Label {override val label:String="SBC A,E"}
object SBC_A_H extends OpCode(0x9C) with Arith8bSubC with OperandH with Size1 with Label {override val label:String="SBC A,H"}
object SBC_A_L extends OpCode(0x9D) with Arith8bSubC with OperandL with Size1 with Label {override val label:String="SBC A,L"}
object SBC_A_HL extends OpCode(0x9E) with Arith8bSubC with OperandHL with Size1 with Label {override val label:String="SBC A,(HL)"}
object SBC_A_IX_d extends OpCode(0xDD,0x9E) with Arith8bSubC with OperandIXd with Size3 with Label {override val label:String="SBC A,(IX+d)"}
object SBC_A_IY_d extends OpCode(0xFD,0x9E) with Arith8bSubC with OperandIYd with Size3 with Label {override val label:String="SBC A,(IY+d)"}
object SBC_A_n extends OpCode(0xDE) with Arith8bSubC with OperandN with Size2 with Label {override val label:String="SBC A,n"}
//AND
object AND_A extends OpCode(0xA7) with Arith8bAnd with OperandA with Size1 with Label {override val label:String="AND A"}
object AND_B extends OpCode(0xA0) with Arith8bAnd with OperandB with Size1 with Label {override val label:String="AND B"}
object AND_C extends OpCode(0xA1) with Arith8bAnd with OperandC with Size1 with Label {override val label:String="AND C"}
object AND_D extends OpCode(0xA2) with Arith8bAnd with OperandD with Size1 with Label {override val label:String="AND D"}
object AND_E extends OpCode(0xA3) with Arith8bAnd with OperandE with Size1 with Label {override val label:String="AND E"}
object AND_H extends OpCode(0xA4) with Arith8bAnd with OperandH with Size1 with Label {override val label:String="AND H"}
object AND_L extends OpCode(0xA5) with Arith8bAnd with OperandL with Size1 with Label {override val label:String="AND L"}
object AND_HL extends OpCode(0xA6) with Arith8bAnd with OperandHL with Size1 with Label {override val label:String="AND (HL)"}
object AND_IX_d extends OpCode(0xDD,0xA6) with Arith8bAnd with OperandIXd with Size3 with Label {override val label:String="AND (IX+d)"}
object AND_IY_d extends OpCode(0xFD,0xA6) with Arith8bAnd with OperandIYd with Size3 with Label {override val label:String="AND (IY+d)"}
object AND_n extends OpCode(0xE6) with Arith8bAnd with OperandN with Size2 with Label {override val label:String="AND n"}
//XOR
object XOR_A extends OpCode(0xAF) with Arith8bXor with OperandA with Size1 with Label {override val label:String="XOR A"}
object XOR_B extends OpCode(0xA8) with Arith8bXor with OperandB with Size1 with Label {override val label:String="XOR B"}
object XOR_C extends OpCode(0xA9) with Arith8bXor with OperandC with Size1 with Label {override val label:String="XOR C"}
object XOR_D extends OpCode(0xAA) with Arith8bXor with OperandD with Size1 with Label {override val label:String="XOR D"}
object XOR_E extends OpCode(0xAB) with Arith8bXor with OperandE with Size1 with Label {override val label:String="XOR E"}
object XOR_H extends OpCode(0xAC) with Arith8bXor with OperandH with Size1 with Label {override val label:String="XOR H"}
object XOR_L extends OpCode(0xAD) with Arith8bXor with OperandL with Size1 with Label {override val label:String="XOR L"}
object XOR_HL extends OpCode(0xAE) with Arith8bXor with OperandHL with Size1 with Label {override val label:String="XOR (HL)"}
object XOR_IX_d extends OpCode(0xDD,0xAE) with Arith8bXor with OperandIXd with Size3 with Label {override val label:String="XOR (IX+d)"}
object XOR_IY_d extends OpCode(0xFD,0xAE) with Arith8bXor with OperandIYd with Size3 with Label {override val label:String="XOR (IY+d)"}
object XOR_n extends OpCode(0xEE) with Arith8bXor with OperandN with Size2 with Label {override val label:String="XOR n"}
//OR
object OR_A extends OpCode(0xB7) with Arith8bOr with OperandA with Size1 with Label {override val label:String="OR A"}
object OR_B extends OpCode(0xB0) with Arith8bOr with OperandB with Size1 with Label {override val label:String="OR B"}
object OR_C extends OpCode(0xB1) with Arith8bOr with OperandC with Size1 with Label {override val label:String="OR C"}
object OR_D extends OpCode(0xB2) with Arith8bOr with OperandD with Size1 with Label {override val label:String="OR D"}
object OR_E extends OpCode(0xB3) with Arith8bOr with OperandE with Size1 with Label {override val label:String="OR E"}
object OR_H extends OpCode(0xB4) with Arith8bOr with OperandH with Size1 with Label {override val label:String="OR H"}
object OR_L extends OpCode(0xB5) with Arith8bOr with OperandL with Size1 with Label {override val label:String="OR L"}
object OR_HL extends OpCode(0xB6) with Arith8bOr with OperandHL with Size1 with Label {override val label:String="OR (HL)"}
object OR_IX_d extends OpCode(0xDD,0xB6) with Arith8bOr with OperandIXd with Size3 with Label {override val label:String="OR (IX+d)"}
object OR_IY_d extends OpCode(0xFD,0xB6) with Arith8bOr with OperandIYd with Size3 with Label {override val label:String="OR (IY+d)"}
object OR_n extends OpCode(0xF6) with Arith8bOr with OperandN with Size2 with Label {override val label:String="OR n"}
//CP
object CP_A extends OpCode(0xBF) with Arith8bCp with OperandA with Size1 with Label {override val label:String="CP A"}
object CP_B extends OpCode(0xB8) with Arith8bCp with OperandB with Size1 with Label {override val label:String="CP B"}
object CP_C extends OpCode(0xB9) with Arith8bCp with OperandC with Size1 with Label {override val label:String="CP C"}
object CP_D extends OpCode(0xBA) with Arith8bCp with OperandD with Size1 with Label {override val label:String="CP D"}
object CP_E extends OpCode(0xBB) with Arith8bCp with OperandE with Size1 with Label {override val label:String="CP E"}
object CP_H extends OpCode(0xBC) with Arith8bCp with OperandH with Size1 with Label {override val label:String="CP H"}
object CP_L extends OpCode(0xBD) with Arith8bCp with OperandL with Size1 with Label {override val label:String="CP L"}
object CP_HL extends OpCode(0xBE) with Arith8bCp with OperandHL with Size1 with Label {override val label:String="CP (HL)"}
object CP_IX_d extends OpCode(0xDD,0xBE) with Arith8bCp with OperandIXd with Size3 with Label {override val label:String="CP (IX+d)"}
object CP_IY_d extends OpCode(0xFD,0xBE) with Arith8bCp with OperandIYd with Size3 with Label {override val label:String="CP (IY+d)"}
object CP_n extends OpCode(0xFE) with Arith8bCp with OperandN with Size2 with Label {override val label:String="CP n"}
//INC
object INC_A extends OpCode(0x3C) with Arith8bInc with SourceDestA with OperandA with Size1 with Label {override val label:String="INC A"}
object INC_B extends OpCode(0x04) with Arith8bInc with SourceDestB with OperandB with Size1 with Label {override val label:String="INC B"}
object INC_C extends OpCode(0x0C) with Arith8bInc with SourceDestC with OperandC with Size1 with Label {override val label:String="INC C"}
object INC_D extends OpCode(0x14) with Arith8bInc with SourceDestD with OperandD with Size1 with Label {override val label:String="INC D"}
object INC_E extends OpCode(0x1C) with Arith8bInc with SourceDestE with OperandE with Size1 with Label {override val label:String="INC E"}
object INC_H extends OpCode(0x24) with Arith8bInc with SourceDestH with OperandH with Size1 with Label {override val label:String="INC H"}
object INC_L extends OpCode(0x2C) with Arith8bInc with SourceDestL with OperandL with Size1 with Label {override val label:String="INC L"}
object INC_HL extends OpCode(0x34) with Arith8bInc with SourceDestHL with OperandHL with Size1 with Label {override val label:String="INC (HL)"}
object INC_IX_d extends OpCode(0xDD,0x34) with Arith8bInc with SourceDestIXd with OperandIXd with Size3 with Label {override val label:String="INC (IX+d)"}
object INC_IY_d extends OpCode(0xFD,0x34) with Arith8bInc with SourceDestIYd with OperandIYd with Size3 with Label {override val label:String="INC (IY+d)"}
//DEC
object DEC_A extends OpCode(0x3D) with Arith8bDec with SourceDestA with OperandA with Size1 with Label {override val label:String="DEC A"}
object DEC_B extends OpCode(0x05) with Arith8bDec with SourceDestB with OperandB with Size1 with Label {override val label:String="DEC B"}
object DEC_C extends OpCode(0x0D) with Arith8bDec with SourceDestC with OperandC with Size1 with Label {override val label:String="DEC C"}
object DEC_D extends OpCode(0x15) with Arith8bDec with SourceDestD with OperandD with Size1 with Label {override val label:String="DEC D"}
object DEC_E extends OpCode(0x1D) with Arith8bDec with SourceDestE with OperandE with Size1 with Label {override val label:String="DEC E"}
object DEC_H extends OpCode(0x25) with Arith8bDec with SourceDestH with OperandH with Size1 with Label {override val label:String="DEC H"}
object DEC_L extends OpCode(0x2D) with Arith8bDec with SourceDestL with OperandL with Size1 with Label {override val label:String="DEC L"}
object DEC_HL extends OpCode(0x35) with Arith8bDec with SourceDestHL with OperandHL with Size1 with Label {override val label:String="DEC (HL)"}
object DEC_IX_d extends OpCode(0xDD,0x35) with Arith8bDec with SourceDestIXd with OperandIXd with Size3 with Label {override val label:String="DEC (IX+d)"}
object DEC_IY_d extends OpCode(0xFD,0x35) with Arith8bDec with SourceDestIYd with OperandIYd with Size3 with Label {override val label:String="DEC (IY+d)"}
//
object CPL extends OpCode(0x2F) with Arith8bCpl with OperandA with Size1 with Label {override val label:String="CPL"}
object NEG extends OpCode(0xED,0x44) with Arith8bNeg with OperandA with Size2 with Label {override val label:String="NEG"}
object SCF extends OpCode(0x37) with Arith8bScf with Size1 with Label {override val label:String="SCF"}
object CCF extends OpCode(0x3F) with Arith8bCcf with Size1 with Label {override val label:String="CCF"}

//Arithmetic 16b
object ADD_HL_BC extends OpCode(0x09) with Arith16bAdd with SourceBC with Size1 with Label {override val label:String="ADD HL,BC"}
object ADD_HL_DE extends OpCode(0x19) with Arith16bAdd with SourceDE with Size1 with Label {override val label:String="ADD HL,DE"}
object ADD_HL_HL extends OpCode(0x29) with Arith16bAdd with SourceHLr with Size1 with Label {override val label:String="ADD HL,HL"}
object ADD_HL_SP extends OpCode(0x39) with Arith16bAdd with SourceSP with Size1 with Label {override val label:String="ADD HL,SP"}
object ADD_IX_BC extends OpCode(0xDD,0x09) with Arith16bAdd with SourceBC with Size2 with Label {override val label:String="ADD IX,BC"}
object ADD_IX_DE extends OpCode(0xDD,0x19) with Arith16bAdd with SourceDE with Size2 with Label {override val label:String="ADD IX,DE"}
object ADD_IX_IX extends OpCode(0xDD,0x29) with Arith16bAdd with SourceIX with Size2 with Label {override val label:String="ADD IX,IX"}
object ADD_IX_SP extends OpCode(0xDD,0x39) with Arith16bAdd with SourceSP with Size2 with Label {override val label:String="ADD IX,SP"}
object ADD_IY_BC extends OpCode(0xFD,0x09) with Arith16bAdd with SourceBC with Size2 with Label {override val label:String="ADD IY,BC"}
object ADD_IY_DE extends OpCode(0xFD,0x19) with Arith16bAdd with SourceDE with Size2 with Label {override val label:String="ADD IY,DE"}
object ADD_IY_IY extends OpCode(0xFD,0x29) with Arith16bAdd with SourceIY with Size2 with Label {override val label:String="ADD IY,IX"}
object ADD_IY_SP extends OpCode(0xFD,0x39) with Arith16bAdd with SourceSP with Size2 with Label {override val label:String="ADD IY,SP"}
object ADC_HL_BC extends OpCode(0xED,0x4A) with Arith16bAddC with SourceBC with Size2 with Label {override val label:String="ADC HL,BC"}
object ADC_HL_DE extends OpCode(0xED,0x5A) with Arith16bAddC with SourceDE with Size2 with Label {override val label:String="ADC HL,DE"}
object ADC_HL_HL extends OpCode(0xED,0x6A) with Arith16bAddC with SourceHLr with Size2 with Label {override val label:String="ADC HL,HL"}
object ADC_HL_SP extends OpCode(0xED,0x7A) with Arith16bAddC with SourceSP with Size2 with Label {override val label:String="ADC HL,SP"}
object SBC_HL_BC extends OpCode(0xED,0x42) with Arith16bSubC with SourceBC with Size2 with Label {override val label:String="SBC HL,BC"}
object SBC_HL_DE extends OpCode(0xED,0x52) with Arith16bSubC with SourceDE with Size2 with Label {override val label:String="SBC HL,DE"}
object SBC_HL_HL extends OpCode(0xED,0x62) with Arith16bSubC with SourceHLr with Size2 with Label {override val label:String="SBC HL,HL"}
object SBC_HL_SP extends OpCode(0xED,0x72) with Arith16bSubC with SourceSP with Size2 with Label {override val label:String="SBC HL,SP"}
object INC_BC extends OpCode(0x03) with Arith16bInc with SourceBC with Size1 with Label {override val label:String="INC BC"}
object INC_DE extends OpCode(0x13) with Arith16bInc with SourceDE with Size1 with Label {override val label:String="INC DE"}
object INC_HL_16 extends OpCode(0x23) with Arith16bInc with SourceHLr with Size1 with Label {override val label:String="INC HL"}
object INC_SP extends OpCode(0x33) with Arith16bInc with SourceSP with Size1 with Label {override val label:String="INC SP"}
object INC_IX extends OpCode(0xDD,0x23) with Arith16bInc with SourceIX with Size2 with Label {override val label:String="INC IX"}
object INC_IY extends OpCode(0xFD,0x23) with Arith16bInc with SourceIY with Size2 with Label {override val label:String="INC IY"}
object DEC_BC extends OpCode(0x0B) with Arith16bDec with SourceBC with Size1 with Label {override val label:String="DEC BC"}
object DEC_DE extends OpCode(0x1B) with Arith16bDec with SourceDE with Size1 with Label {override val label:String="DEC DE"}
object DEC_HL_16 extends OpCode(0x2B) with Arith16bDec with SourceHLr with Size1 with Label {override val label:String="DEC HL"}
object DEC_SP extends OpCode(0x3B) with Arith16bDec with SourceSP with Size1 with Label {override val label:String="DEC SP"}
object DEC_IX extends OpCode(0xDD,0x2B) with Arith16bDec with SourceIX with Size2 with Label {override val label:String="DEC IX"}
object DEC_IY extends OpCode(0xFD,0x2B) with Arith16bDec with SourceIY with Size2 with Label {override val label:String="DEC IY"}
