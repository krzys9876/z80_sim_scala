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

  lazy val numberOfCodes:Int= {
    (main,supp,supp2) match {
      case (OpCode.ANY,OpCode.ANY,OpCode.ANY) => 0
      case (_,OpCode.ANY,OpCode.ANY) => 1
      case (_,_,OpCode.ANY) => 2
      case (_,_,_) => 3
    }
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
  val registerMap:Map[Int,Location]=Map(
    7->Location.register("A"),
    0->Location.register("B"),
    1->Location.register("C"),
    2->Location.register("D"),
    3->Location.register("E"),
    4->Location.register("H"),
    5->Location.register("L"))

  private def codeGenReg(base:Int,bit:Int):Map[Int,Location]={
    val baseOper=base & (~(0x07 << bit))
    registerMap.foldLeft(Map[Int,Location]())((m, entry)=>m++Map(baseOper+(entry._1 << bit)->entry._2))
  }

  private def codeGenBit(base:Int,bit:Int):Map[Int,Int]={
    val baseOper=base & (~(0x07 << bit))
    List.range(0,8).foldLeft(Map[Int,Int]())((m,elem)=>m++Map(baseOper+(elem << bit)->elem))
  }

  private def mapToOpCodeMap[To](base:OpCode,codeNo:Int,baseMap:Map[Int,To]):Map[List[OpCode],To]=
    baseMap.map(entry=>List(base.replaceCode(codeNo,entry._1))->entry._2)

  def generateMapByReg(base:OpCode, codeNo:Int, bit:Int):Map[List[OpCode],Location]={
    val regMap=codeGenReg(base.getCode(codeNo),bit)
    mapToOpCodeMap[Location](base,codeNo,regMap)
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

  def generateFromLists(opcodes:List[OpCode],locations:List[Location],sizes:List[Int]):List[(OpCode,Location,Int)]=
    opcodes.zip(locations).zip(sizes).map(entry=>(entry._1._1,entry._1._2,entry._2))

  //TYPE1: registers decoded by bits 0-2 or 3-5, opcode pattern: B-L,A: 0x01-0x07, (HL): 0x06, (IX+d),(IY+d): (0xDD,0x06), (0xFD,0x06)
  val baseCodesType1:List[OpCode]=List(OpCode(0x07),OpCode(0x00),OpCode(0x01),
    OpCode(0x02),OpCode(0x03),OpCode(0x04),OpCode(0x05),
    OpCode(0x06),OpCode(0xDD,0x06),OpCode(0xFD,0x06))
  val baseLocationsType1:List[Location]=List(Location.register("A"),Location.register("B"),Location.register("C"),
    Location.register("D"),Location.register("E"),Location.register("H"),Location.register("L"),
    Location.registerAddr("HL"),Location.registerAddrIndirOffset("IX", 2),Location.registerAddrIndirOffset("IY", 2))
  val baseSizesType1:List[Int]=List(1,1,1,1,1,1,1,1,3,3)
  def generateOpCodesType1(base:OpCode,bit:Int=0):List[(OpCode,Location,Int)]= {
    val opCodes=baseCodesType1.map(code=>
      if(code.numberOfCodes==1) OpCode(base.main+(code.main << bit)) else OpCode(code.main,base.main+(code.supp << bit)))
    opCodes.zip(baseLocationsType1).zip(baseSizesType1).map(entry=>(entry._1._1,entry._1._2,entry._2))
  }

  //TYPE2: registers decoded as in TYPE1 - used only for bit manipulation
  // opcodes multiplied by bits 0-7
  val baseBitsType2:List[Int]=for{b<-List.range(0,8)} yield b
  val baseSizesType2:List[Int]=List(2,2,2,2,2,2,2,2,4,4)
  def generateOpCodesType2(base:OpCode):List[(OpCode,Location,Int,Int)]= {
    val codeLocSize=baseCodesType1.zip(baseLocationsType1).zip(baseSizesType2).map(entry=>(entry._1._1,entry._1._2,entry._2))
    for{
      bit<-baseBitsType2 //bits (rows)
      (code,loc,size)<-codeLocSize //codes+locations+size (columns)
    } yield
      (if(code.numberOfCodes==1) OpCode(base.main,base.supp+code.main+(bit << 3))
      else OpCode(code.main,base.main,base.supp+code.supp+(bit << 3)),
      loc,bit,size)
  }
}

class UnknownOperationException(message : String) extends Exception(message) {}

trait Label {
  val label:String
  override def toString:String=label
}

//Building blocks for OpCode definition
trait OpCodeSourceLocation {
  val source:Location
}
trait SourceA extends OpCodeSourceLocation {override val source:Location=Location.register("A")}
trait SourceB extends OpCodeSourceLocation {override val source:Location=Location.register("B")}
trait SourceC extends OpCodeSourceLocation {override val source:Location=Location.register("C")}
trait SourceD extends OpCodeSourceLocation {override val source:Location=Location.register("D")}
trait SourceE extends OpCodeSourceLocation {override val source:Location=Location.register("E")}
trait SourceH extends OpCodeSourceLocation {override val source:Location=Location.register("H")}
trait SourceL extends OpCodeSourceLocation {override val source:Location=Location.register("L")}
trait SourceHLra extends OpCodeSourceLocation {override val source:Location=Location.registerAddr("HL")}
trait SourceBC extends OpCodeSourceLocation {override val source:Location=Location.register("BC")}
trait SourceDE extends OpCodeSourceLocation {override val source:Location=Location.register("DE")}
trait SourceHL extends OpCodeSourceLocation {override val source:Location=Location.register("HL")}
trait SourceSP extends OpCodeSourceLocation {override val source:Location=Location.register("SP")}
trait SourceIX extends OpCodeSourceLocation {override val source:Location=Location.register("IX")}
trait SourceIY extends OpCodeSourceLocation {override val source:Location=Location.register("IY")}
trait SourceIXd extends OpCodeSourceLocation {override val source:Location=Location.registerAddrIndirOffset("IX", 2)}
trait SourceIYd extends OpCodeSourceLocation {override val source:Location=Location.registerAddrIndirOffset("IY", 2)}
trait SourceN extends OpCodeSourceLocation {override val source:Location=Location.registerAddrDirOffset("PC", 1)}

trait OpCodeDestLocation {
  val destination:Location
}
trait DestinationA extends OpCodeDestLocation {override val destination:Location=Location.register("A")}
trait DestinationB extends OpCodeDestLocation {override val destination:Location=Location.register("B")}
trait DestinationC extends OpCodeDestLocation {override val destination:Location=Location.register("C")}
trait DestinationD extends OpCodeDestLocation {override val destination:Location=Location.register("D")}
trait DestinationE extends OpCodeDestLocation {override val destination:Location=Location.register("E")}
trait DestinationH extends OpCodeDestLocation {override val destination:Location=Location.register("H")}
trait DestinationL extends OpCodeDestLocation {override val destination:Location=Location.register("L")}
trait DestinationHLra extends OpCodeDestLocation {override val destination:Location=Location.registerAddr("HL")}
trait DestinationBC extends OpCodeDestLocation {override val destination:Location=Location.register("BC")}
trait DestinationDE extends OpCodeDestLocation {override val destination:Location=Location.register("DE")}
trait DestinationHL extends OpCodeDestLocation {override val destination:Location=Location.register("HL")}
trait DestinationSP extends OpCodeDestLocation {override val destination:Location=Location.register("SP")}
trait DestinationIX extends OpCodeDestLocation {override val destination:Location=Location.register("IX")}
trait DestinationIY extends OpCodeDestLocation {override val destination:Location=Location.register("IY")}
trait DestinationIXd extends OpCodeDestLocation {override val destination:Location=Location.registerAddrIndirOffset("IX", 2)}
trait DestinationIYd extends OpCodeDestLocation {override val destination:Location=Location.registerAddrIndirOffset("IY", 2)}
trait DestinationN extends OpCodeDestLocation {override val destination:Location=Location.registerAddrDirOffset("PC", 1)}
trait DestinationEmpty extends OpCodeDestLocation {override val destination:Location=Location.empty}

trait SourceDestA extends SourceA with DestinationA
trait SourceDestB extends SourceB with DestinationB
trait SourceDestC extends SourceC with DestinationC
trait SourceDestD extends SourceD with DestinationD
trait SourceDestE extends SourceE with DestinationE
trait SourceDestH extends SourceH with DestinationH
trait SourceDestL extends SourceL with DestinationL
trait SourceDestHLra extends SourceHLra with DestinationHLra
trait SourceDestBC extends SourceBC with DestinationBC
trait SourceDestDE extends SourceDE with DestinationDE
trait SourceDestHL extends SourceHL with DestinationHL
trait SourceDestSP extends SourceSP with DestinationSP
trait SourceDestIX extends SourceIX with DestinationIX
trait SourceDestIY extends SourceIY with DestinationIY
trait SourceDestIXd extends SourceIXd with DestinationIXd
trait SourceDestIYd extends SourceIYd with DestinationIYd

// copies source and destination from operand - useful for INC and DEC
trait SourceDestFromOperand extends OpCodeSourceLocation with OpCodeDestLocation with OpCodeOperandLocation {
  override val source:Location=operand
  override val destination:Location=operand
}

trait OpCodeOperandLocation {
  val operand:Location
}

trait OperandA extends OpCodeOperandLocation {override val operand:Location=Location.register("A")}
trait OperandB extends OpCodeOperandLocation {override val operand:Location=Location.register("B")}
trait OperandC extends OpCodeOperandLocation {override val operand:Location=Location.register("C")}
trait OperandD extends OpCodeOperandLocation {override val operand:Location=Location.register("D")}
trait OperandE extends OpCodeOperandLocation {override val operand:Location=Location.register("E")}
trait OperandH extends OpCodeOperandLocation {override val operand:Location=Location.register("H")}
trait OperandL extends OpCodeOperandLocation {override val operand:Location=Location.register("L")}
trait OperandHL extends OpCodeOperandLocation {override val operand:Location=Location.registerAddr("HL")}
trait OperandIXd extends OpCodeOperandLocation {override val operand:Location=Location.registerAddrIndirOffset("IX", 2)}
trait OperandIYd extends OpCodeOperandLocation {override val operand:Location=Location.registerAddrIndirOffset("IY", 2)}
trait OperandN extends OpCodeOperandLocation {override val operand:Location=Location.registerAddrDirOffset("PC", 1)}

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

trait OpCodeExchangeLocation {
  val exchange:List[ExchangeLocationBase]
}
trait ExchangeDEHL extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocation("DE","HL"))}
trait ExchangeAF1 extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocation("AF","AF1"))}
trait ExchangeAll1 extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=
  List(new ExchangeLocation("BC","BC1"),new ExchangeLocation("DE","DE1"),new ExchangeLocation("HL","HL1"))}
trait ExchangeSPHL extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect("SP","HL"))}
trait ExchangeSPIX extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect("SP","IX"))}
trait ExchangeSPIY extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect("SP","IY"))}

trait OpCodeBitManipulation {
  val operation:BitOperation
}
trait BitTest extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Test}
trait BitReset extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Reset}
trait BitSet extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Set}



object OpCodes {
  val list:List[OpCode]=
    ADD_A_reg.codes ++ ADC_A_reg.codes ++ SUB_reg.codes ++ SBC_A_reg.codes ++
    AND_reg.codes ++ XOR_reg.codes ++ OR_reg.codes ++ CP_reg.codes ++
    INC_reg.codes ++ DEC_reg.codes ++
    BIT_b_reg.codes ++ RES_b_reg.codes ++ SET_b_reg.codes ++
      List(
    //Arithmetic8b
    ADD_A_n,ADC_A_n,SUB_n,SBC_A_n,AND_n,XOR_n,OR_n,CP_n,CPL,SCF,CCF,NEG,
    //Arithmetic16b
    ADD_HL_BC,ADD_HL_DE,ADD_HL_HL,ADD_HL_SP,ADD_IX_BC,ADD_IX_DE,ADD_IX_IX,ADD_IX_SP,
    ADD_IY_BC,ADD_IY_DE,ADD_IY_IY,ADD_IY_SP,ADC_HL_BC,ADC_HL_DE,ADC_HL_HL,ADC_HL_SP,
    SBC_HL_BC,SBC_HL_DE,SBC_HL_HL,SBC_HL_SP,INC_BC,INC_DE,INC_HL_16,INC_SP,INC_IX,INC_IY,
    DEC_BC,DEC_DE,DEC_HL_16,DEC_SP,DEC_IX,DEC_IY,
    //Exchange
    EX_DE_HL, EX_AF_AF1, EXX, EX_SP_HL, EX_SP_IX, EX_SP_IY
  )

  //private def filterTo(pred:OpCode=>Boolean):List[OpCode]=list.filter(pred(_))

  private def opCodeListToMap[OpCodeAttr,Attr](getAttr:OpCodeAttr=>Attr):Map[List[OpCode],Attr]=
    list.filter(_.isInstanceOf[OpCodeAttr]).map(op=>List(op)->getAttr(op.asInstanceOf[OpCodeAttr])).toMap

  //TODO: flatten list - refactor OpCodeMap
  //NOTE: cannot use generics in vals (only defs) - these maps are used in vals in other classes
  val operandMap:Map[List[OpCode],Location]= //opCodeListToMap[OpCodeOperandLocation,LoadLocation](op=>op.operand)
  //filterTo(op=>op.isInstanceOf[OpCodeOperandLocation]).map(op=> List(op)->op.asInstanceOf[OpCodeOperandLocation].operand).toMap
    list
    .filter(_.isInstanceOf[OpCodeOperandLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeOperandLocation].operand).toMap
  val sourceMap:Map[List[OpCode],Location]= list
    .filter(_.isInstanceOf[OpCodeSourceLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeSourceLocation].source).toMap
  val destinationMap:Map[List[OpCode],Location]= list
    .filter(_.isInstanceOf[OpCodeDestLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeDestLocation].destination).toMap
  val operation8bMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeArithmetic8b])
    .map(op=> List(op)->op.asInstanceOf[OpCodeArithmetic8b].operation).toMap
  val operation16bMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeArithmetic16b])
    .map(op=> List(op)->op.asInstanceOf[OpCodeArithmetic16b].operation).toMap
  val exchangeMap:Map[List[OpCode],List[ExchangeLocationBase]]= list
    .filter(_.isInstanceOf[OpCodeExchangeLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeExchangeLocation].exchange).toMap
  val bitManipulationMap:Map[List[OpCode],BitOperation]= list
    .filter(_.isInstanceOf[OpCodeBitManipulation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeBitManipulation].operation).toMap
  val bitNumMap:Map[List[OpCode],Int]= list
    .filter(_.isInstanceOf[BitManipulationDef])
    .map(op=> List(op)->op.asInstanceOf[BitManipulationDef].bit).toMap
  val sizeMap:Map[List[OpCode],Int]= list
    .filter(_.isInstanceOf[OpCodeSize])
    .map(op=> List(op)->op.asInstanceOf[OpCodeSize].size).toMap
}

//Arithmetic 8b
// generator for ADD A,x - TBC if this is an efficient and readable way of defining opcodes
class Arithmetic8bDef(main:Int, supp:Int, val operand:Location, val size:Int, val label:String)
  extends OpCode(main,supp) with OpCodeOperandLocation with OpCodeSize with Label

//ADD
object ADD_A_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x80)).map(op=>
      new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"ADD A,${op._2.label}") with Arith8bAdd)
}
object ADD_A_n extends OpCode(0xC6) with Arith8bAdd with OperandN with Size2 with Label {override val label:String="ADD A,n"}
//ADC
object ADC_A_reg {
  val codes: List[Arithmetic8bDef] =
    OpCode.generateOpCodesType1(OpCode(0x88)).map(op=>
new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"ADC A,${op._2.label}") with Arith8bAddC)
}
object ADC_A_n extends OpCode(0xCE) with Arith8bAddC with OperandN with Size2 with Label {override val label:String="ADC A,n"}
//SUB
object SUB_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x90)).map(op=>
      new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"SUB ${op._2.label}") with Arith8bSub)
}
object SUB_n extends OpCode(0xD6) with Arith8bSub with OperandN with Size2 with Label {override val label:String="SUB n"}
//SBC
object SBC_A_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x98)).map(op=>
      new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"SBC A,${op._2.label}") with Arith8bSubC)
}
object SBC_A_n extends OpCode(0xDE) with Arith8bSubC with OperandN with Size2 with Label {override val label:String="SBC A,n"}
//AND
object AND_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xA0)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"AND ${op._2.label}") with Arith8bAnd)
}
object AND_n extends OpCode(0xE6) with Arith8bAnd with OperandN with Size2 with Label {override val label:String="AND n"}
//XOR
object XOR_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xA8)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"XOR ${op._2.label}") with Arith8bXor)
}
object XOR_n extends OpCode(0xEE) with Arith8bXor with OperandN with Size2 with Label {override val label:String="XOR n"}
//OR
object OR_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xB0)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"OR ${op._2.label}") with Arith8bOr)
}
object OR_n extends OpCode(0xF6) with Arith8bOr with OperandN with Size2 with Label {override val label:String="OR n"}
//CP
object CP_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0xB8)).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"CP ${op._2.label}") with Arith8bCp with DestinationEmpty)
}
object CP_n extends OpCode(0xFE) with Arith8bCp with OperandN with Size2 with Label {override val label:String="CP n"}
//INC
object INC_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x04),3).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"INC ${op._2.label}") with SourceDestFromOperand with Arith8bInc)
}
//DEC
object DEC_reg {
  val codes: List[Arithmetic8bDef] = OpCode.generateOpCodesType1(OpCode(0x05),3).map(op=>
    new Arithmetic8bDef(op._1.main,op._1.supp,op._2,op._3,f"DEC ${op._2.label}") with SourceDestFromOperand with Arith8bDec)
}
//
object CPL extends OpCode(0x2F) with Arith8bCpl with OperandA with Size1 with Label {override val label:String="CPL"}
object NEG extends OpCode(0xED,0x44) with Arith8bNeg with OperandA with Size2 with Label {override val label:String="NEG"}
object SCF extends OpCode(0x37) with Arith8bScf with Size1 with Label {override val label:String="SCF"}
object CCF extends OpCode(0x3F) with Arith8bCcf with Size1 with Label {override val label:String="CCF"}

//Arithmetic 16b
object ADD_HL_BC extends OpCode(0x09) with Arith16bAdd with SourceBC with DestinationHL with Size1 with Label {override val label:String="ADD HL,BC"}
object ADD_HL_DE extends OpCode(0x19) with Arith16bAdd with SourceDE with DestinationHL with Size1 with Label {override val label:String="ADD HL,DE"}
object ADD_HL_HL extends OpCode(0x29) with Arith16bAdd with SourceHL with DestinationHL with Size1 with Label {override val label:String="ADD HL,HL"}
object ADD_HL_SP extends OpCode(0x39) with Arith16bAdd with SourceSP with DestinationHL with Size1 with Label {override val label:String="ADD HL,SP"}
object ADD_IX_BC extends OpCode(0xDD,0x09) with Arith16bAdd with DestinationIX with SourceBC with Size2 with Label {override val label:String="ADD IX,BC"}
object ADD_IX_DE extends OpCode(0xDD,0x19) with Arith16bAdd with DestinationIX with SourceDE with Size2 with Label {override val label:String="ADD IX,DE"}
object ADD_IX_IX extends OpCode(0xDD,0x29) with Arith16bAdd with DestinationIX with SourceIX with Size2 with Label {override val label:String="ADD IX,IX"}
object ADD_IX_SP extends OpCode(0xDD,0x39) with Arith16bAdd with DestinationIX with SourceSP with Size2 with Label {override val label:String="ADD IX,SP"}
object ADD_IY_BC extends OpCode(0xFD,0x09) with Arith16bAdd with DestinationIY with SourceBC with Size2 with Label {override val label:String="ADD IY,BC"}
object ADD_IY_DE extends OpCode(0xFD,0x19) with Arith16bAdd with DestinationIY with SourceDE with Size2 with Label {override val label:String="ADD IY,DE"}
object ADD_IY_IY extends OpCode(0xFD,0x29) with Arith16bAdd with DestinationIY with SourceIY with Size2 with Label {override val label:String="ADD IY,IX"}
object ADD_IY_SP extends OpCode(0xFD,0x39) with Arith16bAdd with DestinationIY with SourceSP with Size2 with Label {override val label:String="ADD IY,SP"}
object ADC_HL_BC extends OpCode(0xED,0x4A) with Arith16bAddC with DestinationHL with SourceBC with Size2 with Label {override val label:String="ADC HL,BC"}
object ADC_HL_DE extends OpCode(0xED,0x5A) with Arith16bAddC with DestinationHL with SourceDE with Size2 with Label {override val label:String="ADC HL,DE"}
object ADC_HL_HL extends OpCode(0xED,0x6A) with Arith16bAddC with DestinationHL with SourceHL with Size2 with Label {override val label:String="ADC HL,HL"}
object ADC_HL_SP extends OpCode(0xED,0x7A) with Arith16bAddC with DestinationHL with SourceSP with Size2 with Label {override val label:String="ADC HL,SP"}
object SBC_HL_BC extends OpCode(0xED,0x42) with Arith16bSubC with DestinationHL with SourceBC with Size2 with Label {override val label:String="SBC HL,BC"}
object SBC_HL_DE extends OpCode(0xED,0x52) with Arith16bSubC with DestinationHL with SourceDE with Size2 with Label {override val label:String="SBC HL,DE"}
object SBC_HL_HL extends OpCode(0xED,0x62) with Arith16bSubC with DestinationHL with SourceHL with Size2 with Label {override val label:String="SBC HL,HL"}
object SBC_HL_SP extends OpCode(0xED,0x72) with Arith16bSubC with DestinationHL with SourceSP with Size2 with Label {override val label:String="SBC HL,SP"}
object INC_BC extends OpCode(0x03) with Arith16bInc with SourceDestBC with Size1 with Label {override val label:String="INC BC"}
object INC_DE extends OpCode(0x13) with Arith16bInc with SourceDestDE with Size1 with Label {override val label:String="INC DE"}
object INC_HL_16 extends OpCode(0x23) with Arith16bInc with SourceDestHL with Size1 with Label {override val label:String="INC HL"}
object INC_SP extends OpCode(0x33) with Arith16bInc with SourceDestSP with Size1 with Label {override val label:String="INC SP"}
object INC_IX extends OpCode(0xDD,0x23) with Arith16bInc with SourceDestIX with Size2 with Label {override val label:String="INC IX"}
object INC_IY extends OpCode(0xFD,0x23) with Arith16bInc with SourceDestIY with Size2 with Label {override val label:String="INC IY"}
object DEC_BC extends OpCode(0x0B) with Arith16bDec with SourceDestBC with Size1 with Label {override val label:String="DEC BC"}
object DEC_DE extends OpCode(0x1B) with Arith16bDec with SourceDestDE with Size1 with Label {override val label:String="DEC DE"}
object DEC_HL_16 extends OpCode(0x2B) with Arith16bDec with SourceDestHL with Size1 with Label {override val label:String="DEC HL"}
object DEC_SP extends OpCode(0x3B) with Arith16bDec with SourceDestSP with Size1 with Label {override val label:String="DEC SP"}
object DEC_IX extends OpCode(0xDD,0x2B) with Arith16bDec with SourceDestIX with Size2 with Label {override val label:String="DEC IX"}
object DEC_IY extends OpCode(0xFD,0x2B) with Arith16bDec with SourceDestIY with Size2 with Label {override val label:String="DEC IY"}

// Exchange
object EX_DE_HL extends OpCode(0xEB) with ExchangeDEHL with Size1 with Label {override val label:String="EX DE,HL"}
object EX_AF_AF1 extends OpCode(0x08) with ExchangeAF1 with Size1 with Label {override val label:String="EX AF,AF'"}
object EXX extends OpCode(0xD9) with ExchangeAll1 with Size1 with Label {override val label:String="EXX"}
object EX_SP_HL extends OpCode(0xE3) with ExchangeSPHL with Size1 with Label {override val label:String="EX (SP),HL"}
object EX_SP_IX extends OpCode(0xDD,0xE3) with ExchangeSPIX with Size2 with Label {override val label:String="EX (SP),IX"}
object EX_SP_IY extends OpCode(0xFD,0xE3) with ExchangeSPIY with Size2 with Label {override val label:String="EX (SP),IY"}

//Bit manipulation
// generator for BIT, RES, SET
class BitManipulationDef(main:Int, supp:Int, supp2:Int, val source:Location, val bit:Int, val size:Int, val label:String)
  extends OpCode(main,supp,supp2) with OpCodeSourceLocation with OpCodeSize with Label
//BIT
object BIT_b_reg {
  val codes: List[BitManipulationDef] = OpCode.generateOpCodesType2(OpCode(0xCB,0x40)).map(op=>
    new BitManipulationDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"BIT ${op._3},${op._2.label}") with BitTest)
}
//RESET
object RES_b_reg {
  val codes: List[BitManipulationDef] = OpCode.generateOpCodesType2(OpCode(0xCB,0x80)).map(op=>
    new BitManipulationDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"RES ${op._3},${op._2.label}") with BitReset)
}
//SET
object SET_b_reg {
  val codes: List[BitManipulationDef] = OpCode.generateOpCodesType2(OpCode(0xCB,0xC0)).map(op=>
    new BitManipulationDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,op._4,f"SET ${op._3},${op._2.label}") with BitSet)
}
