package org.kr.scala.z80.opcode

import org.kr.scala.z80.opcode.handler.{Add16b, Add8b, AddC16b, AddC8b, And8b, BitOpType, BitOperation, Ccf8b, Comp8b, Cpl8b, Dec16b, Dec8b, ExchangeLocation, ExchangeLocationBase, ExchangeLocationIndirect, InOutOpType, InOutOperation, Inc16b, Inc8b, JumpCondition, JumpOperation, JumpType, Load16BitOpType, Load8BitOpType, Neg8b, Or8b, RotShRl, RotShRla, RotShRlc, RotShRlca, RotShRr, RotShRra, RotShRrc, RotShRrca, RotShSla, RotShSra, RotShSrl, RotateDL, RotateDR, Scf8b, Sub8b, SubC16b, SubC8b, Xor8b}
import org.kr.scala.z80.system.Flag

case class OpCode(main:Int,supp:Int=OpCode.ANY,supp2:Int=OpCode.ANY) {
  /* OpCode format:
  main (supp) (d) (supp2)
  main - primary OpCode for 1-byte opcodes (1st byte of any operation)
  supp - supplementary OpCode for 2-byte opcodes (2nd byte)
  d - offset for opcodes using indexed notation (IX/IY+d) (3rd byte)
  supp2 - second supplementary OpCode for 4-byte opcodes (some IX+d/IY+d) (4th byte)
   */
  override def toString: String =
    f"OpCode(${OpCode.num2hex(main)}${if(supp!=OpCode.ANY) ","+OpCode.num2hex(supp) else ""}${if(supp2!=OpCode.ANY) ","+OpCode.num2hex(supp2) else ""})"

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

  def num2hex(num:Int):String= f"0x$num%02X"

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

  //TYPE1: registers decoded by bits 0-2 or 3-5, used for arithmetic operations
  // opcode pattern: A-L: 0x01-0x07, (HL): 0x06, (IX+d),(IY+d): (0xDD,0x06), (0xFD,0x06)
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
  // Z80 manual, pages 55-57
  val baseSizesType2:List[Int]=List(2,2,2,2,2,2,2,2,4,4)
  def generateOpCodesType2(base:OpCode):List[(OpCode,Location,Int,Int)]= {
    val codeLocSize=baseCodesType1.zip(baseLocationsType1).zip(baseSizesType2).map(entry=>(entry._1._1,entry._1._2,entry._2))
    for {
      bit<-List.range(0,8) //bits (rows in Z80 manual)
      (code,loc,size)<-codeLocSize //codes+locations+size (columns in Z80 manual)
    } yield (
      if(code.numberOfCodes==1) OpCode(base.main,base.supp+code.main+(bit << 3))
      else OpCode(code.main,base.main,base.supp+code.supp+(bit << 3)),
      loc,bit,size
    )
  }

  //TYPE3: registers decoded by bits 3-5 - used only for load
  // opcodes multiplied by list of registers
  val baseLocationsType3:List[Location]=List(Location.register("A"),Location.register("B"),Location.register("C"),
    Location.register("D"),Location.register("E"),Location.register("H"),Location.register("L"))
  val baseCodesType3:List[OpCode]=List(OpCode(0x38),OpCode(0x00),OpCode(0x08),
    OpCode(0x10),OpCode(0x18),OpCode(0x20),OpCode(0x28))
  def generateOpCodesType3(base:OpCode):List[(OpCode,Location,Location,Int)]= {
    val codeSrcLocSize=baseCodesType1.zip(baseLocationsType1).zip(baseSizesType1).map(entry=>(entry._1._1,entry._1._2,entry._2))
    val codeDestLoc=baseCodesType3.zip(baseLocationsType3)
    for {
      (destCode,destLoc)<-codeDestLoc //codes+dest locations (main registers only)
      (code,srcLoc,size)<-codeSrcLocSize //codes+source locations+size (columns in Z80 manual)
    } yield (
      if(code.numberOfCodes==1) OpCode(base.main+code.main+destCode.main)
      else OpCode(code.main,base.main+code.supp+destCode.main),
      srcLoc,destLoc,size
    )
  }

  //TYPE4: registers decoded by bits 0-2, used for selected load operations
  // opcode pattern: A-L only
  val baseCodesType4:List[OpCode]=List(OpCode(0x07),OpCode(0x00),OpCode(0x01),
    OpCode(0x02),OpCode(0x03),OpCode(0x04),OpCode(0x05))
  val baseLocationsType4:List[Location]=baseLocationsType3
  def generateOpCodesType4(base:OpCode,size:Int):List[(OpCode,Location,Int)]= {
    val opCodes=baseCodesType4.map(code=>
      if(base.numberOfCodes==1) OpCode(base.main+code.main) else OpCode(base.main,base.supp+code.main))
    opCodes.zip(baseLocationsType4).map(entry=>(entry._1,entry._2,size))
  }

  //TYPE5: registers decoded by bits 3-5, used for selected load operations
  // opcode pattern: A-L: 0x01-0x07, (HL): 0x06, (IX+d),(IY+d): (0xDD,0x06), (0xFD,0x06)
  val baseCodesType5:List[OpCode]=List(OpCode(0x38),OpCode(0x00),OpCode(0x08),
    OpCode(0x10),OpCode(0x18),OpCode(0x20),OpCode(0x28),OpCode(0x30),OpCode(0xDD,0x30),OpCode(0xFD,0x30))
  val baseLocationsType5:List[Location]=baseLocationsType1
  val baseSizesType5:List[Int]=List(2,2,2,2,2,2,2,2,4,4)
  def generateOpCodesType5(base:OpCode):List[(OpCode,Location,Int)]= {
    val opCodes=baseCodesType5.map(code=>
      if(code.numberOfCodes==1) OpCode(base.main+code.main) else OpCode(code.main,base.main+code.supp))
    opCodes.zip(baseLocationsType5).zip(baseSizesType5).map(entry=>(entry._1._1,entry._1._2,entry._2))
  }

  //TYPE6: registers decoded by bits 0-2, used for rotate and shift
  // opcode pattern: A-L: 0x01-0x07, (HL): 0x06, (IX+d),(IY+d): (0xDD,0x06), (0xFD,0x06)
  val baseCodesType6:List[OpCode]=List(OpCode(0x07),OpCode(0x00),OpCode(0x01),
    OpCode(0x02),OpCode(0x03),OpCode(0x04),OpCode(0x05),OpCode(0x06),OpCode(0xDD,0x06),OpCode(0xFD,0x06))
  val baseLocationsType6:List[Location]=baseLocationsType1
  val baseSizesType6:List[Int]=baseSizesType5
  def generateOpCodesType6(base:OpCode):List[(OpCode,Location,Int)]= {
    val opCodes=baseCodesType6.map(code=>
      if(code.numberOfCodes==1) OpCode(base.main,base.supp+code.main) else OpCode(code.main,base.main,base.supp+code.supp))
    opCodes.zip(baseLocationsType6).zip(baseSizesType6).map(entry=>(entry._1._1,entry._1._2,entry._2))
  }

  //TYPE6: decoding jump conditions by bits 3-5
  val baseCodeOffsetType7:List[Int]=List.range(0,8).map(_ << 3)
  val baseJumpConditions:List[JumpCondition]=List(
    JumpCondition.flag(Flag.Z,value=false),
    JumpCondition.flag(Flag.Z,value=true),
    JumpCondition.flag(Flag.C,value=false),
    JumpCondition.flag(Flag.C,value=true),
    JumpCondition.flag(Flag.P,value=false),
    JumpCondition.flag(Flag.P,value=true),
    JumpCondition.flag(Flag.S,value=false),
    JumpCondition.flag(Flag.S,value=true))
  def generateOpCodesType7(base:OpCode,size:Int):List[(OpCode,JumpCondition,Int)]= {
    val opCodes=baseCodeOffsetType7.map(offset=>OpCode(base.main+offset))
    opCodes.zip(baseJumpConditions).map(entry=>(entry._1,entry._2,size))
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
trait SourceNw extends OpCodeSourceLocation {override val source:Location=Location.registerAddrDirOffset("PC", 1, isWord = true)}
trait SourceStack extends OpCodeSourceLocation {override val source:Location=Location.registerAddr("SP",isWord = true)}
trait Source00h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0000)}
trait Source08h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0008)}
trait Source10h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0010)}
trait Source18h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0018)}
trait Source20h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0020)}
trait Source28h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0028)}
trait Source30h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0030)}
trait Source38h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0038)}

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

trait OpCodeRotateDigit extends SourceA with OperandHL {
  val operation:ArithmeticOperation
}
trait RotateDigitLeft extends OpCodeRotateDigit {override val operation:ArithmeticOperation=RotateDL}
trait RotateDigitRight extends OpCodeRotateDigit {override val operation:ArithmeticOperation=RotateDR}

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

trait OpCodeLoad8Bit extends OpCodeSourceLocation with OpCodeDestLocation {
  val operation:Load8BitOpType
}
trait Load8BitOp extends OpCodeLoad8Bit {override val operation:Load8BitOpType=Load8BitOpType.Load}

trait OpCodeLoad16Bit extends OpCodeSourceLocation with OpCodeDestLocation {
  val operation:Load16BitOpType
}
trait Load16BitOp extends OpCodeLoad16Bit {override val operation:Load16BitOpType=Load16BitOpType.Load}

trait OpStackChange  {
  val stackChange:Int
}
trait PushStack extends OpStackChange {override val stackChange:Int= -2}
trait PopStack extends OpStackChange {override val stackChange:Int= 2}

trait OpCodeRotateShift {
  val operation:ArithmeticOperation
}
trait RotateShiftRlc extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRlc}
trait RotateShiftRlca extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRlca}
trait RotateShiftRrc extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRrc}
trait RotateShiftRrca extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRrca}
trait RotateShiftRl extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRl}
trait RotateShiftRla extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRla}
trait RotateShiftRr extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRr}
trait RotateShiftRra extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRra}
trait RotateShiftSla extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShSla}
trait RotateShiftSra extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShSra}
trait RotateShiftSrl extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShSrl}

trait OpCodeJump {
  val operation:JumpOperation
}
trait JumpOper extends OpCodeJump {override val operation:JumpOperation=JumpType.Jump}
trait JumpRelativeOper extends OpCodeJump {override val operation:JumpOperation=JumpType.JumpR}
trait JumpRelativeDecOper extends OpCodeJump {override val operation:JumpOperation=JumpType.DJumpR}
trait DecrJumpRelativeOper extends OpCodeJump {override val operation:JumpOperation=JumpType.DJumpR}
trait CallOper extends OpCodeJump {override val operation:JumpOperation=JumpType.Call}
trait ReturnOper extends OpCodeJump {override val operation:JumpOperation=JumpType.Return}

trait OpCodeJumpCondition {
  val condition:JumpCondition
}
trait JumpUnconditional extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.empty}
trait JumpC extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.C,value=true)}
trait JumpNC extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.C,value=false)}
trait JumpZ extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.Z,value=true)}
trait JumpNZ extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.Z,value=false)}
trait JumpB0 extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.register("B",0)}

trait OpCodeInOut {
  val operation:InOutOperation
}
trait InOper extends OpCodeInOut {override val operation:InOutOperation=InOutOpType.In}
trait OutOper extends OpCodeInOut {override val operation:InOutOperation=InOutOpType.Out}

//TODO: all pointer to opcode handler in definition of every opcode - this will also simplify Z80System.handle method
object OpCodes {
  val list:List[OpCode]=
    ADD_A_reg.codes ++ ADC_A_reg.codes ++ SUB_reg.codes ++ SBC_A_reg.codes ++
    AND_reg.codes ++ XOR_reg.codes ++ OR_reg.codes ++ CP_reg.codes ++
    INC_reg.codes ++ DEC_reg.codes ++
    BIT_b_reg.codes ++ RES_b_reg.codes ++ SET_b_reg.codes ++
    LD_reg_all.codes ++ LD_HL_reg.codes ++ LD_IXd_reg.codes ++ LD_IYd_reg.codes ++ LD_all_n.codes ++
    RLC_all.codes ++ RRC_all.codes ++ RL_all.codes ++ RR_all.codes ++ SLA_all.codes ++ SRA_all.codes ++ SRL_all.codes ++
    JP_cond.codes ++ CALL_cond.codes ++ RET_cond.codes ++ RST_all.codes ++
      List(
    //Arithmetic8b
    ADD_A_n,ADC_A_n,SUB_n,SBC_A_n,AND_n,XOR_n,OR_n,CP_n,CPL,SCF,CCF,NEG,
    //Rotate digit
    RLD,RRD,
    //Rotate shift
    RLCA,RRCA,RLA,RRA,
    //Arithmetic16b
    ADD_HL_BC,ADD_HL_DE,ADD_HL_HL,ADD_HL_SP,ADD_IX_BC,ADD_IX_DE,ADD_IX_IX,ADD_IX_SP,
    ADD_IY_BC,ADD_IY_DE,ADD_IY_IY,ADD_IY_SP,ADC_HL_BC,ADC_HL_DE,ADC_HL_HL,ADC_HL_SP,
    SBC_HL_BC,SBC_HL_DE,SBC_HL_HL,SBC_HL_SP,INC_BC,INC_DE,INC_HL_16,INC_SP,INC_IX,INC_IY,
    DEC_BC,DEC_DE,DEC_HL_16,DEC_SP,DEC_IX,DEC_IY,
    //Exchange
    EX_DE_HL, EX_AF_AF1, EXX, EX_SP_HL, EX_SP_IX, EX_SP_IY,
    //Load 8 bit
    LD_A_I,LD_A_R,LD_I_A,LD_R_A,LD_A_BC,LD_A_DE,LD_BC_A,LD_DE_A,LD_A_nn,LD_nn_A,
    //Load 16 bit
    PUSH_AF,PUSH_BC,PUSH_DE,PUSH_HL,PUSH_IX,PUSH_IY,POP_AF,POP_BC,POP_DE,POP_HL,POP_IX,POP_IY,
    LD_SP_HL,LD_SP_IX,LD_SP_IY,LD_nn_BC,LD_nn_DE,LD_nn_HL,LD_nn_SP,LD_nn_IX,LD_nn_IY,
    LD_BC_nn,LD_DE_nn,LD_HL_nn,LD_SP_nn,LD_IX_nn,LD_IY_nn,LD_BC_i,LD_DE_i,LD_HL_i,LD_SP_i,LD_IX_i,LD_IY_i,
    //Jump
    JP_nn,JP_HL,JP_IX,JP_IY,JR_n,JR_NZ_n,JR_Z_n,JR_NC_n,JR_C_n,CALL_nn,DJNZ,RET,RETI,
    //IO
    IN_A_n,OUT_n_A,OUT_C_A,OUT_C_B,OUT_C_D,OUT_C_E,OUT_C_H,OUT_C_L
  )

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
  val rotateShiftMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeRotateShift])
    .map(op=> List(op)->op.asInstanceOf[OpCodeRotateShift].operation).toMap
  val rotateDigitMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeRotateDigit])
    .map(op=> List(op)->op.asInstanceOf[OpCodeRotateDigit].operation).toMap
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
  val load8bMap:Map[List[OpCode],Load8BitOpType]= list
    .filter(_.isInstanceOf[Load8BitOp])
    .map(op=> List(op)->op.asInstanceOf[Load8BitOp].operation).toMap
  val load16bMap:Map[List[OpCode],Load16BitOpType]= list
    .filter(_.isInstanceOf[Load16BitOp])
    .map(op=> List(op)->op.asInstanceOf[Load16BitOp].operation).toMap
  val stackChangeMap:Map[List[OpCode],Int]= list
    .filter(_.isInstanceOf[OpStackChange])
    .map(op=> List(op)->op.asInstanceOf[OpStackChange].stackChange).toMap
  val jumpConditionMap:Map[List[OpCode],JumpCondition]= list
    .filter(_.isInstanceOf[OpCodeJumpCondition])
    .map(op=> List(op)->op.asInstanceOf[OpCodeJumpCondition].condition).toMap
  val jumpOperationMap:Map[List[OpCode],JumpOperation]= list
    .filter(_.isInstanceOf[OpCodeJump])
    .map(op=> List(op)->op.asInstanceOf[OpCodeJump].operation).toMap
  val inOutOperationMap:Map[List[OpCode],InOutOperation]= list
    .filter(_.isInstanceOf[OpCodeInOut])
    .map(op=> List(op)->op.asInstanceOf[OpCodeInOut].operation).toMap

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
object CPL extends OpCode(0x2F) with Arith8bCpl with OperandA with Size1 with Label {override val label:String="CPL"}
object NEG extends OpCode(0xED,0x44) with Arith8bNeg with OperandA with Size2 with Label {override val label:String="NEG"}
object SCF extends OpCode(0x37) with Arith8bScf with Size1 with Label {override val label:String="SCF"}
object CCF extends OpCode(0x3F) with Arith8bCcf with Size1 with Label {override val label:String="CCF"}
// Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
object RLD extends OpCode(0xED,0x6F) with RotateDigitLeft with Size2 with Label {override val label:String="RLD"}
object RRD extends OpCode(0xED,0x67) with RotateDigitRight with Size2 with Label {override val label:String="RRD"}

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

//Load 8 bit
// Z80 manual page 42
// generator for LD (8 bit)
class Load8bitDef(main:Int, supp:Int, val source:Location, val destination:Location, val size:Int, val label:String)
  extends OpCode(main,supp)  with Load8BitOp with OpCodeSize with Label
//LD A-L,A-L/(HL)/(IX+d)/(IY+d)
object LD_reg_all {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType3(OpCode(0x40)).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,op._3,op._4,f"LD ${op._3},${op._2.label}"))
}
object LD_HL_reg {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType4(OpCode(0x70),1).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,Location.registerAddr("HL"),op._3,f"LD (HL),${op._2.label}"))
}
object LD_IXd_reg {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType4(OpCode(0xDD,0x70),3).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,Location.registerAddrIndirOffset("IX",2),op._3,f"LD (IX+d),${op._2.label}"))
}
object LD_IYd_reg {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType4(OpCode(0xFD,0x70),3).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,op._2,Location.registerAddrIndirOffset("IY",2),op._3,f"LD (IY+d),${op._2.label}"))
}
object LD_all_n {
  val codes: List[Load8bitDef] = OpCode.generateOpCodesType5(OpCode(0x06)).map(op=>
    new Load8bitDef(op._1.main,op._1.supp,Location.registerAddrDirOffset("PC",op._3-1),op._2,op._3,f"LD ${op._2.label},n"))
}
object LD_A_I extends Load8bitDef(0xED,0x57,Location.register("I"),Location.register("A"),2,"LD A,I")
object LD_A_R extends Load8bitDef(0xED,0x5F,Location.register("R"),Location.register("A"),2,"LD A,R")
object LD_I_A extends Load8bitDef(0xED,0x47,Location.register("A"),Location.register("I"),2,"LD I,A")
object LD_R_A extends Load8bitDef(0xED,0x4F,Location.register("A"),Location.register("R"),2,"LD R,A")
object LD_A_BC extends Load8bitDef(0x0A,OpCode.ANY,Location.registerAddr("BC"),Location.register("A"),1,"LD A,(BC)")
object LD_A_DE extends Load8bitDef(0x1A,OpCode.ANY,Location.registerAddr("DE"),Location.register("A"),1,"LD A,(DE)")
object LD_BC_A extends Load8bitDef(0x02,OpCode.ANY,Location.register("A"),Location.registerAddr("BC"),1,"LD (BC),A")
object LD_DE_A extends Load8bitDef(0x12,OpCode.ANY,Location.register("A"),Location.registerAddr("DE"),1,"LD (DE),A")
object LD_A_nn extends Load8bitDef(0x3A,OpCode.ANY,Location.indirAddress(1),Location.register("A"),3,"LD A,(nn)")
object LD_nn_A extends Load8bitDef(0x32,OpCode.ANY,Location.register("A"),Location.indirAddress(1),3,"LD (nn),A")

//Load 16 bit
// Z80 manual page 45 (NOTE: PUSH qq are X5, not X6!)
// generator for LD (8 bit)
class Load16bitDef(main:Int, supp:Int, val source:Location, val destination:Location, val size:Int, val label:String)
  extends OpCode(main,supp) with Load16BitOp with OpCodeSize with Label
object PUSH_AF extends Load16bitDef(0xF5,OpCode.ANY,Location.register("AF"),Location.registerAddrDirOffset("SP", -2, isWord = true),1,"PUSH AF") with PushStack
object PUSH_BC extends Load16bitDef(0xC5,OpCode.ANY,Location.register("BC"),Location.registerAddrDirOffset("SP", -2, isWord = true),1,"PUSH BC") with PushStack
object PUSH_DE extends Load16bitDef(0xD5,OpCode.ANY,Location.register("DE"),Location.registerAddrDirOffset("SP", -2, isWord = true),1,"PUSH DE") with PushStack
object PUSH_HL extends Load16bitDef(0xE5,OpCode.ANY,Location.register("HL"),Location.registerAddrDirOffset("SP", -2, isWord = true),1,"PUSH HL") with PushStack
object PUSH_IX extends Load16bitDef(0xDD,0xE5,Location.register("IX"),Location.registerAddrDirOffset("SP", -2, isWord = true),2,"PUSH IX") with PushStack
object PUSH_IY extends Load16bitDef(0xFD,0xE5,Location.register("IY"),Location.registerAddrDirOffset("SP", -2, isWord = true),2,"PUSH IY") with PushStack
object POP_AF extends Load16bitDef(0xF1,OpCode.ANY,Location.registerAddr("SP",isWord = true),Location.register("AF"),1,"POP AF") with PopStack
object POP_BC extends Load16bitDef(0xC1,OpCode.ANY,Location.registerAddr("SP",isWord = true),Location.register("BC"),1,"POP BC") with PopStack
object POP_DE extends Load16bitDef(0xD1,OpCode.ANY,Location.registerAddr("SP",isWord = true),Location.register("DE"),1,"POP DE") with PopStack
object POP_HL extends Load16bitDef(0xE1,OpCode.ANY,Location.registerAddr("SP",isWord = true),Location.register("HL"),1,"POP HL") with PopStack
object POP_IX extends Load16bitDef(0xDD,0xE1,Location.registerAddr("SP",isWord = true),Location.register("IX"),2,"POP IX") with PopStack
object POP_IY extends Load16bitDef(0xFD,0xE1,Location.registerAddr("SP",isWord = true),Location.register("IY"),2,"POP IY") with PopStack
object LD_SP_HL extends Load16bitDef(0xF9,OpCode.ANY,Location.register("HL"),Location.register("SP"),1,"LD SP,HL")
object LD_SP_IX extends Load16bitDef(0xDD,0xF9,Location.register("IX"),Location.register("SP"),2,"LD SP,IX")
object LD_SP_IY extends Load16bitDef(0xFD,0xF9,Location.register("IY"),Location.register("SP"),2,"LD SP,IY")
object LD_nn_BC extends Load16bitDef(0xED,0x43,Location.register("BC"),Location.indirAddress(2,isWord = true),4,"LD (nn),BC")
object LD_nn_DE extends Load16bitDef(0xED,0x53,Location.register("DE"),Location.indirAddress(2,isWord = true),4,"LD (nn),DE")
object LD_nn_HL extends Load16bitDef(0x22,OpCode.ANY,Location.register("HL"),Location.indirAddress(1,isWord = true),3,"LD (nn),HL")
object LD_nn_SP extends Load16bitDef(0xED,0x73,Location.register("SP"),Location.indirAddress(2,isWord = true),4,"LD (nn),SP")
object LD_nn_IX extends Load16bitDef(0xDD,0x22,Location.register("IX"),Location.indirAddress(2,isWord = true),4,"LD (nn),IX")
object LD_nn_IY extends Load16bitDef(0xFD,0x22,Location.register("IY"),Location.indirAddress(2,isWord = true),4,"LD (nn),IY")
object LD_BC_nn extends Load16bitDef(0xED,0x4B,Location.indirAddress(2,isWord = true),Location.register("BC"),4,"LD BC,(nn)")
object LD_DE_nn extends Load16bitDef(0xED,0x5B,Location.indirAddress(2,isWord = true),Location.register("DE"),4,"LD DE,(nn)")
object LD_HL_nn extends Load16bitDef(0x2A,OpCode.ANY,Location.indirAddress(1,isWord = true),Location.register("HL"),3,"LD HL,(nn)")
object LD_SP_nn extends Load16bitDef(0xED,0x7B,Location.indirAddress(2,isWord = true),Location.register("SP"),4,"LD SP,(nn)")
object LD_IX_nn extends Load16bitDef(0xDD,0x2A,Location.indirAddress(2,isWord = true),Location.register("IX"),4,"LD IX,(nn)")
object LD_IY_nn extends Load16bitDef(0xFD,0x2A,Location.indirAddress(2,isWord = true),Location.register("IY"),4,"LD IY,(nn)")
object LD_BC_i extends Load16bitDef(0x01,OpCode.ANY,Location.registerAddrDirOffset("PC", 1, isWord = true),Location.register("BC"),3,"LD BC,nn")
object LD_DE_i extends Load16bitDef(0x11,OpCode.ANY,Location.registerAddrDirOffset("PC", 1, isWord = true),Location.register("DE"),3,"LD DE,nn")
object LD_HL_i extends Load16bitDef(0x21,OpCode.ANY,Location.registerAddrDirOffset("PC", 1, isWord = true),Location.register("HL"),3,"LD HL,nn")
object LD_SP_i extends Load16bitDef(0x31,OpCode.ANY,Location.registerAddrDirOffset("PC", 1, isWord = true),Location.register("SP"),3,"LD SP,nn")
object LD_IX_i extends Load16bitDef(0xDD,0x21,Location.registerAddrDirOffset("PC", 2, isWord = true),Location.register("IX"),4,"LD IX,nn")
object LD_IY_i extends Load16bitDef(0xFD,0x21,Location.registerAddrDirOffset("PC", 2, isWord = true),Location.register("IY"),4,"LD IY,nn")

//Rotate and shift
// Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
// generator for rotate and shift
class RotateShiftDef(main:Int, supp:Int, supp2:Int, val source:Location, val size:Int, val label:String)
  extends OpCode(main,supp,supp2) with OpCodeSourceLocation with OpCodeSize with Label
object RLC_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x00)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,f"RLC ${op._2.label}") with RotateShiftRlc)
}
object RRC_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x08)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,f"RRC ${op._2.label}") with RotateShiftRrc)
}
object RL_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x10)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,f"RL ${op._2.label}") with RotateShiftRl)
}
object RR_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x18)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,f"RR ${op._2.label}") with RotateShiftRr)
}
object SLA_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x20)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,f"SLA ${op._2.label}") with RotateShiftSla)
}
object SRA_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x28)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,f"SRA ${op._2.label}") with RotateShiftSra)
}
object SRL_all {
  val codes: List[RotateShiftDef] = OpCode.generateOpCodesType6(OpCode(0xCB,0x38)).map(op=>
    new RotateShiftDef(op._1.main,op._1.supp,op._1.supp2,op._2,op._3,f"SRL ${op._2.label}") with RotateShiftSrl)
}
object RLCA extends OpCode(0x07) with RotateShiftRlca with SourceA with Size1 with Label {override val label:String="RLCA"}
object RRCA extends OpCode(0x0F) with RotateShiftRrca with SourceA with Size1 with Label {override val label:String="RRCA"}
object RLA extends OpCode(0x17) with RotateShiftRla with SourceA with Size1 with Label {override val label:String="RLA"}
object RRA extends OpCode(0x1F) with RotateShiftRra with SourceA with Size1 with Label {override val label:String="RRA"}

//Jump
//Z80 manual p.59
// generator for jump group
class JumpDef(main:Int, val condition:JumpCondition, val size:Int, val label:String)
  extends OpCode(main) with OpCodeJumpCondition with OpCodeSize with Label
object JP_cond {
  val codes:List[JumpDef] = OpCode.generateOpCodesType7(OpCode(0xC2),3).map(op=>
    new JumpDef(op._1.main,op._2,op._3,f"JP ${if(op._2.value==0) "N" else ""}${op._2.flag.symbol},nn") with JumpOper with SourceNw )
}
object CALL_cond {
  val codes:List[JumpDef] = OpCode.generateOpCodesType7(OpCode(0xC4),3).map(op=>
    new JumpDef(op._1.main,op._2,op._3,f"CALL ${if(op._2.value==0) "N" else ""}${op._2.flag.symbol},nn") with CallOper with SourceNw)
}
object RET_cond {
  val codes:List[JumpDef] = OpCode.generateOpCodesType7(OpCode(0xC0),1).map(op=>
    new JumpDef(op._1.main,op._2,op._3,f"RET ${if(op._2.value==0) "N" else ""}${op._2.flag.symbol}") with ReturnOper with SourceStack)
}
object JP_nn extends OpCode(0xC3) with JumpUnconditional with JumpOper with SourceNw with Size3 with Label {override val label:String="JP nn"}
object JR_n extends OpCode(0x18) with JumpUnconditional with JumpRelativeOper with SourceN with Size2 with Label {override val label:String="JR n"}
object JR_NZ_n extends OpCode(0x20) with JumpNZ with JumpRelativeOper with SourceN with Size2 with Label {override val label:String="JR NZ,n"}
object JR_Z_n extends OpCode(0x28) with JumpZ with JumpRelativeOper with SourceN with Size2 with Label {override val label:String="JR Z,n"}
object JR_NC_n extends OpCode(0x30) with JumpNC with JumpRelativeOper with SourceN with Size2 with Label {override val label:String="JR NC,n"}
object JR_C_n extends OpCode(0x38) with JumpC with JumpRelativeOper with SourceN with Size2 with Label {override val label:String="JR C,n"}
object JP_HL extends OpCode(0xE9) with JumpUnconditional with JumpOper with SourceHL with Size1 with Label {override val label:String="JP (HL)"}
object JP_IX extends OpCode(0xDD,0xE9) with JumpUnconditional with JumpOper with SourceIX with Size2 with Label {override val label:String="JP (IX)"}
object JP_IY extends OpCode(0xFD,0xE9) with JumpUnconditional with JumpOper with SourceIY with Size2 with Label {override val label:String="JP (IY)"}
object CALL_nn extends OpCode(0xCD) with JumpUnconditional with CallOper with SourceNw with Size3 with Label {override val label:String="CALL nn"}
object DJNZ extends OpCode(0x10) with JumpB0 with DecrJumpRelativeOper with SourceN with Size2 with Label {override val label:String="DJNZ"}
object RET extends OpCode(0xC9) with JumpUnconditional with ReturnOper with SourceStack with Size1 with Label {override val label:String="RET"}
object RETI extends OpCode(0xED,0x4D) with JumpUnconditional with ReturnOper with SourceStack with Size2 with Label {override val label:String="RETI"}

object RST_all {
  val codes:List[JumpDef]={
    for(
      locCode<-List.range(0,8).map(_ << 3) // list of actual addresses to call
    ) yield {
      new JumpDef(0xC7+locCode,JumpCondition.empty,1,f"RST ${OpCode.num2hex(locCode)}") with CallOper with OpCodeSourceLocation {
        override val source: Location = Location.immediate(locCode)}
    }
  }
}

//IN/OUT
object IN_A_n extends OpCode(0xDB) with DestinationN with SourceA with InOper with Size2 with Label {override val label:String="IN A,(n)"}
object OUT_n_A extends OpCode(0xD3) with DestinationN with SourceA with OutOper with Size2 with Label {override val label:String="OUT (n),A"}
object OUT_C_A extends OpCode(0xED,0x79) with DestinationC with SourceA with OutOper with Size2 with Label {override val label:String="OUT (C),A"}
object OUT_C_B extends OpCode(0xED,0x41) with DestinationC with SourceB with OutOper with Size2 with Label {override val label:String="OUT (C),B"}
object OUT_C_C extends OpCode(0xED,0x49) with DestinationC with SourceC with OutOper with Size2 with Label {override val label:String="OUT (C),C"}
object OUT_C_D extends OpCode(0xED,0x51) with DestinationC with SourceD with OutOper with Size2 with Label {override val label:String="OUT (C),D"}
object OUT_C_E extends OpCode(0xED,0x59) with DestinationC with SourceE with OutOper with Size2 with Label {override val label:String="OUT (C),E"}
object OUT_C_H extends OpCode(0xED,0x61) with DestinationC with SourceH with OutOper with Size2 with Label {override val label:String="OUT (C),H"}
object OUT_C_L extends OpCode(0xED,0x69) with DestinationC with SourceL with OutOper with Size2 with Label {override val label:String="OUT (C),L"}