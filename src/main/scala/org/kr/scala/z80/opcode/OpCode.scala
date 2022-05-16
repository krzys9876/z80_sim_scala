package org.kr.scala.z80.opcode

import org.kr.scala.z80.opcode.handler.JumpCondition
import org.kr.scala.z80.system.{Flag, Regs}

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

  def matches(code:OpCode):Boolean=
    main==code.main &&
    (supp==code.supp || supp==OpCode.ANY || code.supp==OpCode.ANY) &&
    (supp2==code.supp2 || supp2==OpCode.ANY || code.supp2==OpCode.ANY)

  def getCode(codeNo:Int):Int=
    codeNo match {
      case 1 => main
      case 2 => supp
      case 3 => supp2
    }

  def mainOnly:OpCode=OpCode(main)
  def mainSupp:OpCode=OpCode(main,supp)
}

object OpCode {
  val ANY:Int = Int.MinValue
  val registerMap:Map[Int,Location]=Map(
    7->RegisterLocation(Regs.A),
    0->RegisterLocation(Regs.B),
    1->RegisterLocation(Regs.C),
    2->RegisterLocation(Regs.D),
    3->RegisterLocation(Regs.E),
    4->RegisterLocation(Regs.H),
    5->RegisterLocation(Regs.L))

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

  //TYPE1: registers decoded by bits 0-2 or 3-5, used for arithmetic operations
  // opcode pattern: A-L: 0x01-0x07, (HL): 0x06, (IX+d),(IY+d): (0xDD,0x06), (0xFD,0x06)
  val baseCodesType1:List[OpCode]=List(OpCode(0x07),OpCode(0x00),OpCode(0x01),
    OpCode(0x02),OpCode(0x03),OpCode(0x04),OpCode(0x05),
    OpCode(0x06),OpCode(0xDD,0x06),OpCode(0xFD,0x06))
  val baseLocationsType1:List[Location]=List(RegisterLocation(Regs.A),RegisterLocation(Regs.B),RegisterLocation(Regs.C),
    RegisterLocation(Regs.D),RegisterLocation(Regs.E),RegisterLocation(Regs.H),RegisterLocation(Regs.L),
    RegisterAddrLocation(Regs.HL),RegisterAddrIndirOffsetLocation(Regs.IX, 2),RegisterAddrIndirOffsetLocation(Regs.IY, 2))
  val baseSizesType1:List[Int]=List(1,1,1,1,1,1,1,1,3,3)
  val baseTCyclesType1:List[Int]=List(4,4,4,4,4,4,4,7,19,19)
  def generateOpCodesType1(base:OpCode,bit:Int=0):List[(OpCode,Location,Int,Int)]= {
    val opCodes=baseCodesType1.map(code=>
      if(code.numberOfCodes==1) OpCode(base.main+(code.main << bit)) else OpCode(code.main,base.main+(code.supp << bit)))
    opCodes.zip(baseLocationsType1).zip(baseSizesType1).zip(baseTCyclesType1)
      .map({case(((code,loc),size),cycles)=>(code,loc,size,cycles)})
  }

  //TYPE2: registers decoded as in TYPE1 - used only for bit manipulation
  // opcodes multiplied by bits 0-7
  // Z80 manual, pages 55-57
  val baseSizesType2:List[Int]=List(2,2,2,2,2,2,2,2,4,4)
  val baseTCyclesType21:List[Int]=List(8,8,8,8,8,8,8,12,20,20) // bit test
  val baseTCyclesType22:List[Int]=List(8,8,8,8,8,8,8,15,23,23) // bit set or reset
  def generateOpCodesType2(base:OpCode,setOrRes:Boolean):List[(OpCode,Location,Int,Int,Int)]= {
    val cyclesList=if(setOrRes) baseTCyclesType22 else baseTCyclesType21
    val codeLocSize=baseCodesType1.zip(baseLocationsType1).zip(baseSizesType2).zip(cyclesList)
      .map({case(((code,loc),size),cycles)=>(code,loc,size,cycles)})
    for {
      bit<-List.range(0,8) //bits (rows in Z80 manual)
      (code,loc,size,cycles)<-codeLocSize //codes+locations+size (columns in Z80 manual)
    } yield (
      if(code.numberOfCodes==1) OpCode(base.main,base.supp+code.main+(bit << 3))
      else OpCode(code.main,base.main,base.supp+code.supp+(bit << 3)),
      loc,bit,size,cycles
    )
  }

  //TYPE3: registers decoded by bits 3-5 - used only for load
  // opcodes multiplied by list of registers
  val baseLocationsType3:List[Location]=List(RegisterLocation(Regs.A),RegisterLocation(Regs.B),RegisterLocation(Regs.C),
    RegisterLocation(Regs.D),RegisterLocation(Regs.E),RegisterLocation(Regs.H),RegisterLocation(Regs.L))
  val baseCodesType3:List[OpCode]=List(OpCode(0x38),OpCode(0x00),OpCode(0x08),
    OpCode(0x10),OpCode(0x18),OpCode(0x20),OpCode(0x28))
  def generateOpCodesType3(base:OpCode):List[(OpCode,Location,Location,Int,Int)]= {
    val codeSrcLocSize=baseCodesType1.zip(baseLocationsType1).zip(baseSizesType1).zip(baseTCyclesType1)
      .map({case(((code,loc),size),cycles)=>(code,loc,size,cycles)})
    val codeDestLoc=baseCodesType3.zip(baseLocationsType3)
    for {
      (destCode,destLoc)<-codeDestLoc //codes+dest locations (main registers only)
      (code,srcLoc,size,cycles)<-codeSrcLocSize //codes+source locations+size (columns in Z80 manual)
    } yield (
      if(code.numberOfCodes==1) OpCode(base.main+code.main+destCode.main)
      else OpCode(code.main,base.main+code.supp+destCode.main),
      srcLoc,destLoc,size,cycles
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
    opCodes.zip(baseLocationsType4).map({case(code,loc)=>(code,loc,size)})
  }

  //TYPE5: registers decoded by bits 3-5, used for selected load operations
  // opcode pattern: A-L: 0x01-0x07, (HL): 0x06, (IX+d),(IY+d): (0xDD,0x06), (0xFD,0x06)
  val baseCodesType5:List[OpCode]=List(OpCode(0x38),OpCode(0x00),OpCode(0x08),
    OpCode(0x10),OpCode(0x18),OpCode(0x20),OpCode(0x28),OpCode(0x30),OpCode(0xDD,0x30),OpCode(0xFD,0x30))
  val baseLocationsType5:List[Location]=baseLocationsType1
  val baseSizesType5:List[Int]=List(2,2,2,2,2,2,2,2,4,4)
  val baseTCyclesType5:List[Int]=List(7,7,7,7,7,7,7,10,19,19)
  def generateOpCodesType5(base:OpCode):List[(OpCode,Location,Int,Int)]= {
    val opCodes=baseCodesType5.map(code=>
      if(code.numberOfCodes==1) OpCode(base.main+code.main) else OpCode(code.main,base.main+code.supp))
    opCodes.zip(baseLocationsType5).zip(baseSizesType5).zip(baseTCyclesType5)
      .map({case(((code,loc),size),cycles)=>(code,loc,size,cycles)})
  }

  //TYPE6: registers decoded by bits 0-2, used for rotate and shift
  // opcode pattern: A-L: 0x01-0x07, (HL): 0x06, (IX+d),(IY+d): (0xDD,0x06), (0xFD,0x06)
  val baseCodesType6:List[OpCode]=List(OpCode(0x07),OpCode(0x00),OpCode(0x01),
    OpCode(0x02),OpCode(0x03),OpCode(0x04),OpCode(0x05),OpCode(0x06),OpCode(0xDD,0x06),OpCode(0xFD,0x06))
  val baseLocationsType6:List[Location]=baseLocationsType1
  val baseSizesType6:List[Int]=baseSizesType5
  val baseTCyclesType6:List[Int]=List(8,8,8,8,8,8,8,15,23,23)
  def generateOpCodesType6(base:OpCode):List[(OpCode,Location,Int,Int)]= {
    val opCodes=baseCodesType6.map(code=>
      if(code.numberOfCodes==1) OpCode(base.main,base.supp+code.main) else OpCode(code.main,base.main,base.supp+code.supp))
    opCodes.zip(baseLocationsType6).zip(baseSizesType6).zip(baseTCyclesType6)
      .map({case(((code,loc),size),cyles)=>(code,loc,size,cyles)})
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
    opCodes.zip(baseJumpConditions).map({case(code,loc)=>(code,loc,size)})
  }
}

class UnknownOperationException(message : String) extends Exception(message) {}

