package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

sealed abstract class BitOperation(val name:String)

object BitOpType {
  case object Test extends BitOperation("TEST")
  case object Reset extends BitOperation("RESET")
  case object Set extends BitOperation("SET")
  case object None extends BitOperation("NONE")
}

object BitManipulation extends OperationSpec with OpCodeHandler {
  //Z80 manual p.55

  val locationListMap: Map[List[OpCode],LoadLocation] = Map(
    //register
    OpCode.generateListByBit(OpCode(0xCB,0x47),2,3)->LoadLocation.register("A"),
    OpCode.generateListByBit(OpCode(0xCB,0x40),2,3)->LoadLocation.register("B"),
    OpCode.generateListByBit(OpCode(0xCB,0x41),2,3)->LoadLocation.register("C"),
    OpCode.generateListByBit(OpCode(0xCB,0x42),2,3)->LoadLocation.register("D"),
    OpCode.generateListByBit(OpCode(0xCB,0x43),2,3)->LoadLocation.register("E"),
    OpCode.generateListByBit(OpCode(0xCB,0x44),2,3)->LoadLocation.register("H"),
    OpCode.generateListByBit(OpCode(0xCB,0x45),2,3)->LoadLocation.register("L"),
    OpCode.generateListByBit(OpCode(0xCB,0x87),2,3)->LoadLocation.register("A"),
    OpCode.generateListByBit(OpCode(0xCB,0x80),2,3)->LoadLocation.register("B"),
    OpCode.generateListByBit(OpCode(0xCB,0x81),2,3)->LoadLocation.register("C"),
    OpCode.generateListByBit(OpCode(0xCB,0x82),2,3)->LoadLocation.register("D"),
    OpCode.generateListByBit(OpCode(0xCB,0x83),2,3)->LoadLocation.register("E"),
    OpCode.generateListByBit(OpCode(0xCB,0x84),2,3)->LoadLocation.register("H"),
    OpCode.generateListByBit(OpCode(0xCB,0x85),2,3)->LoadLocation.register("L"),
    OpCode.generateListByBit(OpCode(0xCB,0xC7),2,3)->LoadLocation.register("A"),
    OpCode.generateListByBit(OpCode(0xCB,0xC0),2,3)->LoadLocation.register("B"),
    OpCode.generateListByBit(OpCode(0xCB,0xC1),2,3)->LoadLocation.register("C"),
    OpCode.generateListByBit(OpCode(0xCB,0xC2),2,3)->LoadLocation.register("D"),
    OpCode.generateListByBit(OpCode(0xCB,0xC3),2,3)->LoadLocation.register("E"),
    OpCode.generateListByBit(OpCode(0xCB,0xC4),2,3)->LoadLocation.register("H"),
    OpCode.generateListByBit(OpCode(0xCB,0xC5),2,3)->LoadLocation.register("L"),
    //indirect register
    OpCode.generateListByBit(OpCode(0xCB,0x46),2,3)->LoadLocation.registerAddr("HL"),
    OpCode.generateListByBit(OpCode(0xCB,0x86),2,3)->LoadLocation.registerAddr("HL"),
    OpCode.generateListByBit(OpCode(0xCB,0xC6),2,3)->LoadLocation.registerAddr("HL"),
    //indirect register with offset
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0x46),3,3)->LoadLocation.registerAddrIndirOffset("IX",2),
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0x46),3,3)->LoadLocation.registerAddrIndirOffset("IY",2),
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0x86),3,3)->LoadLocation.registerAddrIndirOffset("IX",2),
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0x86),3,3)->LoadLocation.registerAddrIndirOffset("IY",2),
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0xC6),3,3)->LoadLocation.registerAddrIndirOffset("IX",2),
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0xC6),3,3)->LoadLocation.registerAddrIndirOffset("IY",2)
  )

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val bitListMap: Map[List[OpCode],Int] =
    OpCode.generateMapByBit(OpCode(0xCB,0x47),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x40),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x41),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x42),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x43),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x44),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x45),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x46),2,3)++
    OpCode.generateMapByBit(OpCode(0xDD,0xCB,0x46),3,3)++
    OpCode.generateMapByBit(OpCode(0xFD,0xCB,0x46),3,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x87),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x80),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x81),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x82),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x83),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x84),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x85),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0x86),2,3)++
    OpCode.generateMapByBit(OpCode(0xDD,0xCB,0x86),3,3)++
    OpCode.generateMapByBit(OpCode(0xFD,0xCB,0x86),3,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC7),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC0),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC1),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC2),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC3),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC4),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC5),2,3)++
    OpCode.generateMapByBit(OpCode(0xCB,0xC6),2,3)++
    OpCode.generateMapByBit(OpCode(0xDD,0xCB,0xC6),3,3)++
    OpCode.generateMapByBit(OpCode(0xFD,0xCB,0xC6),3,3)


  val bit: OpCodeMap[Int] = new OpCodeMap(bitListMap, 0)

  val operationListMap: Map[List[OpCode],BitOperation] = Map(
    OpCode.generateListByBit(OpCode(0xCB,0x47),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x40),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x41),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x42),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x43),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x44),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x45),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x46),2,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0x46),3,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0x46),3,3)-> BitOpType.Test,
    OpCode.generateListByBit(OpCode(0xCB,0x87),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0x80),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0x81),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0x82),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0x83),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0x84),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0x85),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0x86),2,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0x86),3,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0x86),3,3)-> BitOpType.Reset,
    OpCode.generateListByBit(OpCode(0xCB,0xC7),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xCB,0xC0),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xCB,0xC1),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xCB,0xC2),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xCB,0xC3),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xCB,0xC4),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xCB,0xC5),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xCB,0xC6),2,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0xC6),3,3)-> BitOpType.Set,
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0xC6),3,3)-> BitOpType.Set

  )

  val operation: OpCodeMap[BitOperation] = new OpCodeMap(operationListMap, BitOpType.None)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    OpCode.generateListByBit(OpCode(0xCB,0x47),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x40),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x41),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x42),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x43),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x44),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x45),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x46),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0x46),3,3)-> 4,
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0x46),3,3)-> 4,
    OpCode.generateListByBit(OpCode(0xCB,0x87),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x80),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x81),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x82),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x83),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x84),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x85),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0x86),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0x86),3,3)-> 4,
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0x86),3,3)-> 4,
    OpCode.generateListByBit(OpCode(0xCB,0xC7),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0xC0),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0xC1),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0xC2),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0xC3),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0xC4),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0xC5),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xCB,0xC6),2,3)-> 2,
    OpCode.generateListByBit(OpCode(0xDD,0xCB,0xC6),3,3)-> 4,
    OpCode.generateListByBit(OpCode(0xFD,0xCB,0xC6),3,3)-> 4

  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code:OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val loc=location.find(code)

    val (value, flags) =
      handleBitManipulation(
        operation.find(code),
        bit.find(code),
        system.getValueFromLocation(loc),
        system.getRegValue("F"))
    val change=List(system.putValueToLocation(loc,value),new RegisterChange("F", flags))

    (change,instSize.find(code))
  }

  private def handleBitManipulation(oper: BitOperation, bit: Int, prevValue: Int, prevFlags:Int):(Int,Int)={
    oper match {
      case BitOpType.Test =>
        val newZ= !Z80Utils.getBit(prevValue,bit)
        val newF=new Flag(prevFlags).set(Flag.Z,newZ).set(Flag.H).reset(Flag.N)()
        (prevValue,newF)
      case BitOpType.Reset => (Z80Utils.resetBit(prevValue,bit),prevFlags)
      case BitOpType.Set => (Z80Utils.setBit(prevValue,bit),prevFlags)
    }
  }
}