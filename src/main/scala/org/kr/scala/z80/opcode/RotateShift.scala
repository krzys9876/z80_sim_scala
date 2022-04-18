package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

abstract class ArithmeticOperation(val name:String)

object ArithmeticOpType {
  case object Srl extends ArithmeticOperation("SRL")
  case object Rld extends ArithmeticOperation("RLD")
  case object Rrd extends ArithmeticOperation("RRD")
  case object None extends ArithmeticOperation("NONE")
}

object RotateShift extends OperationSpec with OpCodeHandler {
  // Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
  val operationListMap: Map[List[OpCode],ArithmeticOperation] = Map(
    List(OpCode(0xCB,0x07),OpCode(0xCB,0x00),OpCode(0xCB,0x01),OpCode(0xCB,0x02),OpCode(0xCB,0x03),
      OpCode(0xCB,0x04),OpCode(0xCB,0x05),OpCode(0xCB,0x06),OpCode(0xDD,0xCB,0x06),
      OpCode(0xFD,0xCB,0x06)) -> Rlc,
    List(OpCode(0x07)) -> Rlca,
    List(OpCode(0xCB,0x0F),OpCode(0xCB,0x08),OpCode(0xCB,0x09),OpCode(0xCB,0x0A),OpCode(0xCB,0x0B),
      OpCode(0xCB,0x0C),OpCode(0xCB,0x0D),OpCode(0xCB,0x0E),OpCode(0xDD,0xCB,0x0E),
      OpCode(0xFD,0xCB,0x0E)) -> Rrc,
    List(OpCode(0x0F)) -> Rrca,
    List(OpCode(0xCB,0x17),OpCode(0xCB,0x10),OpCode(0xCB,0x11),OpCode(0xCB,0x12),OpCode(0xCB,0x13),
      OpCode(0xCB,0x14),OpCode(0xCB,0x15),OpCode(0xCB,0x16),OpCode(0xDD,0xCB,0x16),
      OpCode(0xFD,0xCB,0x16)) -> Rl,
    List(OpCode(0x17)) -> Rla,
    List(OpCode(0xCB,0x1F),OpCode(0xCB,0x18),OpCode(0xCB,0x19),OpCode(0xCB,0x1A),OpCode(0xCB,0x1B),
      OpCode(0xCB,0x1C),OpCode(0xCB,0x1D),OpCode(0xCB,0x1E),OpCode(0xDD,0xCB,0x1E),
      OpCode(0xFD,0xCB,0x1E)) -> Rr,
    List(OpCode(0x1F)) -> Rra,
    List(OpCode(0xCB,0x27),OpCode(0xCB,0x20),OpCode(0xCB,0x21),OpCode(0xCB,0x22),OpCode(0xCB,0x23),
      OpCode(0xCB,0x24),OpCode(0xCB,0x25),OpCode(0xCB,0x26),OpCode(0xDD,0xCB,0x26),
      OpCode(0xFD,0xCB,0x26)) -> Sla,
    List(OpCode(0xCB,0x2F),OpCode(0xCB,0x28),OpCode(0xCB,0x29),OpCode(0xCB,0x2A),OpCode(0xCB,0x2B),
      OpCode(0xCB,0x2C),OpCode(0xCB,0x2D),OpCode(0xCB,0x2E),OpCode(0xDD,0xCB,0x2E),
      OpCode(0xFD,0xCB,0x2E)) -> Sra,
    List(OpCode(0xCB,0x3F),OpCode(0xCB,0x38),OpCode(0xCB,0x39),OpCode(0xCB,0x3A),OpCode(0xCB,0x3B),
      OpCode(0xCB,0x3C),OpCode(0xCB,0x3D),OpCode(0xCB,0x3E),OpCode(0xDD,0xCB,0x3E),
      OpCode(0xFD,0xCB,0x3E)) -> ArithmeticOpType.Srl
  )

  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(operationListMap, ArithmeticOpType.None)

  val locationListMap: Map[List[OpCode], LoadLocation] = Map(
    List(OpCode(0x07),OpCode(0x0F),OpCode(0x17),OpCode(0x1F)) -> LoadLocation.register("A"),
    List(OpCode(0xCB,0x06),OpCode(0xCB,0x0E),OpCode(0xCB,0x16),OpCode(0xCB,0x1E),
      OpCode(0xCB,0x26),OpCode(0xCB,0x2E),OpCode(0xCB,0x3E)) -> LoadLocation.registerAddr("HL"),
    List(OpCode(0xDD,0xCB,0x06),OpCode(0xDD,0xCB,0x0E),OpCode(0xDD,0xCB,0x16),OpCode(0xDD,0xCB,0x1E),
      OpCode(0xDD,0xCB,0x26),OpCode(0xDD,0xCB,0x2E),OpCode(0xDD,0xCB,0x3E)
    ) -> LoadLocation.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD,0xCB,0x06),OpCode(0xFD,0xCB,0x0E),OpCode(0xFD,0xCB,0x16),OpCode(0xFD,0xCB,0x1E),
      OpCode(0xFD,0xCB,0x26),OpCode(0xFD,0xCB,0x2E),OpCode(0xFD,0xCB,0x3E)
    ) -> LoadLocation.registerAddrIndirOffset("IY",2)
  )++
  //registers
    OpCode.generateMapByReg(OpCode(0xCB,0x00),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x08),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x10),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x18),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x20),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x28),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x03),2,0)

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x07),OpCode(0x0F),OpCode(0x17),OpCode(0x1F)) ->1,
    List(OpCode(0xCB,0x06),OpCode(0xCB,0x0E),OpCode(0xCB,0x16),OpCode(0xCB,0x1E),OpCode(0xCB,0x26),
      OpCode(0xCB,0x2E),OpCode(0xCB,0x3E)) ->2,
    List(OpCode(0xDD,0xCB,0x06),OpCode(0xFD,0xCB,0x06),OpCode(0xDD,0xCB,0x0E),OpCode(0xFD,0xCB,0x0E),
      OpCode(0xDD,0xCB,0x16),OpCode(0xFD,0xCB,0x16),OpCode(0xDD,0xCB,0x1E),OpCode(0xFD,0xCB,0x1E),
      OpCode(0xDD,0xCB,0x26),OpCode(0xFD,0xCB,0x26),OpCode(0xDD,0xCB,0x2E),OpCode(0xFD,0xCB,0x2E),
      OpCode(0xDD,0xCB,0x3E),OpCode(0xFD,0xCB,0x3E)) ->4,
    OpCode.generateListByReg(OpCode(0xCB,0x00),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x08),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x10),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x18),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x20),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x28),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x38),2,0)->2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val oper = operation.find(code)
    val instrSize = instSize.find(code)
    val loc=location.find(code)
    val prevValue=system.getValueFromLocation(loc)
    val prevFlags=system.getFlags

    val (value, flags) = handleRotateShift(oper, prevValue, prevFlags)

    (List(
      system.putValueToLocation(loc,value),
      new RegisterChange("F", flags.value)
    ),
      instrSize)
  }

  private def handleRotateShift(operation:ArithmeticOperation,prevValueIn:Int,prevFlags:Flag):(Int,Flag)={



    val (valueOut,newF)= operation match {
      case Rlc => Rlc.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Rlca => Rlca.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Rrc => Rrc.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Rrca => Rrca.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Rl => Rl.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Rla => Rla.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Rr => Rr.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Rra => Rra.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Sla => Sla.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
      case Sra => Sra.calcAll(ArithmeticOpInput(prevValueIn,OpCode.ANY,prevFlags))
    }
    (valueOut,newF)
  }

}

class RotateLeftBase(override val name:String) extends ArithmeticOperationCalc(name) with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCBit7

class RotateLeftBaseAccum(override val name:String) extends ArithmeticOperationCalc(name) with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCBit7

class RotateRightBase(override val name:String) extends ArithmeticOperationCalc(name) with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCBit0

class RotateRightBaseAccum(override val name:String) extends ArithmeticOperationCalc(name) with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCBit0

object Rlc extends RotateLeftBase("RLC") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value << 1) & 0xFF) + (if(Z80Utils.getBit(input.value,7)) 1 else 0)
}

object Rlca extends RotateLeftBaseAccum("RLCA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = Rlc.calcUnsigned(input)
}

object Rrc extends RotateRightBase("RRC") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value >> 1) & 0xFF) + (if(Z80Utils.getBit(input.value,0)) 0x80 else 0)
}

object Rrca extends RotateRightBaseAccum("RRCA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = Rrc.calcUnsigned(input)
}

object Rl extends RotateLeftBase("RL") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value << 1) & 0xFF) + input.flags.flagValue(Flag.C)
}

object Rla extends RotateLeftBaseAccum("RL") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = Rl.calcUnsigned(input)
}

object Rr extends RotateRightBase("RR") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value >> 1) & 0xFF) + (input.flags.flagValue(Flag.C) << 7)
}

object Rra extends RotateRightBaseAccum("RRA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = Rr.calcUnsigned(input)
}

object Sla extends RotateLeftBase("SLA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = (input.value << 1) & 0xFF
}

object Sra extends RotateRightBase("SRA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value >> 1) & 0xFF) + (if(Z80Utils.getBit(input.value,7)) 0x80 else 0)
}
