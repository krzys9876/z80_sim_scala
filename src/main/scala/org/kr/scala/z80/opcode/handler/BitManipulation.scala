package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{Location, OpCode, OpCodeHandler, OpCodeMap, OpCodes, OperationSpec}
import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

sealed abstract class BitOperation(val name: String)

object BitOpType {
  case object Test extends BitOperation("TEST")
  case object Reset extends BitOperation("RESET")
  case object Set extends BitOperation("SET")
  case object None extends BitOperation("NONE")
}

object BitManipulation extends OperationSpec with OpCodeHandler {
  //Z80 manual p.55 - NOTE error in opCode: 0xCB, not 0xC8!
  val source: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  val bit: OpCodeMap[Int] = new OpCodeMap(OpCodes.bitNumMap, 0)
  val operation: OpCodeMap[BitOperation] = new OpCodeMap(OpCodes.bitManipulationMap, BitOpType.None)
  override val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)

  override lazy val isOper: OpCode=>Boolean = opcode => operation.contains(opcode)

  override def handle(code:OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val loc=source.find(code)

    val (value, flags) =
      handleBitManipulation(
        operation.find(code),
        bit.find(code),
        system.getValueFromLocation(loc),
        system.getFlags())
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

