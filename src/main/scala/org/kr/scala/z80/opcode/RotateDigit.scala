package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object RotateDigit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
  val operationListMap: Map[List[OpCode],ArithmeticOperation] = Map(
    List(OpCode(0xED,0x6F)) -> ArithmeticOpType.Rld,
    List(OpCode(0xED,0x67)) -> ArithmeticOpType.Rrd
  )

  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(operationListMap, ArithmeticOpType.None)

  val locationListMap: Map[List[OpCode], LoadLocation] = Map(
    List(OpCode(0xED,0x6F),OpCode(0xED,0x67)) -> LoadLocation.registerAddr("HL")
  )

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xED,0x6F),OpCode(0xED,0x67)) ->2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    //http://www.z80.info/z80sflag.htm
    val loc=location.find(code)

    val (valueOutR, valueOutA)=handleRotate(
      operation.find(code),
      system.getValueFromLocation(loc),
      system.getRegValue("A"))

    val newF=
      new Flag(system.getRegValue("F"))
      .set(Flag.S,Z80Utils.isNegative(valueOutA))
      .set(Flag.Z,valueOutA==0)
      .reset(Flag.H)
      .set(Flag.P,Z80Utils.isEvenBits(valueOutA))
      .reset(Flag.N)

    val change=List(
      system.putValueToLocation(loc,valueOutR),
      new RegisterChange("A",valueOutA),
      new RegisterChange("F", newF()))

    (change,instSize.find(code))
  }

  private def handleRotate(oper:ArithmeticOperation,valueR:Int,valueA:Int):(Int,Int)={
    val unchangedDigit1A = valueA & 0xF0
    val digit2A = valueA & 0x0F
    val digit1R = (valueR & 0xF0) >> 4
    val digit2R = valueR & 0x0F
    oper match {
      case ArithmeticOpType.Rld => ((digit2R << 4) + digit2A,unchangedDigit1A + digit1R)
      case ArithmeticOpType.Rrd => ((digit2A << 4) + digit1R,unchangedDigit1A + digit2R)
    }
  }
}
