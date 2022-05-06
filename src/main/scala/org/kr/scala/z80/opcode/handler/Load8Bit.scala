package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, SystemChangeBase, Z80System}

sealed abstract class Load8BitOpType(val name:String)

object Load8BitOpType {
  case object Load extends Load8BitOpType("LOAD")
  case object None extends Load8BitOpType("NONE")
}

object Load8Bit extends LoadSpec with OpCodeHandler {
  override lazy val sourceLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  override lazy val destLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap, Location.empty)
  override lazy val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int) = {
    val value = system.getValueFromLocation(sourceLoc.find(code))
    (List(system.putValueToLocation(destLoc.find(code), value)), instSize.find(code))
  }
}
