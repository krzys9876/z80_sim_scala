package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{SystemChangeBase, Z80System}

sealed abstract class Load8BitOpType(val name:String)

object Load8BitOpType {
  case object Load extends Load8BitOpType("LOAD")
  case object None extends Load8BitOpType("NONE")
}

object Load8Bit extends LoadSpec with OpCodeHandler {
  override val sourceLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  override val destLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap, Location.empty)
  override val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)

  val loadList: OpCodeMap[Load8BitOpType] = new OpCodeMap(OpCodes.load8bMap,Load8BitOpType.None)
  override lazy val isOper: OpCode=>Boolean = opcode => loadList.contains(opcode)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val value=system.getValueFromLocation(sourceLoc.find(code))
    (List(system.putValueToLocation(destLoc.find(code),value)),instSize.find(code))
  }
}
