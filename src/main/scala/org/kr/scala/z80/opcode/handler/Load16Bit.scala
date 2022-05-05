package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{RegisterChangeRelative, SystemChangeBase, Z80System}

sealed abstract class Load16BitOpType(val name:String)

object Load16BitOpType {
  case object Load extends Load16BitOpType("LOAD")
  case object None extends Load16BitOpType("NONE")
}

object Load16Bit extends LoadSpec with OpCodeHandler {
  override lazy val sourceLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  override lazy val destLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap, Location.empty)
  override lazy val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)
  lazy val stackChange: OpCodeMap[Int] = new OpCodeMap(OpCodes.stackChangeMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val sourceLoc = Load16Bit.sourceLoc.find(code)
    val value = system.getValueFromLocation(sourceLoc)
    val destLoc = Load16Bit.destLoc.find(code)
    val instrSize = Load16Bit.instSize.find(code)
    val stackChange = Load16Bit.stackChange.find(code)

    val chgList = List(system.putValueToLocation(destLoc, value, isWord = true))
    val stackChgList = destLoc match {
      case Location(r, _, _, rd, dirO, _, _) if r != "" || (rd != "" && dirO != OpCode.ANY) =>
        List(new RegisterChangeRelative("SP", stackChange))
      case _ => List()
    }
    (chgList ++ stackChgList, instrSize)
  }

}
