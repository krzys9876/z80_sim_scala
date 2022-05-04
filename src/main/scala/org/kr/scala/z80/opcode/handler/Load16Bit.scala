package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{RegisterChangeRelative, SystemChangeBase, Z80System}

sealed abstract class Load16BitOpType(val name:String)

object Load16BitOpType {
  case object Load extends Load16BitOpType("LOAD")
  case object None extends Load16BitOpType("NONE")
}

object Load16Bit extends LoadSpec with OpCodeHandler {
  override val sourceLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  override val destLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap, Location.empty)
  override val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)
  val stackChange: OpCodeMap[Int] = new OpCodeMap(OpCodes.stackChangeMap, 0)

  val loadList: OpCodeMap[Load16BitOpType] = new OpCodeMap(OpCodes.load16bMap, Load16BitOpType.None)
  override lazy val isOper: OpCode => Boolean = opcode => loadList.contains(opcode)

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
