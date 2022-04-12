package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{OutputChange, SystemChangeBase, Z80System}

object InputOutput extends OperationSpec with OpCodeHandler {

  val portLocationListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0xD3))->LoadLocation.registerAddrDirOffset("PC", 1)
  )
  val portLocation: OpCodeMap[LoadLocation] = new OpCodeMap(portLocationListMap, LoadLocation.empty)

  val valueLocationListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0xD3))->LoadLocation.register("A")
  )
  val valueLocation: OpCodeMap[LoadLocation] = new OpCodeMap(valueLocationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xD3))->2
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val port=system.getValueFromLocation(portLocation.find(code))
    val value=system.getValueFromLocation(valueLocation.find(code))
    (List(new OutputChange(port,value)),instSize.find(code))

  }

}
