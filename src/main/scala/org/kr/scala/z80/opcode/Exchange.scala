package org.kr.scala.z80.opcode

object Exchange extends LoadSpec{
  val sourceLocListMap: Map[List[OpCode], LoadLocation] = Map(
    //register pair
    List(OpCode(0xEB, OpCode.ANY)) -> LoadLocation.register("HL")
  )

  override val sourceLoc: OpCodeMap[LoadLocation] = new OpCodeMap(sourceLocListMap, LoadLocation.empty)

  val destLocListMap: Map[List[OpCode], LoadLocation] = Map(
    // register pair
    List(OpCode(0xEB, OpCode.ANY)) -> LoadLocation.register("DE")
  )

  override val destLoc: OpCodeMap[LoadLocation] = new OpCodeMap(destLocListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xEB, OpCode.ANY)) -> 1
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

}
