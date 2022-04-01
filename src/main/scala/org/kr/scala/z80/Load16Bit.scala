package org.kr.scala.z80

object Load16Bit extends LoadSpec {
  // Z80 manual page 45
  override val sourceLocListMap:Map[List[OpCode],LoadLocation]=Map(
    List(OpCode(0x01,OpCode.ANY),OpCode(0x11,OpCode.ANY),OpCode(0x21,OpCode.ANY),
      OpCode(0x31,OpCode.ANY)) -> LoadLocation.indirAddress(1),
    List(OpCode(0xDD,0x21),OpCode(0xFD,0x21)) -> LoadLocation.indirAddress(2)
  )

  override val destLocListMap:Map[List[OpCode],LoadLocation]=Map(
    List(OpCode(0x01,OpCode.ANY)) -> LoadLocation.register("BC"),
    List(OpCode(0x11,OpCode.ANY)) -> LoadLocation.register("DE"),
    List(OpCode(0x21,OpCode.ANY)) -> LoadLocation.register("HL"),
    List(OpCode(0x31,OpCode.ANY)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD,0x21)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0x21)) -> LoadLocation.register("IY")
  )

  override val instructionSizeListMap:Map[List[OpCode],Int]=Map(
    List(OpCode(0x01,OpCode.ANY),OpCode(0x11,OpCode.ANY),OpCode(0x21,OpCode.ANY),OpCode(0x31,OpCode.ANY)) -> 3,
    List(OpCode(0xDD,0x21),OpCode(0xFD,0x21)) -> 4
  )
}
