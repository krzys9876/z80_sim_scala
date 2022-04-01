package org.kr.scala.z80

object Load16Bit extends LoadSpec {
  // Z80 manual page 45
  override val sourceLocListMap:Map[List[OpCode],LoadLocation]=Map(
    //registers
    List(OpCode(0xF9,OpCode.ANY)) -> LoadLocation.register("HL"),
    List(OpCode(0xDD,0xF9)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0xF9)) -> LoadLocation.register("IY"),
    // immediate
    List(OpCode(0x01,OpCode.ANY),OpCode(0x11,OpCode.ANY),OpCode(0x21,OpCode.ANY),
      OpCode(0x31,OpCode.ANY)) -> LoadLocation.registerAddrDirOffset("PC",1),
    List(OpCode(0xDD,0x21),OpCode(0xFD,0x21)) -> LoadLocation.registerAddrDirOffset("PC",2),
    // immediate address
    List(OpCode(0x2A,OpCode.ANY)) -> LoadLocation.indirAddress(1),
    List(OpCode(0xED,0x4B),OpCode(0xED,0x5B),OpCode(0xED,0x7B),
      OpCode(0xDD,0x2A),OpCode(0xFD,0x2A)) -> LoadLocation.indirAddress(2)
  )

  override val destLocListMap:Map[List[OpCode],LoadLocation]=Map(
    List(OpCode(0x01,OpCode.ANY),OpCode(0xED,0x4B)) -> LoadLocation.register("BC"),
    List(OpCode(0x11,OpCode.ANY),OpCode(0xED,0x5B)) -> LoadLocation.register("DE"),
    List(OpCode(0x21,OpCode.ANY),OpCode(0x2A,OpCode.ANY)) -> LoadLocation.register("HL"),
    List(OpCode(0x31,OpCode.ANY),OpCode(0xED,0x7B),OpCode(0xF9,OpCode.ANY),
      OpCode(0xDD,0xF9),OpCode(0xFD,0xF9)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD,0x21),OpCode(0xDD,0x2A)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0x21),OpCode(0xFD,0x2A)) -> LoadLocation.register("IY")
  )

  override val instructionSizeListMap:Map[List[OpCode],Int]=Map(
    List(OpCode(0xF9,OpCode.ANY)) -> 1,
    List(OpCode(0xDD,0xF9),OpCode(0xFD,0xF9)) -> 2,
    List(OpCode(0x01,OpCode.ANY),OpCode(0x11,OpCode.ANY),OpCode(0x21,OpCode.ANY),OpCode(0x31,OpCode.ANY),
      OpCode(0x2A,OpCode.ANY)) -> 3,
    List(OpCode(0xDD,0x21),OpCode(0xFD,0x21),OpCode(0xED,0x4B),OpCode(0xED,0x5B),
      OpCode(0xED,0x7B),OpCode(0xDD,0x2A),OpCode(0xFD,0x2A)) -> 4
  )
}
