package org.kr.scala.z80

object Load16Bit extends LoadSpec {
  // Z80 manual page 45
  val sourceLocListMap:Map[List[OpCode],LoadLocation]=Map(
    //registers
    List(OpCode(0xF6,OpCode.ANY)) -> LoadLocation.register("AF"),
    List(OpCode(0xED,0x43),OpCode(0xC6,OpCode.ANY)) -> LoadLocation.register("BC"),
    List(OpCode(0xED,0x53),OpCode(0xD6,OpCode.ANY)) -> LoadLocation.register("DE"),
    List(OpCode(0xF9,OpCode.ANY),OpCode(0x22,OpCode.ANY),OpCode(0xE6,OpCode.ANY)) -> LoadLocation.register("HL"),
    List(OpCode(0xED,0x73)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD,0xF9),OpCode(0xDD,0x22),OpCode(0xDD,0xE6)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0xF9),OpCode(0xFD,0x22),OpCode(0xFD,0xE6)) -> LoadLocation.register("IY"),
    // indirect register
    List(OpCode(0xF1,OpCode.ANY),OpCode(0xC1,OpCode.ANY),OpCode(0xD1,OpCode.ANY),OpCode(0xE1,OpCode.ANY),
      OpCode(0xDD,0xE1),OpCode(0xFD,0xE1)) -> LoadLocation.registerAddr("SP"),
    // immediate
    List(OpCode(0x01,OpCode.ANY),OpCode(0x11,OpCode.ANY),OpCode(0x21,OpCode.ANY),
      OpCode(0x31,OpCode.ANY)) -> LoadLocation.registerAddrDirOffset("PC",1),
    List(OpCode(0xDD,0x21),OpCode(0xFD,0x21)) -> LoadLocation.registerAddrDirOffset("PC",2),
    // immediate address
    List(OpCode(0x2A,OpCode.ANY)) -> LoadLocation.indirAddress(1),
    List(OpCode(0xED,0x4B),OpCode(0xED,0x5B),OpCode(0xED,0x7B),
      OpCode(0xDD,0x2A),OpCode(0xFD,0x2A)) -> LoadLocation.indirAddress(2)
  )

  override val sourceLoc: OpCodeMap[LoadLocation] = new OpCodeMap(sourceLocListMap,LoadLocation.empty)

  val destLocListMap:Map[List[OpCode],LoadLocation]=Map(
    // register pair
    List(OpCode(0xF1,OpCode.ANY)) -> LoadLocation.register("AF"),
    List(OpCode(0x01,OpCode.ANY),OpCode(0xED,0x4B),OpCode(0xC1,OpCode.ANY)) -> LoadLocation.register("BC"),
    List(OpCode(0x11,OpCode.ANY),OpCode(0xED,0x5B),OpCode(0xD1,OpCode.ANY)) -> LoadLocation.register("DE"),
    List(OpCode(0x21,OpCode.ANY),OpCode(0x2A,OpCode.ANY),OpCode(0xE1,OpCode.ANY)) -> LoadLocation.register("HL"),
    List(OpCode(0x31,OpCode.ANY),OpCode(0xED,0x7B),OpCode(0xF9,OpCode.ANY),
      OpCode(0xDD,0xF9),OpCode(0xFD,0xF9)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD,0x21),OpCode(0xDD,0x2A),OpCode(0xDD,0xE1)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0x21),OpCode(0xFD,0x2A),OpCode(0xFD,0xE1)) -> LoadLocation.register("IY"),
    // indirect register
    List(OpCode(0xF6,OpCode.ANY),OpCode(0xC6,OpCode.ANY),OpCode(0xD6,OpCode.ANY),OpCode(0xE6,OpCode.ANY),
      OpCode(0xDD,0xE6),OpCode(0xFD,0xE6)) -> LoadLocation.registerAddrDirOffset("SP",-2),
    // immediate address
    List(OpCode(0xED,0x43),OpCode(0xED,0x53),OpCode(0xED,0x73),OpCode(0xDD,0x22),
      OpCode(0xFD,0x22)) -> LoadLocation.indirAddress(2),
    List(OpCode(0x22,OpCode.ANY)) -> LoadLocation.indirAddress(1)
  )

  override val destLoc: OpCodeMap[LoadLocation] = new OpCodeMap(destLocListMap,LoadLocation.empty)

  val instructionSizeListMap:Map[List[OpCode],Int]=Map(
    List(OpCode(0xF9,OpCode.ANY),OpCode(0xF1,OpCode.ANY),OpCode(0xC1,OpCode.ANY),
      OpCode(0xD1,OpCode.ANY),OpCode(0xE1,OpCode.ANY),
      OpCode(0xF6,OpCode.ANY),OpCode(0xC6,OpCode.ANY),OpCode(0xD6,OpCode.ANY),OpCode(0xE6,OpCode.ANY)) -> 1,
    List(OpCode(0xDD,0xF9),OpCode(0xFD,0xF9),OpCode(0xDD,0xE1),OpCode(0xFD,0xE1),
      OpCode(0xDD,0xE6),OpCode(0xFD,0xE6)) -> 2,
    List(OpCode(0x01,OpCode.ANY),OpCode(0x11,OpCode.ANY),OpCode(0x21,OpCode.ANY),OpCode(0x31,OpCode.ANY),
      OpCode(0x2A,OpCode.ANY),OpCode(0x22,OpCode.ANY)) -> 3,
    List(OpCode(0xDD,0x21),OpCode(0xFD,0x21),OpCode(0xED,0x4B),OpCode(0xED,0x5B),
      OpCode(0xED,0x7B),OpCode(0xDD,0x2A),OpCode(0xFD,0x2A),
      OpCode(0xED,0x43),OpCode(0xED,0x53),OpCode(0xED,0x73),
      OpCode(0xDD,0x22),OpCode(0xFD,0x22)) -> 4
  )

  val stackChangeListMap:Map[List[OpCode],Int]=Map(
    // POP (SP+2)
    List(OpCode(0xF1,OpCode.ANY),OpCode(0xC1,OpCode.ANY),OpCode(0xD1,OpCode.ANY),OpCode(0xE1,OpCode.ANY),
      OpCode(0xDD,0xE1),OpCode(0xFD,0xE1)) -> 2,
    // PUSH (SP-2)
    List(OpCode(0xF6,OpCode.ANY),OpCode(0xC6,OpCode.ANY),OpCode(0xD6,OpCode.ANY),OpCode(0xE6,OpCode.ANY),
      OpCode(0xDD,0xE6),OpCode(0xFD,0xE6)) -> -2
  )
  lazy val stackChange:Map[OpCode,Int]=Z80Utils.flattenMapOfLists(stackChangeListMap)
  lazy val getstackChange: OpCode=>Int = opcode =>
    stackChange.getOrElse(opcode,stackChange.getOrElse(OpCode(opcode.main,OpCode.ANY),0))

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap,0)
}
