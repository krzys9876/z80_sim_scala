package org.kr.scala.z80.opcode

object Load8Bit extends LoadSpec {
  // Z80 manual page 42
  val sourceLocListMap: Map[List[OpCode], LoadLocation] = Map(
    // registers
    List(OpCode(0xED, 0x57)) -> LoadLocation.register("I"),
    List(OpCode(0xED, 0x5F)) -> LoadLocation.register("R"),
    List(OpCode(0x7F), OpCode(0x4F), OpCode(0x47), OpCode(0x57),
      OpCode(0x5F), OpCode(0x67), OpCode(0x6F), OpCode(0x77),
      OpCode(0x02), OpCode(0x12), OpCode(0xDD, 0x77), OpCode(0xFD, 0x77),
      OpCode(0x32), OpCode(0xED, 0x47), OpCode(0xED, 0x4F)) -> LoadLocation.register("A"),
    List(OpCode(0x78), OpCode(0x40), OpCode(0x48), OpCode(0x50),
      OpCode(0x58), OpCode(0x60), OpCode(0x68), OpCode(0x70),
      OpCode(0xDD, 0x70), OpCode(0xFD, 0x70)) -> LoadLocation.register("B"),
    List(OpCode(0x79), OpCode(0x41), OpCode(0x49), OpCode(0x51),
      OpCode(0x59), OpCode(0x61), OpCode(0x69), OpCode(0x71),
      OpCode(0xDD, 0x71), OpCode(0xFD, 0x71)) -> LoadLocation.register("C"),
    List(OpCode(0x7A), OpCode(0x42), OpCode(0x4A), OpCode(0x52),
      OpCode(0x5A), OpCode(0x62), OpCode(0x6A), OpCode(0x72),
      OpCode(0xDD, 0x72), OpCode(0xFD, 0x72)) -> LoadLocation.register("D"),
    List(OpCode(0x7B), OpCode(0x43), OpCode(0x4B), OpCode(0x53),
      OpCode(0x5B), OpCode(0x63), OpCode(0x6B), OpCode(0x73),
      OpCode(0xDD, 0x73), OpCode(0xFD, 0x73)) -> LoadLocation.register("E"),
    List(OpCode(0x7C), OpCode(0x44), OpCode(0x4C), OpCode(0x54),
      OpCode(0x5C), OpCode(0x64), OpCode(0x6C), OpCode(0x74),
      OpCode(0xDD, 0x74), OpCode(0xFD, 0x74)) -> LoadLocation.register("H"),
    List(OpCode(0x7D), OpCode(0x45), OpCode(0x4D), OpCode(0x55),
      OpCode(0x5D), OpCode(0x65), OpCode(0x6D), OpCode(0x75),
      OpCode(0xDD, 0x75), OpCode(0xFD, 0x75)) -> LoadLocation.register("L"),
    // indirect registers
    List(OpCode(0x7E), OpCode(0x46), OpCode(0x4E), OpCode(0x56),
      OpCode(0x5E), OpCode(0x66), OpCode(0x6E)) -> LoadLocation.registerAddr("HL"),
    List(OpCode(0x0A)) -> LoadLocation.registerAddr("BC"),
    List(OpCode(0x1A)) -> LoadLocation.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD, 0x7E), OpCode(0xDD, 0x46), OpCode(0xDD, 0x4E), OpCode(0xDD, 0x56), OpCode(0xDD, 0x5E),
      OpCode(0xDD, 66), OpCode(0xDD, 0x6E)) -> LoadLocation.registerAddrIndirOffset("IX", 2),
    List(OpCode(0xFD, 0x7E), OpCode(0xFD, 0x46), OpCode(0xFD, 0x4E), OpCode(0xFD, 0x56), OpCode(0xFD, 0x5E),
      OpCode(0xFD, 66), OpCode(0xFD, 0x6E)) -> LoadLocation.registerAddrIndirOffset("IY", 2),
    // immediate address
    List(OpCode(0x3A)) -> LoadLocation.indirAddress(1),
    // immediate
    List(OpCode(0x3E), OpCode(0x06), OpCode(0x0E), OpCode(0x16),
      OpCode(0x1E), OpCode(0x26), OpCode(0x2E),
      OpCode(0x36)) -> LoadLocation.registerAddrDirOffset("PC", 1),
    List(OpCode(0xDD, 0x36), OpCode(0xFD, 0x36)) -> LoadLocation.registerAddrDirOffset("PC", 3)
  )

  override val sourceLoc: OpCodeMap[LoadLocation] = new OpCodeMap(sourceLocListMap, LoadLocation.empty)

  val destLocListMap: Map[List[OpCode], LoadLocation] = Map(
    // registers
    List(OpCode(0xED, 0x57), OpCode(0xED, 0x5F), OpCode(0x7F), OpCode(0x78), OpCode(0x79),
      OpCode(0x7A), OpCode(0x7B), OpCode(0x7C), OpCode(0x7D),
      OpCode(0x7E), OpCode(0x7F), OpCode(0x0A), OpCode(0x1A), OpCode(0xDD, 0x7E),
      OpCode(0xFD, 0x7E), OpCode(0x3A), OpCode(0x3E)) -> LoadLocation.register("A"),
    List(OpCode(0x47), OpCode(0x40), OpCode(0x41), OpCode(0x42),
      OpCode(0x43), OpCode(0x44), OpCode(0x45),
      OpCode(0x46), OpCode(0xDD, 0x46), OpCode(0xFD, 0x46), OpCode(0x06)) -> LoadLocation.register("B"),
    List(OpCode(0x4F), OpCode(0x48), OpCode(0x49), OpCode(0x4A),
      OpCode(0x4B), OpCode(0x4C), OpCode(0x4D),
      OpCode(0x4E), OpCode(0xDD, 0x4E), OpCode(0xFD, 0x4E), OpCode(0x0E)) -> LoadLocation.register("C"),
    List(OpCode(0x57), OpCode(0x50), OpCode(0x51), OpCode(0x52),
      OpCode(0x53), OpCode(0x54), OpCode(0x55),
      OpCode(0x56), OpCode(0xDD, 0x56), OpCode(0xFD, 0x56), OpCode(0x16)) -> LoadLocation.register("D"),
    List(OpCode(0x5F), OpCode(0x58), OpCode(0x59), OpCode(0x5A),
      OpCode(0x5B), OpCode(0x5C), OpCode(0x5D),
      OpCode(0x5E), OpCode(0xDD, 0x5E), OpCode(0xFD, 0x5E), OpCode(0x1E)) -> LoadLocation.register("E"),
    List(OpCode(0x67), OpCode(0x60), OpCode(0x61), OpCode(0x62),
      OpCode(0x63), OpCode(0x64), OpCode(0x65),
      OpCode(0x66), OpCode(0xDD, 0x66), OpCode(0xFD, 0x66), OpCode(0x26)) -> LoadLocation.register("H"),
    List(OpCode(0x6F), OpCode(0x68), OpCode(0x69), OpCode(0x6A),
      OpCode(0x6B), OpCode(0x6C), OpCode(0x6D),
      OpCode(0x6E), OpCode(0xDD, 0x6E), OpCode(0xFD, 0x6E), OpCode(0x2E)) -> LoadLocation.register("L"),
    List(OpCode(0xED, 0x47)) -> LoadLocation.register("I"),
    List(OpCode(0xED, 0x4F)) -> LoadLocation.register("R"),
    // indirect registers
    List(OpCode(0x77), OpCode(0x70), OpCode(0x71), OpCode(0x72),
      OpCode(0x73), OpCode(0x74), OpCode(0x75), OpCode(0x36))
      -> LoadLocation.registerAddr("HL"),
    List(OpCode(0x02)) -> LoadLocation.registerAddr("BC"),
    List(OpCode(0x12)) -> LoadLocation.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD, 0x77), OpCode(0xDD, 0x70), OpCode(0xDD, 0x71), OpCode(0xDD, 0x72), OpCode(0xDD, 0x73),
      OpCode(0xDD, 74), OpCode(0xDD, 0x75), OpCode(0xDD, 0x36)) -> LoadLocation.registerAddrIndirOffset("IX", 2),
    List(OpCode(0xFD, 0x77), OpCode(0xFD, 0x70), OpCode(0xFD, 0x71), OpCode(0xFD, 0x72), OpCode(0xFD, 0x73),
      OpCode(0xFD, 74), OpCode(0xFD, 0x75), OpCode(0xFD, 0x36)) -> LoadLocation.registerAddrIndirOffset("IY", 2),
    // immediate address
    List(OpCode(0x32)) -> LoadLocation.indirAddress(1)
  )

  override val destLoc: OpCodeMap[LoadLocation] = new OpCodeMap(destLocListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x7F), OpCode(0x4F), OpCode(0x47), OpCode(0x57),
      OpCode(0x5F), OpCode(0x67), OpCode(0x6F), OpCode(0x77),
      OpCode(0x02), OpCode(0x12),
      OpCode(0x78), OpCode(0x40), OpCode(0x48), OpCode(0x50),
      OpCode(0x58), OpCode(0x60), OpCode(0x68), OpCode(0x70),
      OpCode(0x79), OpCode(0x41), OpCode(0x49), OpCode(0x51),
      OpCode(0x59), OpCode(0x61), OpCode(0x69), OpCode(0x71),
      OpCode(0x7A), OpCode(0x42), OpCode(0x4A), OpCode(0x52),
      OpCode(0x5A), OpCode(0x62), OpCode(0x6A), OpCode(0x72),
      OpCode(0x7B), OpCode(0x43), OpCode(0x4B), OpCode(0x53),
      OpCode(0x5B), OpCode(0x63), OpCode(0x6B), OpCode(0x73),
      OpCode(0x7C), OpCode(0x44), OpCode(0x4C), OpCode(0x54),
      OpCode(0x5C), OpCode(0x64), OpCode(0x6C), OpCode(0x74),
      OpCode(0x7D), OpCode(0x45), OpCode(0x4D), OpCode(0x55),
      OpCode(0x5D), OpCode(0x65), OpCode(0x6D), OpCode(0x75),
      OpCode(0x0A), OpCode(0x1A),
      OpCode(0x7E), OpCode(0x46), OpCode(0x4E), OpCode(0x56),
      OpCode(0x5E), OpCode(0x66), OpCode(0x6E),
      OpCode(0x1A)) -> 1,
    List(OpCode(0xED, 0x57), OpCode(0xED, 0x5F), OpCode(0xED, 0x47), OpCode(0xED, 0x4F),
      OpCode(0x3E), OpCode(0x06), OpCode(0x0E), OpCode(0x16),
      OpCode(0x1E), OpCode(0x26), OpCode(0x2E), OpCode(0x36)) -> 2,
    List(OpCode(0xDD, 0x77), OpCode(0xFD, 0x77), OpCode(0x32), OpCode(0xDD, 0x70), OpCode(0xFD, 0x70),
      OpCode(0xDD, 0x71), OpCode(0xFD, 0x71), OpCode(0xDD, 0x72), OpCode(0xFD, 0x72),
      OpCode(0xDD, 0x73), OpCode(0xFD, 0x73), OpCode(0xDD, 0x74), OpCode(0xFD, 0x74),
      OpCode(0xDD, 0x75), OpCode(0xFD, 0x75),
      OpCode(0xDD, 0x7E), OpCode(0xDD, 0x46), OpCode(0xDD, 0x4E), OpCode(0xDD, 0x56), OpCode(0xDD, 0x5E),
      OpCode(0xDD, 66), OpCode(0xDD, 0x6E),
      OpCode(0xFD, 0x7E), OpCode(0xFD, 0x46), OpCode(0xFD, 0x4E), OpCode(0xFD, 0x56), OpCode(0xFD, 0x5E),
      OpCode(0xFD, 66), OpCode(0xFD, 0x6E), OpCode(0x3A)) -> 3,
    List(OpCode(0xDD, 0x36), OpCode(0xFD, 0x36)) -> 4
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
