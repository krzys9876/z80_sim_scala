package org.kr.scala.z80.opcode

object Load8Bit extends LoadSpec {
  // Z80 manual page 42
  val sourceLocListMap: Map[List[OpCode], LoadLocation] = Map(
    // registers
    List(OpCode(0xED, 0x57)) -> LoadLocation.register("I"),
    List(OpCode(0xED, 0x5F)) -> LoadLocation.register("R"),
    List(OpCode(0x02), OpCode(0x12),
      OpCode(0x32), OpCode(0xED, 0x47), OpCode(0xED, 0x4F)) -> LoadLocation.register("A"),
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
  )++
  //main registers (A-L)
    OpCode.generateMapByReg(OpCode(0x40),1,0)++
    OpCode.generateMapByReg(OpCode(0x48),1,0)++
    OpCode.generateMapByReg(OpCode(0x50),1,0)++
    OpCode.generateMapByReg(OpCode(0x58),1,0)++
    OpCode.generateMapByReg(OpCode(0x60),1,0)++
    OpCode.generateMapByReg(OpCode(0x68),1,0)++
    OpCode.generateMapByReg(OpCode(0x70),1,0)++
    OpCode.generateMapByReg(OpCode(0x78),1,0)++
    OpCode.generateMapByReg(OpCode(0xDD,0x70),2,0)++
    OpCode.generateMapByReg(OpCode(0xFD,0x70),2,0)

  override val sourceLoc: OpCodeMap[LoadLocation] = new OpCodeMap(sourceLocListMap, LoadLocation.empty)

  val destLocListMap: Map[List[OpCode], LoadLocation] = Map(
    // registers
    List(OpCode(0xED, 0x57), OpCode(0xED, 0x5F),
      OpCode(0x7E), OpCode(0x7F), OpCode(0x0A), OpCode(0x1A), OpCode(0xDD, 0x7E),
      OpCode(0xFD, 0x7E), OpCode(0x3A), OpCode(0x3E)) -> LoadLocation.register("A"),
    List(OpCode(0x46), OpCode(0xDD, 0x46), OpCode(0xFD, 0x46), OpCode(0x06)) -> LoadLocation.register("B"),
    List(OpCode(0x4E), OpCode(0xDD, 0x4E), OpCode(0xFD, 0x4E), OpCode(0x0E)) -> LoadLocation.register("C"),
    List(OpCode(0x56), OpCode(0xDD, 0x56), OpCode(0xFD, 0x56), OpCode(0x16)) -> LoadLocation.register("D"),
    List(OpCode(0x5E), OpCode(0xDD, 0x5E), OpCode(0xFD, 0x5E), OpCode(0x1E)) -> LoadLocation.register("E"),
    List(OpCode(0x66), OpCode(0xDD, 0x66), OpCode(0xFD, 0x66), OpCode(0x26)) -> LoadLocation.register("H"),
    List(OpCode(0x6E), OpCode(0xDD, 0x6E), OpCode(0xFD, 0x6E), OpCode(0x2E)) -> LoadLocation.register("L"),
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
  )++
  //main registers (A-L)
    OpCode.generateMapByReg(OpCode(0x47),1,3)++
    OpCode.generateMapByReg(OpCode(0x40),1,3)++
    OpCode.generateMapByReg(OpCode(0x41),1,3)++
    OpCode.generateMapByReg(OpCode(0x42),1,3)++
    OpCode.generateMapByReg(OpCode(0x43),1,3)++
    OpCode.generateMapByReg(OpCode(0x44),1,3)++
    OpCode.generateMapByReg(OpCode(0x45),1,3)


  override val destLoc: OpCodeMap[LoadLocation] = new OpCodeMap(destLocListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x02), OpCode(0x12),
      OpCode(0x0A), OpCode(0x1A),OpCode(0x7E), OpCode(0x46), OpCode(0x4E), OpCode(0x56),
      OpCode(0x5E), OpCode(0x66), OpCode(0x6E),OpCode(0x1A)) -> 1,
      OpCode.generateListByReg(OpCode(0x40),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0x48),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0x50),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0x58),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0x60),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0x68),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0x70),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0x78),1,0) -> 1,
      OpCode.generateListByReg(OpCode(0xDD,0x70),2,0) ->3,
      OpCode.generateListByReg(OpCode(0xFD,0x70),2,0) ->3,
      List(OpCode(0xED, 0x57), OpCode(0xED, 0x5F), OpCode(0xED, 0x47), OpCode(0xED, 0x4F),
      OpCode(0x3E), OpCode(0x06), OpCode(0x0E), OpCode(0x16),
      OpCode(0x1E), OpCode(0x26), OpCode(0x2E), OpCode(0x36)) -> 2,
    List(OpCode(0x32),OpCode(0xDD, 0x7E), OpCode(0xDD, 0x46), OpCode(0xDD, 0x4E), OpCode(0xDD, 0x56),
      OpCode(0xDD, 0x5E),OpCode(0xDD, 66), OpCode(0xDD, 0x6E),
      OpCode(0xFD, 0x7E), OpCode(0xFD, 0x46), OpCode(0xFD, 0x4E), OpCode(0xFD, 0x56), OpCode(0xFD, 0x5E),
      OpCode(0xFD, 66), OpCode(0xFD, 0x6E), OpCode(0x3A)) -> 3,
    List(OpCode(0xDD, 0x36), OpCode(0xFD, 0x36)) -> 4
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
