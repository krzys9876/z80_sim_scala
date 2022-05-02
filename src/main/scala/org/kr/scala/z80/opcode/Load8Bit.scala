package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{SystemChangeBase, Z80System}

object Load8Bit extends LoadSpec with OpCodeHandler {
  // Z80 manual page 42
  val sourceLocListMap: Map[List[OpCode], Location] = Map(
    // registers
    List(OpCode(0xED, 0x57)) -> Location.register("I"),
    List(OpCode(0xED, 0x5F)) -> Location.register("R"),
    List(OpCode(0x02), OpCode(0x12),
      OpCode(0x32), OpCode(0xED, 0x47), OpCode(0xED, 0x4F)) -> Location.register("A"),
    // indirect registers
    List(OpCode(0x7E), OpCode(0x46), OpCode(0x4E), OpCode(0x56),
      OpCode(0x5E), OpCode(0x66), OpCode(0x6E)) -> Location.registerAddr("HL"),
    List(OpCode(0x0A)) -> Location.registerAddr("BC"),
    List(OpCode(0x1A)) -> Location.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD, 0x7E), OpCode(0xDD, 0x46), OpCode(0xDD, 0x4E), OpCode(0xDD, 0x56), OpCode(0xDD, 0x5E),
      OpCode(0xDD, 66), OpCode(0xDD, 0x6E)) -> Location.registerAddrIndirOffset("IX", 2),
    List(OpCode(0xFD, 0x7E), OpCode(0xFD, 0x46), OpCode(0xFD, 0x4E), OpCode(0xFD, 0x56), OpCode(0xFD, 0x5E),
      OpCode(0xFD, 66), OpCode(0xFD, 0x6E)) -> Location.registerAddrIndirOffset("IY", 2),
    // immediate address
    List(OpCode(0x3A)) -> Location.indirAddress(1),
    // immediate
    List(OpCode(0x3E), OpCode(0x06), OpCode(0x0E), OpCode(0x16),
      OpCode(0x1E), OpCode(0x26), OpCode(0x2E),
      OpCode(0x36)) -> Location.registerAddrDirOffset("PC", 1),
    List(OpCode(0xDD, 0x36), OpCode(0xFD, 0x36)) -> Location.registerAddrDirOffset("PC", 3)
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

  override val sourceLoc: OpCodeMap[Location] = new OpCodeMap(sourceLocListMap, Location.empty)

  val destLocListMap: Map[List[OpCode], Location] = Map(
    // registers
    List(OpCode(0xED, 0x57), OpCode(0xED, 0x5F),
      OpCode(0x7E), OpCode(0x7F), OpCode(0x0A), OpCode(0x1A), OpCode(0xDD, 0x7E),
      OpCode(0xFD, 0x7E), OpCode(0x3A), OpCode(0x3E)) -> Location.register("A"),
    List(OpCode(0x46), OpCode(0xDD, 0x46), OpCode(0xFD, 0x46), OpCode(0x06)) -> Location.register("B"),
    List(OpCode(0x4E), OpCode(0xDD, 0x4E), OpCode(0xFD, 0x4E), OpCode(0x0E)) -> Location.register("C"),
    List(OpCode(0x56), OpCode(0xDD, 0x56), OpCode(0xFD, 0x56), OpCode(0x16)) -> Location.register("D"),
    List(OpCode(0x5E), OpCode(0xDD, 0x5E), OpCode(0xFD, 0x5E), OpCode(0x1E)) -> Location.register("E"),
    List(OpCode(0x66), OpCode(0xDD, 0x66), OpCode(0xFD, 0x66), OpCode(0x26)) -> Location.register("H"),
    List(OpCode(0x6E), OpCode(0xDD, 0x6E), OpCode(0xFD, 0x6E), OpCode(0x2E)) -> Location.register("L"),
    List(OpCode(0xED, 0x47)) -> Location.register("I"),
    List(OpCode(0xED, 0x4F)) -> Location.register("R"),
    // indirect registers
    List(OpCode(0x77), OpCode(0x70), OpCode(0x71), OpCode(0x72),
      OpCode(0x73), OpCode(0x74), OpCode(0x75), OpCode(0x36))
      -> Location.registerAddr("HL"),
    List(OpCode(0x02)) -> Location.registerAddr("BC"),
    List(OpCode(0x12)) -> Location.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD, 0x77), OpCode(0xDD, 0x70), OpCode(0xDD, 0x71), OpCode(0xDD, 0x72), OpCode(0xDD, 0x73),
      OpCode(0xDD, 74), OpCode(0xDD, 0x75), OpCode(0xDD, 0x36)) -> Location.registerAddrIndirOffset("IX", 2),
    List(OpCode(0xFD, 0x77), OpCode(0xFD, 0x70), OpCode(0xFD, 0x71), OpCode(0xFD, 0x72), OpCode(0xFD, 0x73),
      OpCode(0xFD, 74), OpCode(0xFD, 0x75), OpCode(0xFD, 0x36)) -> Location.registerAddrIndirOffset("IY", 2),
    // immediate address
    List(OpCode(0x32)) -> Location.indirAddress(1)
  )++
  //main registers (A-L)
    OpCode.generateMapByReg(OpCode(0x47),1,3)++
    OpCode.generateMapByReg(OpCode(0x40),1,3)++
    OpCode.generateMapByReg(OpCode(0x41),1,3)++
    OpCode.generateMapByReg(OpCode(0x42),1,3)++
    OpCode.generateMapByReg(OpCode(0x43),1,3)++
    OpCode.generateMapByReg(OpCode(0x44),1,3)++
    OpCode.generateMapByReg(OpCode(0x45),1,3)


  override val destLoc: OpCodeMap[Location] = new OpCodeMap(destLocListMap, Location.empty)

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

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val value=system.getValueFromLocation(sourceLoc.find(code))
    (List(system.putValueToLocation(destLoc.find(code),value)),instSize.find(code))
  }
}
