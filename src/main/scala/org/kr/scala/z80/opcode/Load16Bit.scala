package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{RegisterChangeRelative, SystemChangeBase, Z80System}

object Load16Bit extends LoadSpec with OpCodeHandler {
  // Z80 manual page 45 (NOTE: PUSH qq are X5, not X6!)
  val sourceLocListMap: Map[List[OpCode], LoadLocation] = Map(
    //registers
    List(OpCode(0xF5)) -> LoadLocation.register("AF"),
    List(OpCode(0xED, 0x43), OpCode(0xC5)) -> LoadLocation.register("BC"),
    List(OpCode(0xED, 0x53), OpCode(0xD5)) -> LoadLocation.register("DE"),
    List(OpCode(0xF9), OpCode(0x22), OpCode(0xE5)) -> LoadLocation.register("HL"),
    List(OpCode(0xED, 0x73)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD, 0xF9), OpCode(0xDD, 0x22), OpCode(0xDD, 0xE5)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD, 0xF9), OpCode(0xFD, 0x22), OpCode(0xFD, 0xE5)) -> LoadLocation.register("IY"),
    // indirect register
    List(OpCode(0xF1), OpCode(0xC1), OpCode(0xD1), OpCode(0xE1),
      OpCode(0xDD, 0xE1), OpCode(0xFD, 0xE1)) -> LoadLocation.registerAddr("SP",isWord = true),
    // immediate
    List(OpCode(0x01), OpCode(0x11), OpCode(0x21),
      OpCode(0x31)) -> LoadLocation.registerAddrDirOffset("PC", 1, isWord = true),
    List(OpCode(0xDD, 0x21), OpCode(0xFD, 0x21)) -> LoadLocation.registerAddrDirOffset("PC", 2,isWord = true),
    // immediate address
    List(OpCode(0x2A)) -> LoadLocation.indirAddress(1,isWord = true),
    List(OpCode(0xED, 0x4B), OpCode(0xED, 0x5B), OpCode(0xED, 0x7B),
      OpCode(0xDD, 0x2A), OpCode(0xFD, 0x2A)) -> LoadLocation.indirAddress(2,isWord = true)
  )

  override val sourceLoc: OpCodeMap[LoadLocation] = new OpCodeMap(sourceLocListMap, LoadLocation.empty)

  val destLocListMap: Map[List[OpCode], LoadLocation] = Map(
    // register pair
    List(OpCode(0xF1)) -> LoadLocation.register("AF"),
    List(OpCode(0x01), OpCode(0xED, 0x4B), OpCode(0xC1)) -> LoadLocation.register("BC"),
    List(OpCode(0x11), OpCode(0xED, 0x5B), OpCode(0xD1)) -> LoadLocation.register("DE"),
    List(OpCode(0x21), OpCode(0x2A), OpCode(0xE1)) -> LoadLocation.register("HL"),
    List(OpCode(0x31), OpCode(0xED, 0x7B), OpCode(0xF9),
      OpCode(0xDD, 0xF9), OpCode(0xFD, 0xF9)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD, 0x21), OpCode(0xDD, 0x2A), OpCode(0xDD, 0xE1)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD, 0x21), OpCode(0xFD, 0x2A), OpCode(0xFD, 0xE1)) -> LoadLocation.register("IY"),
    // indirect register
    List(OpCode(0xF5), OpCode(0xC5), OpCode(0xD5), OpCode(0xE5),
      OpCode(0xDD, 0xE5), OpCode(0xFD, 0xE5)) -> LoadLocation.registerAddrDirOffset("SP", -2, isWord = true),
    // immediate address
    List(OpCode(0xED, 0x43), OpCode(0xED, 0x53), OpCode(0xED, 0x73), OpCode(0xDD, 0x22),
      OpCode(0xFD, 0x22)) -> LoadLocation.indirAddress(2,isWord = true),
    List(OpCode(0x22)) -> LoadLocation.indirAddress(1,isWord = true)
  )

  override val destLoc: OpCodeMap[LoadLocation] = new OpCodeMap(destLocListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xF9), OpCode(0xF1), OpCode(0xC1),
      OpCode(0xD1), OpCode(0xE1),
      OpCode(0xF5), OpCode(0xC5), OpCode(0xD5), OpCode(0xE5)) -> 1,
    List(OpCode(0xDD, 0xF9), OpCode(0xFD, 0xF9), OpCode(0xDD, 0xE1), OpCode(0xFD, 0xE1),
      OpCode(0xDD, 0xE5), OpCode(0xFD, 0xE5)) -> 2,
    List(OpCode(0x01), OpCode(0x11), OpCode(0x21), OpCode(0x31),
      OpCode(0x2A), OpCode(0x22)) -> 3,
    List(OpCode(0xDD, 0x21), OpCode(0xFD, 0x21), OpCode(0xED, 0x4B), OpCode(0xED, 0x5B),
      OpCode(0xED, 0x7B), OpCode(0xDD, 0x2A), OpCode(0xFD, 0x2A),
      OpCode(0xED, 0x43), OpCode(0xED, 0x53), OpCode(0xED, 0x73),
      OpCode(0xDD, 0x22), OpCode(0xFD, 0x22)) -> 4
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  val stackChangeListMap: Map[List[OpCode], Int] = Map(
    // POP (SP+2)
    List(OpCode(0xF1), OpCode(0xC1), OpCode(0xD1), OpCode(0xE1),
      OpCode(0xDD, 0xE1), OpCode(0xFD, 0xE1)) -> 2,
    // PUSH (SP-2)
    List(OpCode(0xF5), OpCode(0xC5), OpCode(0xD5), OpCode(0xE5),
      OpCode(0xDD, 0xE5), OpCode(0xFD, 0xE5)) -> -2
  )

  val stackChange: OpCodeMap[Int] = new OpCodeMap(stackChangeListMap, 0)

  override def handle(code:OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val sourceLoc=Load16Bit.sourceLoc.find(code)
    val value=system.getValueFromLocation(sourceLoc)
    val destLoc=Load16Bit.destLoc.find(code)
    val instrSize=Load16Bit.instSize.find(code)
    val stackChange=Load16Bit.stackChange.find(code)

    val chgList= List(system.putValueToLocation(destLoc,value,isWord = true))
    val stackChgList=destLoc match {
      case LoadLocation(r,_,_,rd,dirO,_,_) if r!="" || (rd!="" && dirO!=OpCode.ANY) =>
        List(new RegisterChangeRelative("SP",stackChange))
      case _ => List()
    }
    (chgList++stackChgList,instrSize)
  }

}
