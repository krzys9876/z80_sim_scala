package org.kr.scala.z80.opcode

object RotateShift extends OperationSpec {
  // Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
  val operationListMap: Map[List[OpCode],ArithmeticOperation] = Map(
    List(OpCode(0xCB,0x07),OpCode(0xCB,0x00),OpCode(0xCB,0x01),OpCode(0xCB,0x02),OpCode(0xCB,0x03),
      OpCode(0xCB,0x04),OpCode(0xCB,0x05),OpCode(0xCB,0x06),OpCode(0xDD,0xCB,0x06),
      OpCode(0xFD,0xCB,0x06)) -> ArithmeticOpType.Rlc,
    List(OpCode(0x07)) -> ArithmeticOpType.Rlca,
    List(OpCode(0xCB,0x0F),OpCode(0xCB,0x08),OpCode(0xCB,0x09),OpCode(0xCB,0x0A),OpCode(0xCB,0x0B),
      OpCode(0xCB,0x0C),OpCode(0xCB,0x0D),OpCode(0xCB,0x0E),OpCode(0xDD,0xCB,0x0E),
      OpCode(0xFD,0xCB,0x0E)) -> ArithmeticOpType.Rrc,
    List(OpCode(0x0F)) -> ArithmeticOpType.Rrca,
    List(OpCode(0xCB,0x17),OpCode(0xCB,0x10),OpCode(0xCB,0x11),OpCode(0xCB,0x12),OpCode(0xCB,0x13),
      OpCode(0xCB,0x14),OpCode(0xCB,0x15),OpCode(0xCB,0x16),OpCode(0xDD,0xCB,0x16),
      OpCode(0xFD,0xCB,0x16)) -> ArithmeticOpType.Rl,
    List(OpCode(0x17)) -> ArithmeticOpType.Rla,
    List(OpCode(0xCB,0x1F),OpCode(0xCB,0x18),OpCode(0xCB,0x19),OpCode(0xCB,0x1A),OpCode(0xCB,0x1B),
      OpCode(0xCB,0x1C),OpCode(0xCB,0x1D),OpCode(0xCB,0x1E),OpCode(0xDD,0xCB,0x1E),
      OpCode(0xFD,0xCB,0x1E)) -> ArithmeticOpType.Rr,
    List(OpCode(0x1F)) -> ArithmeticOpType.Rra,
    List(OpCode(0xCB,0x27),OpCode(0xCB,0x20),OpCode(0xCB,0x21),OpCode(0xCB,0x22),OpCode(0xCB,0x23),
      OpCode(0xCB,0x24),OpCode(0xCB,0x25),OpCode(0xCB,0x26),OpCode(0xDD,0xCB,0x26),
      OpCode(0xFD,0xCB,0x26)) -> ArithmeticOpType.Sla,
    List(OpCode(0xCB,0x2F),OpCode(0xCB,0x28),OpCode(0xCB,0x29),OpCode(0xCB,0x2A),OpCode(0xCB,0x2B),
      OpCode(0xCB,0x2C),OpCode(0xCB,0x2D),OpCode(0xCB,0x2E),OpCode(0xDD,0xCB,0x2E),
      OpCode(0xFD,0xCB,0x2E)) -> ArithmeticOpType.Sra,
    List(OpCode(0xCB,0x3F),OpCode(0xCB,0x38),OpCode(0xCB,0x39),OpCode(0xCB,0x3A),OpCode(0xCB,0x3B),
      OpCode(0xCB,0x3C),OpCode(0xCB,0x3D),OpCode(0xCB,0x3E),OpCode(0xDD,0xCB,0x3E),
      OpCode(0xFD,0xCB,0x3E)) -> ArithmeticOpType.Srl
  )

  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(operationListMap, ArithmeticOpType.None)

  val locationListMap: Map[List[OpCode], LoadLocation] = Map(
    List(OpCode(0xCB,0x07),OpCode(0xCB,0x0F),OpCode(0xCB,0x17),OpCode(0xCB,0x1F),
      OpCode(0xCB,0x27),OpCode(0xCB,0x2F),OpCode(0xCB,0x3F),
      OpCode(0x07),OpCode(0x0F),OpCode(0x17),OpCode(0x1F)) -> LoadLocation.register("A"),
    List(OpCode(0xCB,0x00),OpCode(0xCB,0x08),OpCode(0xCB,0x10),OpCode(0xCB,0x18),
      OpCode(0xCB,0x20),OpCode(0xCB,0x28),OpCode(0xCB,0x38)) -> LoadLocation.register("B"),
    List(OpCode(0xCB,0x01),OpCode(0xCB,0x09),OpCode(0xCB,0x11),OpCode(0xCB,0x19),
      OpCode(0xCB,0x21),OpCode(0xCB,0x29),OpCode(0xCB,0x39)) -> LoadLocation.register("C"),
    List(OpCode(0xCB,0x02),OpCode(0xCB,0x0A),OpCode(0xCB,0x12),OpCode(0xCB,0x1A),
      OpCode(0xCB,0x22),OpCode(0xCB,0x2A),OpCode(0xCB,0x3A)) -> LoadLocation.register("D"),
    List(OpCode(0xCB,0x03),OpCode(0xCB,0x0B),OpCode(0xCB,0x13),OpCode(0xCB,0x1B),
      OpCode(0xCB,0x23),OpCode(0xCB,0x2B),OpCode(0xCB,0x3B)) -> LoadLocation.register("E"),
    List(OpCode(0xCB,0x04),OpCode(0xCB,0x0C),OpCode(0xCB,0x14),OpCode(0xCB,0x1C),
      OpCode(0xCB,0x24),OpCode(0xCB,0x2C),OpCode(0xCB,0x3C)) -> LoadLocation.register("H"),
    List(OpCode(0xCB,0x05),OpCode(0xCB,0x0D),OpCode(0xCB,0x15),OpCode(0xCB,0x1D),
      OpCode(0xCB,0x25),OpCode(0xCB,0x2D),OpCode(0xCB,0x3D)) -> LoadLocation.register("L"),
    List(OpCode(0xCB,0x06),OpCode(0xCB,0x0E),OpCode(0xCB,0x16),OpCode(0xCB,0x1E),
      OpCode(0xCB,0x26),OpCode(0xCB,0x2E),OpCode(0xCB,0x3E)) -> LoadLocation.registerAddr("HL"),
    List(OpCode(0xDD,0xCB,0x06),OpCode(0xDD,0xCB,0x0E),OpCode(0xDD,0xCB,0x16),OpCode(0xDD,0xCB,0x1E),
      OpCode(0xDD,0xCB,0x26),OpCode(0xDD,0xCB,0x2E),OpCode(0xDD,0xCB,0x3E)
    ) -> LoadLocation.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD,0xCB,0x06),OpCode(0xFD,0xCB,0x0E),OpCode(0xFD,0xCB,0x16),OpCode(0xFD,0xCB,0x1E),
      OpCode(0xFD,0xCB,0x26),OpCode(0xFD,0xCB,0x2E),OpCode(0xFD,0xCB,0x3E)
    ) -> LoadLocation.registerAddrIndirOffset("IY",2)
  )

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x07),OpCode(0x0F),OpCode(0x17),OpCode(0x1F)) ->1,
    List(OpCode(0xCB,0x07),OpCode(0xCB,0x00),OpCode(0xCB,0x01),OpCode(0xCB,0x02),OpCode(0xCB,0x03),
      OpCode(0xCB,0x04),OpCode(0xCB,0x05),OpCode(0xCB,0x06),
      OpCode(0xCB,0x0F),OpCode(0xCB,0x08),OpCode(0xCB,0x09),OpCode(0xCB,0x0A),OpCode(0xCB,0x0B),
      OpCode(0xCB,0x0C),OpCode(0xCB,0x0D),OpCode(0xCB,0x0E),
      OpCode(0xCB,0x17),OpCode(0xCB,0x10),OpCode(0xCB,0x11),OpCode(0xCB,0x12),OpCode(0xCB,0x13),
      OpCode(0xCB,0x14),OpCode(0xCB,0x15),OpCode(0xCB,0x16),
      OpCode(0xCB,0x1F),OpCode(0xCB,0x18),OpCode(0xCB,0x19),OpCode(0xCB,0x1A),OpCode(0xCB,0x1B),
      OpCode(0xCB,0x1C),OpCode(0xCB,0x1D),OpCode(0xCB,0x1E),
      OpCode(0xCB,0x27),OpCode(0xCB,0x20),OpCode(0xCB,0x21),OpCode(0xCB,0x22),OpCode(0xCB,0x23),
      OpCode(0xCB,0x24),OpCode(0xCB,0x25),OpCode(0xCB,0x26),
      OpCode(0xCB,0x2F),OpCode(0xCB,0x28),OpCode(0xCB,0x29),OpCode(0xCB,0x2A),OpCode(0xCB,0x2B),
      OpCode(0xCB,0x2C),OpCode(0xCB,0x2D),OpCode(0xCB,0x2E),
      OpCode(0xCB,0x3F),OpCode(0xCB,0x38),OpCode(0xCB,0x39),OpCode(0xCB,0x3A),OpCode(0xCB,0x3B),
      OpCode(0xCB,0x3C),OpCode(0xCB,0x3D),OpCode(0xCB,0x3E)) ->2,
    List(OpCode(0xDD,0xCB,0x06),OpCode(0xFD,0xCB,0x06),OpCode(0xDD,0xCB,0x0E),OpCode(0xFD,0xCB,0x0E),
      OpCode(0xDD,0xCB,0x16),OpCode(0xFD,0xCB,0x16),OpCode(0xDD,0xCB,0x1E),OpCode(0xFD,0xCB,0x1E),
      OpCode(0xDD,0xCB,0x26),OpCode(0xFD,0xCB,0x26),OpCode(0xDD,0xCB,0x2E),OpCode(0xFD,0xCB,0x2E),
      OpCode(0xDD,0xCB,0x3E),OpCode(0xFD,0xCB,0x3E)) ->4
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}

object RotateDigit extends OperationSpec {
  // Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
  val operationListMap: Map[List[OpCode],ArithmeticOperation] = Map(
    List(OpCode(0xED,0x6F)) -> ArithmeticOpType.Rld,
    List(OpCode(0xED,0x67)) -> ArithmeticOpType.Rrd
  )

  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(operationListMap, ArithmeticOpType.None)

  val locationListMap: Map[List[OpCode], LoadLocation] = Map(
    List(OpCode(0xED,0x6F),OpCode(0xED,0x67)) -> LoadLocation.registerAddr("HL")
  )

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xED,0x6F),OpCode(0xED,0x67)) ->2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
