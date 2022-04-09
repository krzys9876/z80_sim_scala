package org.kr.scala.z80.opcode

object Arithmetic8Bit extends OperationSpec {
  // Z80 manual page 50 (NOTE: ADD A,(HL) is 0x86, not 0x88!
  val operationListMap: Map[List[OpCode],AritheticOpLocationBase] = Map(
    List(OpCode(0x87),OpCode(0x80),OpCode(0x81),OpCode(0x82),OpCode(0x83),OpCode(0x84),OpCode(0x85),
      OpCode(0x86),OpCode(0xDD, 0x86),OpCode(0xFD, 0x86),OpCode(0xC6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Add),
    List(OpCode(0x8F),OpCode(0x88),OpCode(0x89),OpCode(0x8A),OpCode(0x8B),OpCode(0x8C),OpCode(0x8D),
      OpCode(0x8E),OpCode(0xDD, 0x8E),OpCode(0xFD, 0x8E),OpCode(0xCE)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.AddC),
    List(OpCode(0x97),OpCode(0x90),OpCode(0x91),OpCode(0x92),OpCode(0x93),OpCode(0x94),OpCode(0x95),
      OpCode(0x96),OpCode(0xDD, 0x96),OpCode(0xFD, 0x96),OpCode(0xD6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Sub),
    List(OpCode(0x9F),OpCode(0x98),OpCode(0x99),OpCode(0x9A),OpCode(0x9B),OpCode(0x9C),OpCode(0x9D),
      OpCode(0x9E),OpCode(0xDD, 0x9E),OpCode(0xFD, 0x9E),OpCode(0xDE)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.SubC),
    List(OpCode(0xA7),OpCode(0xA0),OpCode(0xA1),OpCode(0xA2),OpCode(0xA3),OpCode(0xA4),OpCode(0xA5),
      OpCode(0xA6),OpCode(0xDD, 0xA6),OpCode(0xFD, 0xA6),OpCode(0xE6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.And),
    List(OpCode(0xAF),OpCode(0xA8),OpCode(0xA9),OpCode(0xAA),OpCode(0xAB),OpCode(0xAC),OpCode(0xAD),
      OpCode(0xAE),OpCode(0xDD, 0xAE),OpCode(0xFD, 0xAE),OpCode(0xEE)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Xor),
    List(OpCode(0xB7),OpCode(0xB0),OpCode(0xB1),OpCode(0xB2),OpCode(0xB3),OpCode(0xB4),OpCode(0xB5),
      OpCode(0xB6),OpCode(0xDD, 0xB6),OpCode(0xFD, 0xB6),OpCode(0xF6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Or),
    List(OpCode(0xBF),OpCode(0xB8),OpCode(0xB9),OpCode(0xBA),OpCode(0xBB),OpCode(0xBC),OpCode(0xBD),
      OpCode(0xBE),OpCode(0xDD, 0xBE),OpCode(0xFD, 0xBE),OpCode(0xFE)) -> new ArithmeticOpLocationFlags(ArithmeticOpType.Comp),
    List(OpCode(0x3C),OpCode(0x04),OpCode(0x0C),OpCode(0x14),OpCode(0x1C),OpCode(0x24),OpCode(0x2C),
      OpCode(0x34),OpCode(0xDD, 0x34),OpCode(0xFD, 0x34)) -> new ArithmeticOpVariableLocation(ArithmeticOpType.Inc),
    List(OpCode(0x3D),OpCode(0x05),OpCode(0x0D),OpCode(0x15),OpCode(0x1D),OpCode(0x25),OpCode(0x2D),
      OpCode(0x35),OpCode(0xDD, 0x35),OpCode(0xFD, 0x35)) -> new ArithmeticOpVariableLocation(ArithmeticOpType.Dec)
  )

  val operation: OpCodeMap[AritheticOpLocationBase] = new OpCodeMap(operationListMap, AritheticOpLocationBase.empty)

  val operandListMap: Map[List[OpCode],LoadLocation] = Map(
    //indirect register
    List(OpCode(0x86),OpCode(0x8E),OpCode(0x96),OpCode(0x9E),
      OpCode(0xA6),OpCode(0xAE),OpCode(0xB6),OpCode(0xBE),
      OpCode(0x34),OpCode(0x35)
    ) -> LoadLocation.registerAddr("HL"),
    //indirect registers with offset
    List(OpCode(0xDD, 0x86),OpCode(0xDD, 0x8E),OpCode(0xDD, 0x96),OpCode(0xDD, 0x9E),
      OpCode(0xDD, 0xA6),OpCode(0xDD, 0xAE),OpCode(0xDD, 0xB6),OpCode(0xDD, 0xBE),
      OpCode(0xDD, 0x34),OpCode(0xDD, 0x35)
    ) -> LoadLocation.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD, 0x86),OpCode(0xFD, 0x8E),OpCode(0xFD, 0x96),OpCode(0xFD, 0x9E),
      OpCode(0xFD, 0xA6),OpCode(0xFD, 0xAE),OpCode(0xFD, 0xB6),OpCode(0xFD, 0xBE),
      OpCode(0xFD, 0x34),OpCode(0xFD, 0x35)
    ) -> LoadLocation.registerAddrIndirOffset("IY",2),
    // immediate
    List(OpCode(0xC6), OpCode(0xCE), OpCode(0xD6), OpCode(0xDE),
      OpCode(0xE6), OpCode(0xEE), OpCode(0xF6),
      OpCode(0xFE)) -> LoadLocation.registerAddrDirOffset("PC", 1)
  )++
    //register
    OpCode.opCodeGenReg(OpCode(0x80),1,0)++
    OpCode.opCodeGenReg(OpCode(0x88),1,0)++
    OpCode.opCodeGenReg(OpCode(0x90),1,0)++
    OpCode.opCodeGenReg(OpCode(0x98),1,0)++
    OpCode.opCodeGenReg(OpCode(0xA0),1,0)++
    OpCode.opCodeGenReg(OpCode(0xA8),1,0)++
    OpCode.opCodeGenReg(OpCode(0xB0),1,0)++
    OpCode.opCodeGenReg(OpCode(0xB8),1,0)++
    OpCode.opCodeGenReg(OpCode(0x04),1,3)++
    OpCode.opCodeGenReg(OpCode(0x05),1,3)

  val operand: OpCodeMap[LoadLocation] = new OpCodeMap(operandListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x86),OpCode(0x8E),OpCode(0x96),OpCode(0x9E),OpCode(0xA6),OpCode(0xAE),OpCode(0xB6),OpCode(0xBE),
      OpCode(0x34),OpCode(0x35)) -> 1,
    List(OpCode(0xC6),OpCode(0xCE),OpCode(0xD6),OpCode(0xDE),OpCode(0xE6),OpCode(0xEE),OpCode(0xF6),OpCode(0xFE),
      OpCode(0xCE),OpCode(0xDE),OpCode(0xE6),OpCode(0xEE),OpCode(0xF6),OpCode(0xFE)) -> 2,
    List(OpCode(0xDD, 0x86),OpCode(0xDD, 0x8E),OpCode(0xDD, 0x96),OpCode(0xDD, 0x9E),OpCode(0xDD, 0xA6),
      OpCode(0xDD, 0xAE),OpCode(0xDD, 0xB6),OpCode(0xDD, 0xBE),OpCode(0xDD, 0x34),OpCode(0xDD, 0x35),
      OpCode(0xFD, 0x86),OpCode(0xFD, 0x8E),OpCode(0xFD, 0x96),OpCode(0xFD, 0x9E),
      OpCode(0xFD, 0xA6),OpCode(0xFD, 0xAE),OpCode(0xFD, 0xB6),OpCode(0xFD, 0xBE),
      OpCode(0xFD, 0x34),OpCode(0xFD, 0x35)) -> 3,
      OpCode.opCodeGen(OpCode(0x80),1,0)->1,
      OpCode.opCodeGen(OpCode(0x88),1,0)->1,
      OpCode.opCodeGen(OpCode(0x90),1,0)->1,
      OpCode.opCodeGen(OpCode(0x98),1,0)->1,
      OpCode.opCodeGen(OpCode(0xA0),1,0)->1,
      OpCode.opCodeGen(OpCode(0xA8),1,0)->1,
      OpCode.opCodeGen(OpCode(0xB0),1,0)->1,
      OpCode.opCodeGen(OpCode(0xB8),1,0)->1,
      OpCode.opCodeGen(OpCode(0x04),1,3)->1,
      OpCode.opCodeGen(OpCode(0x05),1,3)->1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

}
