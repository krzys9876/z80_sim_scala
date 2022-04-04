package org.kr.scala.z80.opcode

sealed abstract class ArithmeticOperation(val name:String)

object Arith8Bit {
  case object Add extends ArithmeticOperation("ADD")
  case object Sub extends ArithmeticOperation("SUB")
  case object And extends ArithmeticOperation("AND")
  case object Or extends ArithmeticOperation("OR")
  case object Xor extends ArithmeticOperation("XOR")
  case object Comp extends ArithmeticOperation("CP")
  case object Inc extends ArithmeticOperation("INC")
  case object Dec extends ArithmeticOperation("DEC")
  case object None extends ArithmeticOperation("NONE")
}

class Arith8BitBase (val operation:ArithmeticOperation)
class Arith8BitAccum(override val operation:ArithmeticOperation) extends Arith8BitBase(operation)
//class Arith8BitAccumRegIndirect(override val operation:ArithmeticOperation, val operandReg:String) extends Arith8BitBase(operation)

object Arith8BitBase {
  val empty:Arith8BitBase=new Arith8BitBase(Arith8Bit.None)
}

object Arithmetic8Bit extends OperationSpec {
  // Z80 manual page 50
  val arithOperationListMap: Map[List[OpCode],Arith8BitBase] = Map(
    //register
    List(OpCode(0x87, OpCode.ANY),OpCode(0x80, OpCode.ANY),OpCode(0x81, OpCode.ANY),OpCode(0x82, OpCode.ANY),
      OpCode(0x83, OpCode.ANY),OpCode(0x84, OpCode.ANY),OpCode(0x85, OpCode.ANY),
      OpCode(0x86, OpCode.ANY),OpCode(0xDD, 0x86),OpCode(0xFD, 0x86),
      OpCode(0xC6, OpCode.ANY)) -> new Arith8BitAccum(Arith8Bit.Add),
    List(OpCode(0x97, OpCode.ANY),OpCode(0x90, OpCode.ANY),OpCode(0x91, OpCode.ANY),OpCode(0x92, OpCode.ANY),
      OpCode(0x93, OpCode.ANY),OpCode(0x94, OpCode.ANY),OpCode(0x95, OpCode.ANY),
      OpCode(0x96, OpCode.ANY),OpCode(0xDD, 0x96),OpCode(0xFD, 0x96),
      OpCode(0xD6, OpCode.ANY)) -> new Arith8BitAccum(Arith8Bit.Sub)
  )
  val arithOperation: OpCodeMap[Arith8BitBase] = new OpCodeMap(arithOperationListMap, Arith8BitBase.empty)

  val operandListMap: Map[List[OpCode],LoadLocation] = Map(
    //register
    List(OpCode(0x87, OpCode.ANY),OpCode(0x8F, OpCode.ANY),OpCode(0x97, OpCode.ANY),OpCode(0x9F, OpCode.ANY),
      OpCode(0xA7, OpCode.ANY),OpCode(0xAF, OpCode.ANY),OpCode(0xB7, OpCode.ANY),OpCode(0xBF, OpCode.ANY),
      OpCode(0x3C, OpCode.ANY),OpCode(0x3D, OpCode.ANY)
    ) -> LoadLocation.register("A"),
    List(OpCode(0x80, OpCode.ANY),OpCode(0x88, OpCode.ANY),OpCode(0x90, OpCode.ANY),OpCode(0x98, OpCode.ANY),
      OpCode(0xA0, OpCode.ANY),OpCode(0xA8, OpCode.ANY),OpCode(0xB0, OpCode.ANY),OpCode(0xB8, OpCode.ANY),
      OpCode(0x04, OpCode.ANY),OpCode(0x05, OpCode.ANY)
    ) -> LoadLocation.register("B"),
    List(OpCode(0x81, OpCode.ANY),OpCode(0x89, OpCode.ANY),OpCode(0x91, OpCode.ANY),OpCode(0x99, OpCode.ANY),
      OpCode(0xA1, OpCode.ANY),OpCode(0xA9, OpCode.ANY),OpCode(0xB1, OpCode.ANY),OpCode(0xB9, OpCode.ANY),
      OpCode(0x0C, OpCode.ANY),OpCode(0x0D, OpCode.ANY)
    ) -> LoadLocation.register("C"),
    List(OpCode(0x82, OpCode.ANY),OpCode(0x8A, OpCode.ANY),OpCode(0x92, OpCode.ANY),OpCode(0x9A, OpCode.ANY),
      OpCode(0xA2, OpCode.ANY),OpCode(0xAA, OpCode.ANY),OpCode(0xB2, OpCode.ANY),OpCode(0xBA, OpCode.ANY),
      OpCode(0x14, OpCode.ANY),OpCode(0x15, OpCode.ANY)
    ) -> LoadLocation.register("D"),
    List(OpCode(0x83, OpCode.ANY),OpCode(0x8B, OpCode.ANY),OpCode(0x93, OpCode.ANY),OpCode(0x9B, OpCode.ANY),
      OpCode(0xA3, OpCode.ANY),OpCode(0xAB, OpCode.ANY),OpCode(0xB3, OpCode.ANY),OpCode(0xBB, OpCode.ANY),
      OpCode(0x1C, OpCode.ANY),OpCode(0x1D, OpCode.ANY)
    ) -> LoadLocation.register("E"),
    List(OpCode(0x84, OpCode.ANY),OpCode(0x8C, OpCode.ANY),OpCode(0x94, OpCode.ANY),OpCode(0x9C, OpCode.ANY),
      OpCode(0xA4, OpCode.ANY),OpCode(0xAC, OpCode.ANY),OpCode(0xB4, OpCode.ANY),OpCode(0xBC, OpCode.ANY),
      OpCode(0x24, OpCode.ANY),OpCode(0x25, OpCode.ANY)
    ) -> LoadLocation.register("H"),
    List(OpCode(0x85, OpCode.ANY),OpCode(0x8D, OpCode.ANY),OpCode(0x95, OpCode.ANY),OpCode(0x9D, OpCode.ANY),
      OpCode(0xA5, OpCode.ANY),OpCode(0xAD, OpCode.ANY),OpCode(0xB5, OpCode.ANY),OpCode(0xBD, OpCode.ANY),
      OpCode(0x2C, OpCode.ANY),OpCode(0x2D, OpCode.ANY)
    ) -> LoadLocation.register("L"),
    //indirect register
    List(OpCode(0x88, OpCode.ANY),OpCode(0x8E, OpCode.ANY),OpCode(0x96, OpCode.ANY),OpCode(0x9E, OpCode.ANY),
      OpCode(0xA6, OpCode.ANY),OpCode(0xAE, OpCode.ANY),OpCode(0xB6, OpCode.ANY),OpCode(0xBE, OpCode.ANY),
      OpCode(0x34, OpCode.ANY),OpCode(0x35, OpCode.ANY)
    ) -> LoadLocation.registerAddr("HL")
  )

  val operand: OpCodeMap[LoadLocation] = new OpCodeMap(operandListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x87, OpCode.ANY),OpCode(0x80, OpCode.ANY),OpCode(0x81, OpCode.ANY),OpCode(0x82, OpCode.ANY),
      OpCode(0x83, OpCode.ANY),OpCode(0x84, OpCode.ANY),OpCode(0x85, OpCode.ANY),
      OpCode(0x86, OpCode.ANY),OpCode(0xDD, 0x86),OpCode(0xFD, 0x86),OpCode(0xC6, OpCode.ANY),
      OpCode(0x97, OpCode.ANY),OpCode(0x90, OpCode.ANY),OpCode(0x91, OpCode.ANY),OpCode(0x92, OpCode.ANY),
      OpCode(0x93, OpCode.ANY),OpCode(0x94, OpCode.ANY),OpCode(0x95, OpCode.ANY),
      OpCode(0x96, OpCode.ANY),OpCode(0xDD, 0x96),OpCode(0xFD, 0x96),OpCode(0xD6, OpCode.ANY)) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

}
