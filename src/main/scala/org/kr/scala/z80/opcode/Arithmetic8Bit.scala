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
class Arith8BitAccumReg(override val operation:ArithmeticOperation, val operandReg:String) extends Arith8BitBase(operation)

object Arith8BitBase {
  val empty:Arith8BitBase=new Arith8BitBase(Arith8Bit.None)
}


object Arithmetic8Bit extends OperationSpec {

  val arithOperationListMap: Map[List[OpCode],List[Arith8BitBase]] = Map(
    //register
    List(OpCode(0x87, OpCode.ANY)) -> List(new Arith8BitAccumReg(Arith8Bit.Add,"A")),
    List(OpCode(0x80, OpCode.ANY)) -> List(new Arith8BitAccumReg(Arith8Bit.Add,"B")),
    List(OpCode(0x81, OpCode.ANY)) -> List(new Arith8BitAccumReg(Arith8Bit.Add,"C")),
    List(OpCode(0x82, OpCode.ANY)) -> List(new Arith8BitAccumReg(Arith8Bit.Add,"D")),
    List(OpCode(0x83, OpCode.ANY)) -> List(new Arith8BitAccumReg(Arith8Bit.Add,"E")),
    List(OpCode(0x84, OpCode.ANY)) -> List(new Arith8BitAccumReg(Arith8Bit.Add,"H")),
    List(OpCode(0x85, OpCode.ANY)) -> List(new Arith8BitAccumReg(Arith8Bit.Add,"L"))
  )
  val arithOperation: OpCodeMap[List[Arith8BitBase]] = new OpCodeMap(arithOperationListMap, List(Arith8BitBase.empty))


  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x87, OpCode.ANY),OpCode(0x80, OpCode.ANY),OpCode(0x81, OpCode.ANY),OpCode(0x82, OpCode.ANY),
      OpCode(0x83, OpCode.ANY),OpCode(0x84, OpCode.ANY),OpCode(0x85, OpCode.ANY)) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

}
