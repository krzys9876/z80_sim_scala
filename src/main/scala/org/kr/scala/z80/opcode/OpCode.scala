package org.kr.scala.z80.opcode

case class OpCode(main:Int,supp:Int=OpCode.ANY,supp2:Int=OpCode.ANY) {
  /* OpCode format:
  main (supp) (d) (supp2)
  main - primary OpCode for 1-byte opcodes (1st byte of any operation)
  supp - supplementary OpCode for 2-byte opcodes (2nd byte)
  d - offset for opcodes using indexed notation (IX/IY+d) (3rd byte)
  supp2 - second supplementary OpCode for 4-byte opcodes (some IX+d/IY+d) (4th byte)
   */
  override def toString: String = f"OpCode($main${if(supp!=OpCode.ANY) ","+supp}${if(supp2!=OpCode.ANY) ","+supp2})"

  lazy val isNop:Option[OpType] = Nop.opType(this)
  lazy val isLoad8Bit:Option[OpType] = Load8Bit.opType(this)
  lazy val isLoad16Bit:Option[OpType] = Load16Bit.opType(this)
  lazy val isExchange:Option[OpType] = Exchange.opType(this)
  lazy val isArithmetic8Bit:Option[OpType] = Arithmetic8Bit.opType(this)
  lazy val isArithmetic16Bit:Option[OpType] = Arithmetic16Bit.opType(this)
  lazy val isRotateShift:Option[OpType] = RotateShift.opType(this)
  lazy val isRotateDigit:Option[OpType] = RotateDigit.opType(this)

  lazy val opTypeList:List[Option[OpType]]=OpType.specs.map(_.opType(this))
  lazy val opType:OpType= if(main==0) OpType.NopType else opTypeList.find(_.isDefined).flatten.getOrElse(OpType.UnknownType)
}

object OpCode {
  val ANY:Int = Int.MinValue
}

class UnknownOperationException(message : String) extends Exception(message) {}

sealed abstract class OpType(name:String)

object OpType {
  case object NopType extends OpType("NOP")
  case object Load8BitType extends OpType("Load8Bit")
  case object Load16BitType extends OpType("Load16Bit")
  case object ExchangeType extends OpType("Exchange")
  case object Arithmetic8BitType extends OpType("Arithmetic8Bit")
  case object Arithmetic16BitType extends OpType("Arithmetic16Bit")
  case object RotateShiftType extends OpType("RotateShift")
  case object RotateDigitType extends OpType("RotateDigit")
  case object UnknownType extends OpType("Unknown")

  val specs:List[OperationSpec]=List(Load8Bit,Load16Bit,Exchange,Arithmetic8Bit,Arithmetic16Bit,RotateShift,RotateDigit,Nop)
}
