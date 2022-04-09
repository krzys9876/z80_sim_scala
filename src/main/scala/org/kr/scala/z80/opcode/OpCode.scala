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
  lazy val opTypeSpec:OperationSpec=OpCode.specs.find(_.isOper(this)).getOrElse(Unknown)
}

object OpCode {
  val ANY:Int = Int.MinValue
  val specs:List[OperationSpec]=
    List(Load8Bit,Load16Bit,Exchange,Arithmetic8Bit,Arithmetic16Bit,RotateShift,RotateDigit,Nop,Unknown)
}

class UnknownOperationException(message : String) extends Exception(message) {}
