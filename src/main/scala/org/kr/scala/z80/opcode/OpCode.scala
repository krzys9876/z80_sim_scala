package org.kr.scala.z80.opcode

case class OpCode(main:Int,supp:Int) {
  override def toString: String = f"OpCode($main,${if(supp==OpCode.ANY) "ANY" else supp})"

  lazy val isNop:Boolean = main==0
  lazy val isLoad8Bit:Boolean = Load8Bit.isOper(this)
  lazy val isLoad16Bit:Boolean = Load16Bit.isOper(this)
  lazy val opType:OpType=
    isNop match {
      case true => OpType.Nop
      case _ => isLoad8Bit match {
        case true => OpType.Load8Bit
        case _ => isLoad16Bit match {
          case true => OpType.Load16Bit
          case _ => OpType.Unknown
        }
      }
    }
}

object OpCode {
  val ANY:Int = Int.MinValue
}

class UnknownOperationException(message : String) extends Exception(message) {}

sealed abstract class OpType(name:String)

object OpType {
  case object Nop extends OpType("NOP")
  case object Load8Bit extends OpType("Load8Bit")
  case object Load16Bit extends OpType("Load16Bit")
  case object Unknown extends OpType("Unknown")
}
