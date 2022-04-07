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

  lazy val isNop:Boolean = main==0
  lazy val isLoad8Bit:Boolean = Load8Bit.isOper(this)
  lazy val isLoad16Bit:Boolean = Load16Bit.isOper(this)
  lazy val isExchange:Boolean = Exchange.isOper(this)
  lazy val isArithmetic8Bit:Boolean = Arithmetic8Bit.isOper(this)
  lazy val isArithmetic16Bit:Boolean = Arithmetic16Bit.isOper(this)
  lazy val isRotateShift:Boolean = RotateShift.isOper(this)
  lazy val opType:OpType=
    isNop match {
      case true => OpType.Nop
      case _ => isLoad8Bit match {
        case true => OpType.Load8Bit
        case _ => isLoad16Bit match {
          case true => OpType.Load16Bit
          case _ => isExchange match {
            case true => OpType.Exchange
            case _ => isArithmetic8Bit match {
              case true => OpType.Arithmetic8Bit
              case _ => isArithmetic16Bit match {
                case true => OpType.Arithmetic16Bit
                case _ => isRotateShift match {
                  case true => OpType.RotateShift
                  case _ => OpType.Unknown
                }
              }
            }
          }
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
  case object Exchange extends OpType("Exchange")
  case object Arithmetic8Bit extends OpType("Arithmetic8Bit")
  case object Arithmetic16Bit extends OpType("Arithmetic16Bit")
  case object RotateShift extends OpType("RotateShift")
  case object Unknown extends OpType("Unknown")
}
