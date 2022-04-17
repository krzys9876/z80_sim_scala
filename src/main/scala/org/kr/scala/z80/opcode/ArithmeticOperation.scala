package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.Flag
import org.kr.scala.z80.utils.Z80Utils

abstract class ArithmeticOperationCalc(override val name:String) extends ArithmeticOperation(name) with FlagCalculatorBase {
  def calcAll(input:ArithmeticOpInput):(Int,Flag)={
    val calcResult=calc(input)
    val calcFlags=flags(calcResult,input.flags)
    (calcResult.valueOut,calcFlags)
  }

  def calc(input:ArithmeticOpInput):ArithmeticOpResult
}

abstract class ArithmeticOperation(val name:String)

case class ArithmeticOpInput(value:Int, operand:Int, flags:Flag)

class ArithmeticOpResult(val valueUnsigned:Int, val valueSigned:Int, val valueHalf:Int, val isWord:Boolean=false) {
  lazy val valueOut:Int=valueUnsigned & (if(isWord) 0xFFFF else 0xFF)
}

class ArithmeticOpResultByte(override val valueUnsigned:Int, override val valueSigned:Int, override val valueHalf:Int)
  extends ArithmeticOpResult(valueUnsigned,valueSigned,valueHalf,false)

class ArithmeticOpResultWord(override val valueUnsigned:Int, override val valueSigned:Int, override val valueHalf:Int)
  extends ArithmeticOpResult(valueUnsigned,valueSigned,valueHalf,true)

object ArithmeticOpType {
  case object Add extends ArithmeticOperation("ADD")
  case object AddC extends ArithmeticOperation("ADD_CARRY")
  case object Sub extends ArithmeticOperation("SUB")
  case object SubC extends ArithmeticOperation("SUB_CARRY")
  case object And extends ArithmeticOperation("AND")
  case object Or extends ArithmeticOperation("OR")
  case object Xor extends ArithmeticOperation("XOR")
  case object Comp extends ArithmeticOperation("CP")
  case object Inc extends ArithmeticOperation("INC")
  case object Dec extends ArithmeticOperation("DEC")
  case object Rlc extends ArithmeticOperation("RLC")
  case object Rrc extends ArithmeticOperation("RRC")
  case object Rl extends ArithmeticOperation("RL")
  case object Rr extends ArithmeticOperation("RR")
  case object Rlca extends ArithmeticOperation("RLCA")
  case object Rrca extends ArithmeticOperation("RRCA")
  case object Rla extends ArithmeticOperation("RLA")
  case object Rra extends ArithmeticOperation("RRA")
  case object Sla extends ArithmeticOperation("SLA")
  case object Sra extends ArithmeticOperation("SRA")
  case object Srl extends ArithmeticOperation("SRL")
  case object Rld extends ArithmeticOperation("RLD")
  case object Rrd extends ArithmeticOperation("RRD")
  case object Cpl extends ArithmeticOperation("CPL")
  case object Neg extends ArithmeticOperation("NEG")
  case object Ccf extends ArithmeticOperation("CCF")
  case object Scf extends ArithmeticOperation("SCF")
  case object None extends ArithmeticOperation("NONE")
}

class AritheticOpLocationBase(val operation:ArithmeticOperation)
class ArithmeticOpLocationAccum(override val operation:ArithmeticOperation) extends AritheticOpLocationBase(operation)
class ArithmeticOpLocationFlags(override val operation:ArithmeticOperation) extends AritheticOpLocationBase(operation)
class ArithmeticOpVariableLocation(override val operation:ArithmeticOperation) extends AritheticOpLocationBase(operation)

object AritheticOpLocationBase {
  val empty:AritheticOpLocationBase=new AritheticOpLocationBase(ArithmeticOpType.None)
}

trait FlagCalculatorBase {
  def calcS(res:ArithmeticOpResult,prevFlags:Flag):Boolean=prevFlags(Flag.S)
  def calcZ(res:ArithmeticOpResult,prevFlags:Flag):Boolean=prevFlags(Flag.Z)
  def calcH(res:ArithmeticOpResult,prevFlags:Flag):Boolean=prevFlags(Flag.H)
  def calcP(res:ArithmeticOpResult,prevFlags:Flag):Boolean=prevFlags(Flag.P)
  def calcN(res:ArithmeticOpResult,prevFlags:Flag):Boolean=prevFlags(Flag.N)
  def calcC(res:ArithmeticOpResult,prevFlags:Flag):Boolean=prevFlags(Flag.C)

  def flags(res:ArithmeticOpResult,prevFlags:Flag):Flag={
    new Flag(Flag.set(
      calcS(res,prevFlags),
      calcZ(res,prevFlags),
      calcH(res,prevFlags),
      calcP(res,prevFlags),
      calcN(res,prevFlags),
      calcC(res,prevFlags)))
  }
}

trait FlagSSignByte extends FlagCalculatorBase {
  override def calcS(res:ArithmeticOpResult,prevFlags:Flag):Boolean=Z80Utils.isNegativeByte(res.valueUnsigned)
}
trait FlagSSignWord extends FlagCalculatorBase {
  override def calcS(res:ArithmeticOpResult,prevFlags:Flag):Boolean=Z80Utils.isNegativeWord(res.valueUnsigned)
}
trait FlagZZero extends FlagCalculatorBase {
  override def calcZ(res:ArithmeticOpResult,prevFlags:Flag):Boolean=res.valueOut==0
}
trait FlagHCarryByte extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,prevFlags:Flag):Boolean=res.valueHalf>0x0F
}
trait FlagHCarryWord extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,prevFlags:Flag):Boolean=res.valueHalf > 0x0FFF
}
trait FlagHBorrow extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,prevFlags:Flag):Boolean=res.valueHalf<0
}
trait FlagPOverflowByte extends FlagCalculatorBase {
  override def calcP(res:ArithmeticOpResult,prevFlags:Flag):Boolean=Z80Utils.isOutOfRangeByte(res.valueSigned)
}
trait FlagPOverflowWord extends FlagCalculatorBase {
  override def calcP(res:ArithmeticOpResult,prevFlags:Flag):Boolean=Z80Utils.isOutOfRangeWord(res.valueSigned)
}
trait FlagNSet extends FlagCalculatorBase {
  override def calcN(res:ArithmeticOpResult,prevFlags:Flag):Boolean=true
}
trait FlagNReset extends FlagCalculatorBase {
  override def calcN(res:ArithmeticOpResult,prevFlags:Flag):Boolean=false
}
trait FlagCCarry extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,prevFlags:Flag):Boolean=res.valueUnsigned > res.valueOut
}
trait FlagCBorrow extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,prevFlags:Flag):Boolean=res.valueUnsigned < res.valueOut
}

