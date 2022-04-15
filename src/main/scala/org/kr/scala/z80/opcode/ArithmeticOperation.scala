package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.Flag

abstract class ArithmeticOperationCalc(override val name:String) extends ArithmeticOperation(name) {
  def calcAll(input:ArithmeticOpInput):(Int,Flag)={
    val calcResult=calc(input)
    val calcFlags=flags(calcResult,input.flags)
    (calcResult.valueOut,calcFlags)
  }
  def calc(input:ArithmeticOpInput):ArithmeticOpResult
  def flags(res:ArithmeticOpResult,prevFlags:Flag):Flag=prevFlags
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
