package org.kr.scala.z80.opcode

sealed abstract class ArithmeticOperation(val name:String)

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
  case object Sla extends ArithmeticOperation("SLA")
  case object Sra extends ArithmeticOperation("SRA")
  case object Srl extends ArithmeticOperation("SRL")
  case object Rld extends ArithmeticOperation("RLD")
  case object Rrd extends ArithmeticOperation("RRD")
  case object None extends ArithmeticOperation("NONE")
}

class AritheticOpLocationBase(val operation:ArithmeticOperation)
class ArithmeticOpLocationAccum(override val operation:ArithmeticOperation) extends AritheticOpLocationBase(operation)
class ArithmeticOpLocationFlags(override val operation:ArithmeticOperation) extends AritheticOpLocationBase(operation)
class ArithmeticOpVariableLocation(override val operation:ArithmeticOperation) extends AritheticOpLocationBase(operation)

object AritheticOpLocationBase {
  val empty:AritheticOpLocationBase=new AritheticOpLocationBase(ArithmeticOpType.None)
}
