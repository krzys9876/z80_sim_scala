package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system._
import org.kr.scala.z80.utils.{IntValue, Z80Utils}

sealed abstract class JumpOperation(val name:String)

object JumpType {
  case object Jump extends JumpOperation("JUMP")
  case object JumpR extends JumpOperation("JUMP_RELATIVE")
  case object DJumpR extends JumpOperation("DEC_JUMP_RELATIVE")
  case object Call extends JumpOperation("CALL")
  case object Return extends JumpOperation("RETURN")
  case object None extends JumpOperation("NONE")
}

object JumpCallReturn extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeJump with OpCodeJumpCondition with OpCodeSourceLocation with OpCodeSize with OpCodeTCycles](code)

    val oper = actualCode.operation
    val instrSize = actualCode.size
    val checker = new JumpConditionChecker(actualCode.condition)

    val changePC =
      handleJump(
        oper,
        actualCode.condition,
        checker,
        actualCode.source,
        instrSize)
    val changeStack = handleStack(checker.isMet, oper, instrSize)

    (changePC ++ changeStack, 0, if(checker.isMet) actualCode.t+actualCode.tConditional else actualCode.t)
  }

  private def calcAddress(oper: JumpOperation, value: Int, prevPC: Int): Int =
    oper match {
      case JumpType.JumpR | JumpType.DJumpR => calcRelativeAddress(prevPC, value)
      case _ => value
    }

  // Jump relative - relative operand is 2's complement and must be incremented by 2
  private def calcRelativeAddress(pc: Int, relative: Int): Int = Z80Utils.word2ComplToRaw(pc + 2 + Z80Utils.rawByteTo2Compl(relative))

  private def handleJump(oper: JumpOperation, cond: JumpCondition, checker: JumpConditionChecker, location: Location, instrSize: Int)
                        (implicit system: Z80System): List[SystemChange] = {
    val prevPC = system.getRegValue(Regs.PC)
    val address = calcAddress(oper, system.getValueFromLocation(location), prevPC)
    val registerDecrValue = checker.decRegValue
    val newPC = chooseAddress(prevPC, address, checker)
    val changePC = List(new RegisterChange(Regs.PC, newPC + (if (!checker.isMet) instrSize else 0)))
    val changeReg = (oper, cond) match {
      case (JumpType.DJumpR, c) if c.isRegister => List(new RegisterChange(c.register, registerDecrValue))
      case _ => List()
    }
    changePC ++ changeReg
  }

  private def chooseAddress(prevPC: Int, address: Int, checker: JumpConditionChecker): Int =
    if (checker.isMet) address else prevPC

  private def handleStack(shouldJump: Boolean, oper: JumpOperation, instrSize: Int)(implicit system: Z80System): List[SystemChange] =
    (shouldJump, oper) match {
      case (true, JumpType.Call) => List(
        new MemoryChangeWord(system.getRegValue(Regs.SP) - 2, system.getRegValue(Regs.PC) + instrSize),
        new RegisterChangeRelative(Regs.SP, -2)
      )
      case (true, JumpType.Return) => List(
        new RegisterChangeRelative(Regs.SP, 2)
      )
      case _ => List()
    }
}

case class JumpCondition(flag:FlagSymbol,register:RegSymbol,value:Int) {
  lazy val flagValue:Boolean = value!=0
  lazy val isFlag:Boolean = flag!=Flag.None
  lazy val isRegister:Boolean = register!=Regs.NONE
  lazy val isEmpty:Boolean = this.equals(JumpCondition.empty)

  override def toString:String=if(isEmpty) "empty" else f"$flag/${if(register!=Regs.NONE) register else "-"}=$value"
}

object JumpCondition {
  def flag(flag:FlagSymbol,value:Boolean):JumpCondition=JumpCondition(flag,Regs.NONE,if(value) 1 else 0)
  def register(register:RegSymbol,value:Int):JumpCondition=JumpCondition(Flag.None,register,value)
  val empty:JumpCondition=JumpCondition(Flag.None,Regs.NONE,OpCode.ANY)
}

class IncorrectJumpCondition(message : String) extends Exception(message) {}

class JumpConditionChecker(val condition: JumpCondition)(implicit system: Z80System) {
  lazy val decRegValue:Int =
    condition match {
      case c if c.isRegister => Z80Utils.add8bit(system.getRegValue(c.register),-1)
      case _ => OpCode.ANY
    }

  lazy val isMet: Boolean =
    condition match {
      case c if c.isEmpty => true
      case c if c.isFlag => system.getFlags.flagValue(condition.flag) == condition.value
      case c if c.isRegister => decRegValue != c.value
      case _ => throw new IncorrectJumpCondition(f"unknown condition state: ${condition.toString}")
    }
}