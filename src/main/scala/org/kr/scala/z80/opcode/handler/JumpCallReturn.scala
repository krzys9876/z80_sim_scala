package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system._
import org.kr.scala.z80.utils.{AnyInt, IntValue, OptionInt, Z80Utils}

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
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System, Int, Int) = {
    val actualCode=castType[OpCode with OpCodeJump with OpCodeJumpCondition with OpCodeSourceLocation with OpCodeSize with OpCodeTCycles](code)

    val oper = actualCode.operation
    val instrSize = actualCode.size
    val checker = new JumpConditionChecker(actualCode.condition)

    val chgSystemStJump = handleStackForJump(checker.isMet, oper, instrSize)(system)
    val chgSystemPC =
      handleJump(
        oper,
        actualCode.condition,
        checker,
        actualCode.source,
        instrSize)(chgSystemStJump)
    val chgSystemStReturn = handleStackForReturn(checker.isMet, oper)(chgSystemPC)

    (chgSystemStReturn, 0, if(checker.isMet) actualCode.t+actualCode.tConditional else actualCode.t)
  }

  private def calcAddress(oper: JumpOperation, value: Int, prevPC: Int): Int =
    oper match {
      case JumpType.JumpR | JumpType.DJumpR => calcRelativeAddress(prevPC, value)
      case _ => value
    }

  // Jump relative - relative operand is 2's complement and must be incremented by 2
  private def calcRelativeAddress(pc: Int, relative: Int): Int = Z80Utils.word2ComplToRaw(pc + 2 + Z80Utils.rawByteTo2Compl(relative))

  private def handleJump(oper: JumpOperation, cond: JumpConditionBase, checker: JumpConditionChecker, location: Location, instrSize: Int)
                        (system: Z80System): Z80System = {
    val prevPC = system.getRegValue(Regs.PC)
    val address = calcAddress(oper, system.getValueFromLocation(location), prevPC)
    val newPC = chooseAddress(prevPC, address, checker)
    val chgSystemPC = system.changeRegister(Regs.PC, newPC + (if (!checker.isMet) instrSize else 0))
    val chgSystem = (oper, cond) match {
      case (JumpType.DJumpR, c:RegisterJumpCondition) => chgSystemPC.changeRegister(c.register, checker.decRegValue())
      case _ => chgSystemPC
    }
    chgSystem
  }

  private def chooseAddress(prevPC: Int, address: Int, checker: JumpConditionChecker): Int =
    if (checker.isMet) address else prevPC

  private def handleStackForJump(shouldJump: Boolean, oper: JumpOperation, instrSize: Int)(system: Z80System): Z80System =
    (shouldJump, oper) match {
      case (true, JumpType.Call) =>
        system
          .changeMemoryWord(system.getRegValue(Regs.SP) - 2, system.getRegValue(Regs.PC) + instrSize)
          .changeRegisterRelative(Regs.SP, -2)
      case _ => system
    }

  private def handleStackForReturn(shouldJump: Boolean, oper: JumpOperation)(system: Z80System): Z80System =
    (shouldJump, oper) match {
      case (true, JumpType.Return) => system.changeRegisterRelative(Regs.SP, 2)
      case _ => system
    }
}

abstract class JumpConditionBase {
  val symbol:String
}

case object EmptyJumpCondition extends JumpConditionBase {
  override val symbol:String="empty"
  override def toString:String="empty"
}

case class RegisterJumpCondition(register:RegSymbol,value:OptionInt) extends JumpConditionBase {
  override val symbol:String=register.symbol
  override def toString:String=f"reg:$register=$value"
}

case class FlagJumpCondition(flag:FlagSymbol,boolValue:Boolean) extends JumpConditionBase {
  val value:OptionInt=IntValue(if(boolValue) 1 else 0)
  override val symbol:String=flag.symbol
  override def toString:String=f"flag:$flag=$value"
}

class IncorrectJumpCondition(message : String) extends Exception(message) {}

class JumpConditionChecker(val condition: JumpConditionBase)(implicit system: Z80System) {
  lazy val decRegValue:OptionInt =
    condition match {
      case c : RegisterJumpCondition => IntValue(Z80Utils.add8bit(system.getRegValue(c.register),-1))
      case _ => AnyInt
    }

  lazy val isMet: Boolean =
    condition match {
      case EmptyJumpCondition => true
      case c : FlagJumpCondition => system.getFlags.flagValue(c.flag) == c.value()
      case c : RegisterJumpCondition => decRegValue() != c.value()
      case _ => throw new IncorrectJumpCondition(f"unknown condition state: ${condition.toString}")
    }
}