package org.kr.scala.z80.system

class RegisterController(override val state:Register) extends BaseStateMonad[Register](state) {
  def >>= (fChangeState: Register=>RegisterController):RegisterController=fChangeState(state)
}

object RegisterController {
  def apply(state: Register):RegisterController = new RegisterController(state)
  def blank:RegisterController = new RegisterController(Register.blank)

  val set: (String, Int) => Register => RegisterController = (regSymbol, value) => register =>
    RegisterController(register.set(regSymbol, value))
  val setRelative: (String, Int) => Register => RegisterController = (regSymbol, value) => register =>
    RegisterController(register.setRelative(regSymbol, value))
}
