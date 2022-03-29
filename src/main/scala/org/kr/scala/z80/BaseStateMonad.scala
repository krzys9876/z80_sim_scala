package org.kr.scala.z80

class BaseStateMonad[StateType](val state:StateType) {
  def >>= (fChangeState: StateType=>BaseStateMonad[StateType]):BaseStateMonad[StateType]=fChangeState(state)
  def get:StateType = state
}

object BaseStateMonad {
  def apply[StateType](state: StateType):BaseStateMonad[StateType] = new BaseStateMonad(state)
}
