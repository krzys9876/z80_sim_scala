package org.kr.scala.z80.system

class BaseStateMonad[StateType](val state:StateType) {
  // equivalent to map
  def >>= (fChangeState: StateType=>BaseStateMonad[StateType]):BaseStateMonad[StateType]=fChangeState(state)
  // equivalent to flatMap
  def >>== (fChangeState: StateType=>StateType):BaseStateMonad[StateType]=BaseStateMonad(fChangeState(state))
  def get:StateType = state
}

object BaseStateMonad {
  def apply[StateType](state: StateType):BaseStateMonad[StateType] = new BaseStateMonad(state)
}
