package org.kr.scala.z80.system

class StateWatcher[StateType](val state:StateType) {
  // equivalent to map
  def >>== (fChangeState: StateType=>StateType):StateWatcher[StateType]=StateWatcher(fChangeState(state))
  // equivalent to flatMap
  def >>= (fChangeState: StateType=>StateWatcher[StateType]):StateWatcher[StateType]=fChangeState(state)
  def get:StateType = state
}

object StateWatcher {
  def apply[StateType](state: StateType):StateWatcher[StateType] = new StateWatcher(state)
}
