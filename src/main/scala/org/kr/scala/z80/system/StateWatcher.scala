package org.kr.scala.z80.system

class StateWatcherBase[StateType](val state:StateType)(implicit val debugger: Debugger) {
  // equivalent to map
  def >>== (fChangeState: StateType=>StateType):StateWatcherBase[StateType]= StateWatcherBase(fChangeState(state))
  // equivalent to flatMap
  def >>= (fChangeState: StateType=>StateWatcherBase[StateType]):StateWatcherBase[StateType]=fChangeState(state)
  def get:StateType = state
}

object StateWatcherBase {
  def apply[StateType](state: StateType)(implicit debugger: Debugger):StateWatcherBase[StateType] = new StateWatcherBase(state)
}

class StateWatcher[StateType](override val state:StateType)(override implicit val debugger: Debugger) extends StateWatcherBase(state) {
  // equivalent to map
  override def >>== (fChangeState: StateType=>StateType):StateWatcher[StateType]={
    val after=StateWatcher(fChangeState(state))
    debugger.info(state,after.state)
    after
  }
}

object StateWatcher {
  def apply[StateType](state: StateType)(implicit debugger: Debugger):StateWatcher[StateType] = new StateWatcher(state)
}

class StateWatcherSilent[StateType](override val state:StateType)(override implicit val debugger: Debugger) extends StateWatcherBase(state)

object StateWatcherSilent {
  def apply[StateType](state: StateType)(implicit debugger: Debugger):StateWatcherSilent[StateType] = new StateWatcherSilent(state)
}

