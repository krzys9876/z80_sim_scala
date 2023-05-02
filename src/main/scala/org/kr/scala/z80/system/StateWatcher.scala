package org.kr.scala.z80.system

abstract class StateWatcherBase[StateType]()(implicit val debugger: Debugger) {
  def state:StateType
  // equivalent to map
  def >>== (fChangeState: StateType=>StateType):StateWatcherBase[StateType]= unit(fChangeState(state))
  // equivalent to flatMap
  def >>= (fChangeState: StateType=>StateWatcherBase[StateType]):StateWatcherBase[StateType]=fChangeState(state)
  def get:StateType = state
  def unit(newState:StateType):StateWatcherBase[StateType]
}

abstract class StateWatcherHandlerBase[StateType]() {
  def createNewWatcher(state:StateType)(implicit debugger: Debugger):StateWatcherBase[StateType]
}

class StateWatcher[StateType](override val state:StateType)(override implicit val debugger: Debugger) extends StateWatcherBase[StateType]() {
  // equivalent to map
  override def >>== (fChangeState: StateType=>StateType):StateWatcher[StateType]={
    val after=unit(fChangeState(state))
    debugger.info(state,after.state)
    after
  }

  override def unit(newState: StateType): StateWatcher[StateType] = new StateWatcher(newState)
}

object StateWatcher {
  def apply[StateType](state: StateType)(implicit debugger: Debugger):StateWatcher[StateType] = new StateWatcher(state)
}

class StateWatcherHandler[StateType]() extends StateWatcherHandlerBase[StateType] {
  override def createNewWatcher(state:StateType)(implicit debugger: Debugger):StateWatcher[StateType] = StateWatcher(state)
}

class StateWatcherSilent[StateType](override val state:StateType)(override implicit val debugger: Debugger) extends StateWatcherBase[StateType]() {
  override def unit(newState: StateType): StateWatcherSilent[StateType] = new StateWatcherSilent(newState)
}

object StateWatcherSilent {
  def apply[StateType](state: StateType)(implicit debugger: Debugger):StateWatcherSilent[StateType] = new StateWatcherSilent(state)
}

class StateWatcherMutable[StateType](var s:StateType)(override implicit val debugger: Debugger) extends StateWatcherBase[StateType]() {
  def state: StateType = s
  override def unit(newState: StateType): StateWatcherMutable[StateType] = {
    s=newState
    this
  }
}

object StateWatcherMutable {
  def apply[StateType](state: StateType)(implicit debugger: Debugger):StateWatcherMutable[StateType] = new StateWatcherMutable(state)
}

class StateWatcherMutableHandler[StateType]() extends StateWatcherHandlerBase[StateType] {
  override def createNewWatcher(state:StateType)(implicit debugger: Debugger):StateWatcherMutable[StateType] = StateWatcherMutable(state)
}
