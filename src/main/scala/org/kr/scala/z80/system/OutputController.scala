package org.kr.scala.z80.system

class OutputController(override val state:OutputFile) extends BaseStateMonad[OutputFile](state) {
  def >>= (fChangeState: OutputFile=>OutputController):OutputController=fChangeState(state)
}

object OutputController {
  def apply(state: OutputFile):OutputController = new OutputController(state)
  def blank:OutputController = new OutputController(OutputFile.blank)
}
