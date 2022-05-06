package org.kr.scala.z80.system

class InputController(override val state:InputFile) extends BaseStateMonad[InputFile](state){
  def >>= (fChangeState: InputFile=>InputController):InputController=fChangeState(state)

  def read(port:Int):Int=state.read(port)
}

object InputController {
  def apply(state: InputFile):InputController = new InputController(state)
  def blank:InputController = new InputController(InputFile.blank)

  val attachPort: (Int, InputPort) => InputFile => InputController = (port, inPort) => inputFile =>
    InputController(inputFile.addOrReplace(port,inPort))

  val refreshPort: Int => InputFile => InputController = port => inputFile => {
    val inputPort:InputPort=inputFile.ports.getOrElse(port,InputPortConstant.blank)
    val inputPortRefreshed:InputPort=inputPort.refresh()
    InputController(inputFile.addOrReplace(port,inputPortRefreshed))
  }

}
