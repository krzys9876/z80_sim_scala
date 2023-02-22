package org.kr.scala.z80.system

abstract class InputPort {
  def read():Int
  def refresh():InputPort
}

class InputPortConstant(val value:Int) extends InputPort {
  def read():Int=value
  def refresh():InputPort=this
}

object InputPortConstant {
  val blank:InputPortConstant=new InputPortConstant(0)
}

class InputPortSequential(val value:Int, val every: Int, val position:Int=0, defaultValue:Int=0) extends InputPort {
  def read():Int=if(position==0) value else defaultValue
  def refresh():InputPort=new InputPortSequential(value,every,(position+1) % every,defaultValue)
}

class InputPortSingle(val value:Int, val defaultValue:Int=0, val isAtStart:Boolean=true) extends InputPort {
  def read():Int=if(isAtStart) value else defaultValue
  def refresh():InputPort=new InputPortSingle(value,defaultValue,false)
}

class InputPortMultiple(val valueList:List[Int], val defaultValue:Int=0) extends InputPort {
  def read():Int=valueList match {
    case head :: _ => head
    case _ => defaultValue
  }
  def refresh():InputPort=new InputPortMultiple(
    valueList match {
      case _ :: tail => tail
      case _ => List()
    }, defaultValue)
}

class InputFile(val ports:Map[PortID,InputPort]=Map()) {
  def read(port:PortID)(implicit debugger:Debugger):Int={
    ports.getOrElse(port,InputPortConstant.blank).read()
  }
  def attachPort(port:PortID, inPort:InputPort):InputFile=new InputFile(this.ports ++ Map(port->inPort))
}

object InputFile {
  private val CR:Int=0x0D
  def blank:InputFile=new InputFile(Map())

  private def keys2Ints(line:String, cr:Int=CR):List[Int]=line.chars().toArray.toList ++ List(cr)
  def linesList2Ints(lines:List[String],cr:Int=CR):List[Int]=
    lines
    .foldLeft(List[Int]())((list,line)=>list ++ keys2Ints(line.strip(),cr))

  // functions changing state (InputFile=>InputFile)
  val attachPort: (PortID, InputPort) => InputFile => InputFile = (port, inPort) => inputFile => inputFile.attachPort(port,inPort)
  val refreshPort: PortID => InputFile => InputFile = port => inputFile => {
    val inputPort:InputPort=inputFile.ports.getOrElse(port,InputPortConstant.blank)
    val inputPortRefreshed:InputPort=inputPort.refresh()
    inputFile.attachPort(port,inputPortRefreshed)
  }

}

case class PortID(num:Int)

