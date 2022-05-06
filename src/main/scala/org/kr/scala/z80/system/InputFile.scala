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

/*class InputPortSequential(val value:Int, val every: Int) extends InputPort {
  def read():Int=value
}*/


class InputFile(val ports:Map[Int,InputPort]=Map()) {
  def read(port:Int):Int=ports.getOrElse(port,InputPortConstant.blank).read()
  def addOrReplace(port:Int, inPort:InputPort):InputFile=new InputFile(this.ports ++ Map(port->inPort))
}

object InputFile {
  def blank:InputFile=new InputFile(Map())
}

