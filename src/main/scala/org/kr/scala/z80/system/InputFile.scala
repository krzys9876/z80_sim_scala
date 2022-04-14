package org.kr.scala.z80.system

abstract class InputPort {
  def read():Int
}

class InputPortConstant(val value:Int) extends InputPort {
  def read():Int=value
}

class InputFile(val ports:Map[Int,InputPort]=Map()) {
  def read(port:Int):Int=ports.getOrElse(port,new InputPortConstant(0)).read()
  def add(port:Int,inPort:InputPort):InputFile=new InputFile(this.ports ++ Map(port->inPort))
}

object InputFile {
  def blank:InputFile=new InputFile(Map())
}

