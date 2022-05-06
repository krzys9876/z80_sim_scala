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

class InputFile(val ports:Map[Int,InputPort]=Map()) {
  def read(port:Int):Int=ports.getOrElse(port,InputPortConstant.blank).read()
  def addOrReplace(port:Int, inPort:InputPort):InputFile=new InputFile(this.ports ++ Map(port->inPort))
}

object InputFile {
  def blank:InputFile=new InputFile(Map())

  def keys2Ints(line:String):List[Int]=line.chars().toArray.toList ++ List(0x0D)
  def keys2IntsCR(line:String,cr:Int):List[Int]=line.chars().toArray.toList ++ List(cr)
  def lines2IntsCR(lines:String,cr:Int):List[Int]=lines.strip().split("\r\n").toList
    .foldLeft(List[Int]())((list,line)=>list ++ keys2IntsCR(line.strip(),cr))
}

