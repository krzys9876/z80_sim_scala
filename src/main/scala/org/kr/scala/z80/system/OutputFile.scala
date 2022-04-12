package org.kr.scala.z80.system

class OutputFile(val files:Map[Int,Vector[Int]]) {
  def apply(port:Int,pos:Int):Int={
    val file=getFile(port)
    if(file.size>pos) file(pos)
    else 0
  }
  def put(port:Int,value:Int):OutputFile={
    val file=getFile(port) :+ value
    new OutputFile(files ++ Map(port->file))
  }

  def print(port:Int,limit:Int=9999):Unit =
    getFile(port).slice(0,limit).foreach(value=>println(f"0x$value%02X $value ${value.toChar}"))

  private def getFile(port:Int):Vector[Int]=files.getOrElse(port,Vector())
}

object OutputFile {
  def blank:OutputFile= new OutputFile(Map())
}
