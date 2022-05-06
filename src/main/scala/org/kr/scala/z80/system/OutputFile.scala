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

  def show(port:Int, limit:Int=9999)(implicit formatter:OutputFormatter, outputter:Outputter):Unit =
    getFile(port).slice(0,limit).foreach(value=>outputter.out(formatter.format(value)))

  private def getFile(port:Int):Vector[Int]=files.getOrElse(port,Vector())
}

object OutputFile {
  def blank:OutputFile= new OutputFile(Map())
}

trait OutputFormatter {
  def format:Int=>String
}

object DetailedFormatter extends OutputFormatter {
  override def format:Int=>String = value=>f"0x$value%02X $value ${value.toChar}"
}

object CharFormatter extends OutputFormatter {
  override def format:Int=>String = value=>f"${value.toChar}"
}

trait Outputter {
  def out:String=>Unit
}

object PrintOutputter extends Outputter {
  override def out:String=>Unit=text=>print(text)
}

object PrintlnOutputter extends Outputter {
  override def out:String=>Unit=text=>print(text)
}

