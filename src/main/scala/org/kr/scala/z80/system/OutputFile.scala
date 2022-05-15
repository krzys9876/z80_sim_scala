package org.kr.scala.z80.system

class OutputPort(val data:Vector[Int]) {
  def put(value:Int)= new OutputPort(data :+ value)
  val size:Int=data.size
  def apply(pos:Int):Int=data(pos)
}

object OutputPort {
  val empty:OutputPort=new OutputPort(Vector())
}

class OutputFile(val files:Map[Int,OutputPort]) {
  def apply(port:Int,pos:Int):Int={
    val file=getFile(port)
    if(file.size>pos) file(pos)
    else 0
  }
  def write(port:Int, value:Int)(implicit debugger: Debugger):OutputFile={
    val file=getFile(port).put(value)
    debugger.output(port,value)
    new OutputFile(files ++ Map(port->file))
  }

  def show(port:Int, limit:Int=9999)(implicit formatter:OutputFormatter, outputter:Outputter):Unit =
    getFile(port).data.slice(0,limit).foreach(value=>outputter.out(formatter.format(value)))

  private def getFile(port:Int):OutputPort=files.getOrElse(port,OutputPort.empty)
}

object OutputFile {
  def blank:OutputFile= new OutputFile(Map())

  def out(implicit debugger:Debugger): (Int, Int) => OutputFile => OutputFile = (port, value) => outputFile =>
    outputFile.write(port,value)

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

