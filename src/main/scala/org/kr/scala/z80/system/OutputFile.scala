package org.kr.scala.z80.system

class OutputPort(val data:Vector[Int]) {
  def put(value:Int)= new OutputPort(data :+ value)
  val size:Int=data.size
  def apply(pos:Int):Int=data(pos)
}

object OutputPort {
  val empty:OutputPort=new OutputPort(Vector())
}

class OutputFile(val files:Map[PortID,OutputPort], val lastPort:PortID=PortID(0), val lastValue:Int=0) {
  def apply(port:PortID,pos:Int):Int={
    val file=getFile(port)
    if(file.size>pos) file(pos)
    else 0
  }
  def write(port:PortID, value:Int)(implicit debugger: Debugger):OutputFile={
    val file=getFile(port).put(value)
    new OutputFile(files ++ Map(port->file),port,value)
  }

  def show(port:PortID, limit:Int=9999)(implicit formatter:OutputFormatter, outputter:Outputer):Unit =
    getFile(port).data.slice(0,limit).foreach(value=>outputter.out(formatter.format(value)))

  private def getFile(port:PortID):OutputPort=files.getOrElse(port,OutputPort.empty)
}

object OutputFile {
  def blank:OutputFile= new OutputFile(Map())

  // functions changing state (OutputFile=>OutputFile)
  def out(implicit debugger:Debugger): (PortID, Int) => OutputFile => OutputFile = (port, value) => outputFile =>
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

trait Outputer {
  def out:String=>Unit
}

object PrintOutputer extends Outputer {
  override def out:String=>Unit=text=>print(text)
}

object PrintlnOutputer extends Outputer {
  override def out:String=>Unit=text=>print(text)
}

