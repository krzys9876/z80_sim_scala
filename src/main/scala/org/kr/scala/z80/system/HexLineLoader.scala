package org.kr.scala.z80.system

class HexLineLoader(val line: String) {
  val lineType:Int=HexStr(line.substring(7,9)).int
  val bytes:Int=if(isEndLine) 0 else HexStr(line.substring(1,3)).int
  val address:Int=if(isEndLine) 0 else HexStr(line.substring(3,7)).int
  val values:Vector[Int]= {
    Vector() ++
      (if(isEndLine) Vector()
      else
        line
          .substring(9,9+bytes*2)
          .grouped(2)
          .map(str=>HexStr(str).int))
    //val substr=line.substring(9,9+bytes*2)
    //val groups=substr.grouped(2)
    //val mapped=groups.map(str=>Integer.parseInt(str,16))
  }
  lazy val isEndLine:Boolean= lineType==1
}

case class HexStr(str:String) {
  val int:Int=Integer.parseInt(str,16)
}
