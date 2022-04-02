package org.kr.scala.z80

abstract class MapHandler[From,To](val mapOfLists:Map[List[From],To]) {
  lazy val m:Map[From,To]=MapHandler.flatten(mapOfLists)
  val defaultFrom:From=>From
  val defaultTo:To
  lazy val find:From=>To = from => m.getOrElse(from,m.getOrElse(defaultFrom(from),defaultTo))
  lazy val contains:From=>Boolean = from => m.contains(from) || m.contains(defaultFrom(from))
}

object MapHandler {
  def flatten[A,B](mapOfLists:Map[List[A],B]):Map[A,B]=
    mapOfLists.map(entry=>entry._1.flatMap(opcode=>Map(opcode->entry._2))).flatten.toMap
}
