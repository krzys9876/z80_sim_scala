package org.kr.scala.z80.utils

abstract class MapHandler[From,To](val m:Map[From,To]) {
  val defaultFrom:From=>List[From]
  val defaultTo:To
  lazy val find:From=>To = from =>findInMapOrDefault(from).getOrElse(from->defaultTo)._2
  lazy val contains:From=>Boolean = from =>findInMapOrDefault(from).isDefined

  private def findInMapOrDefault(from:From):Option[(From,To)] =
    m.find(entry=>entry._1==from || defaultFrom(from).contains(entry._1))
}
