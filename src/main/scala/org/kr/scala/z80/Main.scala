package org.kr.scala.z80

import org.kr.scala.z80.system.{CharFormatter, ConsoleDebugger, Debugger, DummyDebugger, InputController, InputFile, InputPortMultiple, MemoryController, OutputController, OutputFormatter, Outputter, PrintOutputter, RegisterController, Z80System, Z80SystemController}

import scala.jdk.CollectionConverters.ListHasAsScala
import java.nio.file.{Files, Path}

object Main extends App {

  if(args.length!=2) {
    println("Incorrect input parameters: hex file and input file")
    System.exit(1)
  }

  println("START")

  val CONTROL_PORT=0xB1
  val DATA_PORT=0xB0
  val MEMORY_TOP="65536"
  // memory
  val memory=prepareMemory(args(0))
  // input keys sequence
  val input=prepareInput(args(1))
  //whole system
  val initSystem=Z80SystemController(new Z80System(memory,RegisterController.blank,OutputController.blank,input))

  implicit val debugger:Debugger=ConsoleDebugger
  val after=initSystem >>= Z80SystemController.run(debugger)(500000)

  //printOutput()

  println("END")

  private def readFile(fullFileWithPath:String):List[String]=
    Files.readAllLines(Path.of(fullFileWithPath)).asScala.toList

  private def prepareMemory(hexFile:String):MemoryController={
    val hexLines=readFile(hexFile)
    MemoryController.blank(0x10000) >>= MemoryController.loadHexLines(hexLines) >>= MemoryController.lockTo(0x4000)
  }

  private def prepareInput(inputFile:String):InputController={
    // add initial "memory top" answer to skip long-lasting memory test
    val inputLines=List(MEMORY_TOP) ++ readFile(inputFile)
    val inputList:List[Int]=InputFile.linesList2Ints(inputLines)
    val inputPortKeys=new InputPortMultiple(inputList)
    val inputPortControl=new InputPortMultiple(List.fill(inputList.length)(1))
    InputController.blank >>=
      InputController.attachPort(CONTROL_PORT,inputPortControl) >>=
      InputController.attachPort(DATA_PORT,inputPortKeys)
  }

  private def printOutput():Unit={
    implicit val outputFormatter:OutputFormatter=CharFormatter
    implicit val outputter:Outputter=PrintOutputter
    after.get.outputController.get.show(DATA_PORT)
  }
}