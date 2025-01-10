package org.poach3r

import os.*

import scala.io
import scala.io.StdIn
import scala.annotation.tailrec

object Main:
  // this is the only mutable thing in this entire project and it really pisses me off
  var wd: Path = os.pwd

  def main(args: Array[String]): Unit =
    runLoop()

  /** The main shell loop
    *
    * @param printing
    *   Whether `Printer` should be used instead of `Interpreter`
    */
  def runLoop(printing: Boolean = false): Unit = try
    println(wd)
    print("> ")
    val tokens = Scanner.scan(StdIn.readLine)
    val expressions = Parser.parse(tokens)

    if printing then expressions.map(_.accept(Printer)).foreach(println)
    else expressions.map(_.accept(Interpreter))
    println()
    runLoop()
  catch
    case e: Exception =>
      e.printStackTrace()
      runLoop()
