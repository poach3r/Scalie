package org.poach3r

import os.*

import scala.io
import scala.io.StdIn

object Main:
  def main(args: Array[String]): Unit =
    runLoop()

def runLoop(printing: Boolean = false): Unit = try
  println(os.pwd)
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
