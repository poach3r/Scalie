package org.poach3r

import mainargs.{main, arg, ParserForClass, Flag}

import os.*

import scala.io
import scala.io.StdIn
import scala.annotation.tailrec

object Main:
  // this is the only mutable thing in this entire project and it really pisses me off
  var wd: Path = os.pwd

  @main
  case class Config(
      @arg(short = 'p', doc = "The prompt that is shown before all commands.")
      prompt: Option[String],
      @arg(doc = "Use the printer instead of the interpreter.") printer: Flag
  )

  def main(args: Array[String]): Unit =
    val config = ParserForClass[Config].constructOrExit(args)
    runLoop(config)

  /** The main shell loop
    *
    * @param printing
    *   Whether `Printer` should be used instead of `Interpreter`
    */
  def runLoop(
      config: Config
  ): Unit = try
    // prompt
    Parser
      .parse(
        Scanner.scan(config.prompt.getOrElse("echo[\"-n\" $pwd[] \"\n> \"]"))
      )
      .map(_.accept(Interpreter))

    val tokens = Scanner.scan(StdIn.readLine)
    val expressions = Parser.parse(tokens)

    if config.printer.value then
      expressions.map(_.accept(Printer)).foreach(println)
    else expressions.map(_.accept(Interpreter))
    println()
    runLoop(config)
  catch
    case e: Exception =>
      e.printStackTrace()
      runLoop(config)
