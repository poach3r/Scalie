package org.poach3r

import scala.annotation.tailrec

import org.poach3r.builtins.*
import org.poach3r.Expression.*
import os.proc
import org.poach3r.Main.wd

object Interpreter extends Expression.Visitor[Any]:
  private val builtins = Array[Builtin](
    Exit,
    Stringify,
    Cd,
    Truthy,
    Numify
  )

  override def visitBinaryExpr(expr: Binary): Any =
    lazy val left = expr.left.accept(this)
    lazy val right = expr.right.accept(this)

    expr.operator.lexeme match
      case TokenType.Plus         => asNum(left) + asNum(right)
      case TokenType.Minus        => asNum(left) - asNum(right)
      case TokenType.Slash        => asNum(left) / asNum(right)
      case TokenType.Star         => asNum(left) * asNum(right)
      case TokenType.Less         => asNum(left) < asNum(right)
      case TokenType.LessEqual    => asNum(left) <= asNum(right)
      case TokenType.Greater      => asNum(left) > asNum(right)
      case TokenType.GreaterEqual => asNum(left) >= asNum(right)
      case TokenType.And          => asBool(left) && asBool(right)
      case TokenType.Or           => asBool(left) || asBool(right)
      case TokenType.EqualEqual   => left == right

      // unused, stops the compiler from warning about inexhaustive pattern matching
      case _ => 0

  override def visitLiteralExpr(expr: Literal): Any = expr.value
  override def visitUnaryExpr(expr: Unary): Any = 2
  override def visitGroupingExpr(expr: Grouping): Any = expr.expr.accept(this)
  override def visitCommandExpr(expr: Command): Any =
    val args = expr.args.accept(this)
    // this should only ever be a length of 0 or 1
    val possibleBuiltins = builtins.filter(_.name == expr.command)

    if possibleBuiltins.isEmpty then
      // execute system command if no builtin was found
      val command =
        if args.isInstanceOf[ScalieNull] then (expr.command, Array[String]())
        else if args.isInstanceOf[Array[Any]] then
          (expr.command, args.asInstanceOf[Array[Any]].map(_.toString()))
        else (expr.command, Array(args.toString()))

      if expr.silent then
        os.call(
          cmd = command,
          cwd = wd,
          stdin = os.Inherit,
          stderr = os.Inherit
        ).out
          .lines()
          .mkString(" ")
      else
        os.call(
          cmd = command,
          cwd = wd,
          stdin = os.Inherit,
          stdout = os.Inherit,
          stderr = os.Inherit
        ).exitCode
    else // execute a builtin if it exists
      possibleBuiltins.map { builtin =>
        if args.isInstanceOf[Array[Any]] then
          val arr = args.asInstanceOf[Array[Any]]
          if arr.length != builtin.arity && builtin.arity != -1 then
            arityError(builtin, arr.length)
          else builtin.execute(arr, expr.silent)
        else if args.isInstanceOf[ScalieNull] then
          if builtin.arity != 0 && builtin.arity != -1 then
            arityError(builtin, 0)
          else builtin.execute(Array(), expr.silent)
        else
          builtin.arity match
            case 1  => builtin.execute(Array(args), expr.silent)
            case -1 => builtin.execute(Array(args), expr.silent)
            case 0  => arityError(builtin, 0)
      }.head

  override def visitArrayExpr(expr: Arr): Any =
    expr.arr.map(_.accept(this))

  private def asString(obj: Any): String =
    if obj.isInstanceOf[String] then obj.asInstanceOf[String]
    else throw RuntimeException(s"Object '${obj.toString}' is not a boolean.")

  private def asNum(obj: Any): Double =
    if obj.isInstanceOf[Double] then obj.asInstanceOf[Double]
    else throw RuntimeException(s"Object '${obj.toString}' is not a boolean.")

  private def asBool(obj: Any): Boolean =
    if obj.isInstanceOf[Boolean] then obj.asInstanceOf[Boolean]
    else throw RuntimeException(s"Object '${obj.toString}' is not a boolean.")

  private def arityError(builtin: Builtin, amt: Int): Unit =
    throw RuntimeException(
      s"Builtin '${builtin.name}' requires ${builtin.arity} arguments but found $amt."
    )
