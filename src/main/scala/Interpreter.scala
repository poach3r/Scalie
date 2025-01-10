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
    Cd
  )

  override def visitBinaryExpr(expr: Binary): Any =
    val left = expr.left.accept(this)
    val right = expr.right.accept(this)

    expr.operator.lexeme match
      case TokenType.Plus =>
        if left.isInstanceOf[String] && right.isInstanceOf[String] then
          left.asInstanceOf[String].concat(right.asInstanceOf)
        else if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] + right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot add '$left' and '$right'.")

      case TokenType.Minus =>
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] - right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot substract '$right' from '$left'.")

      case TokenType.Slash =>
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] / right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot divide '$left' by '$right'.")

      case TokenType.Star =>
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] * right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot multiply '$left' by '$right'.")

      case TokenType.Less =>
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] < right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot compare '$left' and '$right'.")

      case TokenType.LessEqual =>
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] <= right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot compare '$left' and '$right'.")

      case TokenType.Greater =>
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] > right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot compare '$left' and '$right'.")

      case TokenType.GreaterEqual =>
        if left.isInstanceOf[Double] && right.isInstanceOf[Double] then
          left.asInstanceOf[Double] >= right.asInstanceOf[Double]
        else throw RuntimeException(s"Cannot compare '$left' and '$right'.")

      case TokenType.EqualEqual => left == right

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
          builtin.execute(arr, expr.silent)
        else if args.isInstanceOf[ScalieNull] then
          if builtin.arity != 0 && builtin.arity != -1 then
            arityError(builtin, 0)
          else builtin.execute(Array(), expr.silent)
        else if builtin.arity != 1 && builtin.arity != -1 then
          arityError(builtin, 1)
        else builtin.execute(Array(args), expr.silent)
      }.head

  override def visitArrayExpr(expr: Arr): Any =
    expr.arr.map(_.accept(this))

  private def arityError(builtin: Builtin, amt: Int): Unit =
    throw RuntimeException(
      s"Builtin '${builtin.name}' requires ${builtin.arity} arguments but found $amt."
    )
