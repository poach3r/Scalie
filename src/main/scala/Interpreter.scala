package org.poach3r

import scala.annotation.tailrec

import org.poach3r.Expression.*
import os.proc

object Interpreter extends Expression.Visitor[Any]:
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

    expr.command match
      case "exit" => System.exit(0)
      case _ =>
        if args.isInstanceOf[ScalieNull] then
          os.call(
            cmd = expr.command,
            stdin = os.Inherit,
            stdout = os.Inherit,
            stderr = os.Inherit
          )
        else if args.isInstanceOf[Array[Any]] then
          os.call(
            cmd =
              (expr.command, args.asInstanceOf[Array[Any]].map(_.toString())),
            stdin = os.Inherit,
            stdout = os.Inherit,
            stderr = os.Inherit
          )
        else
          os.call(
            cmd = (expr.command, args.toString()),
            stdin = os.Inherit,
            stdout = os.Inherit,
            stderr = os.Inherit
          )

  override def visitArrayExpr(expr: Arr): Any =
    expr.arr.map(_.accept(this))
