package org.poach3r

import org.poach3r.Expression.*

object Printer extends Expression.Visitor[String]:
  override def visitBinaryExpr(expr: Binary): String =
    val left = expr.left.accept(this)
    val right = expr.right.accept(this)

    s"($left ${expr.operator.literal} $right)"

  override def visitLiteralExpr(expr: Literal): String = fuck(expr.value)
  override def visitUnaryExpr(expr: Unary): String = ""
  override def visitGroupingExpr(expr: Grouping): String =
    s"(${expr.expr.accept(this)})"
  override def visitCommandExpr(expr: Command): String =
    s"(${expr.command}: ${expr.args.accept(this)})"

  def fuck(x: Any): String =
    if x.isInstanceOf[Array[Any]] then
      x.asInstanceOf[Array[Any]].map(fuck).mkString("[", ", ", "]")
    else if x.isInstanceOf[Expr] then fuck(x.asInstanceOf[Expr].accept(this))
    else x.toString()

  override def visitArrayExpr(expr: Arr): String = ???
