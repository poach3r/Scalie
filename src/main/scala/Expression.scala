package org.poach3r

object Expression:
  trait Expr:
    def accept[T](visitor: Visitor[T]): T

  trait Visitor[T]:
    def visitLiteralExpr(expr: Literal): T
    def visitUnaryExpr(expr: Unary): T
    def visitBinaryExpr(expr: Binary): T
    def visitGroupingExpr(expr: Grouping): T
    def visitCommandExpr(expr: Command): T
    def visitArrayExpr(expr: Arr): T

  case class Literal(value: Any) extends Expr:
    override def accept[T](visitor: Visitor[T]): T =
      visitor.visitLiteralExpr(this)

  case class Unary(operator: Token, expr: Expr) extends Expr:
    override def accept[T](visitor: Visitor[T]): T =
      visitor.visitUnaryExpr(this)

  case class Binary(operator: Token, left: Expr, right: Expr) extends Expr:
    override def accept[T](visitor: Visitor[T]): T =
      visitor.visitBinaryExpr(this)

  case class Grouping(expr: Expr) extends Expr:
    override def accept[T](visitor: Visitor[T]): T =
      visitor.visitGroupingExpr(this)

  case class Command(command: String, args: Expr, silent: Boolean) extends Expr:
    override def accept[T](visitor: Visitor[T]): T =
      visitor.visitCommandExpr(this)

  case class Arr(arr: Array[Expr]) extends Expr:
    override def accept[T](visitor: Visitor[T]): T =
      visitor.visitArrayExpr(this)
