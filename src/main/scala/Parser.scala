package org.poach3r

import os.Path
import scala.annotation.tailrec

object Parser:
  /** Recursively parses an array of tokens creating an array of expressions.
    * @param tokens
    *   The tokens to be parsed
    * @param index
    *   The current token being parsed
    * @param expressions
    *   The already parsed expressions
    * @return
    */
  @tailrec
  def parse(
      tokens: Array[Token],
      index: Int = 0,
      expressions: Array[Expression.Expr] = Array()
  ): Array[Expression.Expr] =
    if index >= tokens.length then return expressions

    val result = equality(tokens, index)
    parse(tokens, result._2, expressions.appended(result._1))

  private def equality(
      tokens: Array[Token],
      index: Int
  ): (Expression.Expr, Int) =
    val expr = comparison(tokens, index)

    if expr._2 < tokens.length && isMatching(
        tokens(expr._2),
        TokenType.EqualEqual
      )
    then
      val right = equality(tokens, expr._2 + 1)
      (Expression.Binary(tokens(expr._2), expr._1, right._1), right._2)
    else expr

  private def comparison(
      tokens: Array[Token],
      index: Int
  ): (Expression.Expr, Int) =
    val expr = term(tokens, index)

    if expr._2 < tokens.length && isMatching(
        tokens(expr._2),
        TokenType.Greater,
        TokenType.GreaterEqual,
        TokenType.Less,
        TokenType.LessEqual
      )
    then
      val right = comparison(tokens, expr._2 + 1)
      (Expression.Binary(tokens(expr._2), expr._1, right._1), right._2)
    else expr

  private def term(
      tokens: Array[Token],
      index: Int
  ): (Expression.Expr, Int) =
    val expr = factor(tokens, index)

    if expr._2 < tokens.length && isMatching(
        tokens(expr._2),
        TokenType.Plus,
        TokenType.Minus
      )
    then
      val right = term(tokens, expr._2 + 1)
      (Expression.Binary(tokens(expr._2), expr._1, right._1), right._2)
    else expr

  private def factor(
      tokens: Array[Token],
      index: Int
  ): (Expression.Expr, Int) =
    val expr = primary(tokens, index)

    if expr._2 < tokens.length && isMatching(
        tokens(expr._2),
        TokenType.Star,
        TokenType.Slash
      )
    then
      val right = factor(tokens, expr._2 + 1)
      (Expression.Binary(tokens(expr._2), expr._1, right._1), right._2)
    else expr

  private def primary(
      tokens: Array[Token],
      index: Int
  ): (Expression.Expr, Int) =
    if isMatching(tokens(index), TokenType.Command) then
      val arguments = equality(tokens, index + 1)
      (
        Expression.Command(
          tokens(index).literal.asInstanceOf[String],
          arguments._1
        ),
        arguments._2 + 1
      )
    else if isMatching(tokens(index), TokenType.LeftBracket) then
      val exprs = getUntilRightBracket(tokens, index + 1)
      (Expression.Arr(exprs._1), exprs._2)
    else if isMatching(tokens(index), TokenType.LeftParen) then
      val expr = equality(tokens, index + 1)

      if isMatching(tokens(expr._2), TokenType.RightParen) then
        throw RuntimeException("Expected ')' at end of grouping expression.")

      (Expression.Grouping(expr._1), expr._2 + 1)
    else if isMatching(tokens(index), TokenType.Eof) then
      (Expression.Literal(ScalieNull()), index + 1)
    else (Expression.Literal(tokens(index).literal), index + 1)

  private def isMatching(token: Token, types: TokenType*): Boolean =
    types.filter { tType =>
      tType == token.lexeme
    }.nonEmpty

  @tailrec
  def getUntilRightBracket(
      tokens: Array[Token],
      index: Int,
      exprs: Array[Expression.Expr] = Array()
  ): (Array[Expression.Expr], Int) =
    if tokens.length <= index || isMatching(
        tokens(index),
        TokenType.RightBracket
      )
    then return (exprs, index + 1)

    val expr = equality(tokens, index)
    getUntilRightBracket(tokens, expr._2, exprs.appended(expr._1))