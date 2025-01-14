package org.poach3r

import scala.annotation.tailrec

import org.poach3r.Token

object Scanner:
  /** Recursively creates an array of tokens based on text input.
    * @param tokens
    *   The already parsed tokens
    * @param text
    *   The full text to be scanned
    * @param index
    *   The current element of `text` being scanned
    * @return
    */
  @tailrec
  def scan(
      text: String,
      index: Int = 0,
      tokens: Array[Token] = Array()
  ): Array[Token] =
    if index == text.length() then
      return tokens.appended(Token(TokenType.Eof, 0, index))

    val newTokenAndIndex = scanChar(text, tokens, index)

    if newTokenAndIndex._1.isDefined then
      scan(
        text.trim(),
        newTokenAndIndex._2,
        tokens.appended(newTokenAndIndex._1.get)
      )
    else scan(text.trim(), newTokenAndIndex._2, tokens)

  private def scanChar(
      text: String,
      tokens: Array[Token],
      index: Int
  ): (Option[Token], Int) =
    text(index) match
      case ' ' => (None, index + 1)
      case '.' => (Some(Token(TokenType.Dot, '.', index)), index + 1)
      case ',' => (Some(Token(TokenType.Comma, ',', index)), index + 1)
      case '"' =>
        val str = getString(text, index + 1, '"')
        (Some(Token(TokenType.String, str, index)), index + 2 + str.length())
      case ';' => (None, index + 1)
      case '(' => (Some(Token(TokenType.LeftParen, '(', index)), index + 1)
      case ')' => (Some(Token(TokenType.RightParen, ')', index)), index + 1)
      case '[' => (Some(Token(TokenType.LeftBracket, '[', index)), index + 1)
      case ']' => (Some(Token(TokenType.RightBracket, ']', index)), index + 1)
      case '+' => (Some(Token(TokenType.Plus, '+', index)), index + 1)
      case '-' => (Some(Token(TokenType.Minus, '-', index)), index + 1)
      case '/' => (Some(Token(TokenType.Slash, '/', index)), index + 1)
      case '*' => (Some(Token(TokenType.Star, '*', index)), index + 1)
      case '$' => (Some(Token(TokenType.Dollar, '$', index)), index + 1)
      case '!' => (Some(Token(TokenType.Exclamation, '!', index)), index + 1)
      case '|' => (Some(Token(TokenType.Or, '|', index)), index + 1)
      case '&' => (Some(Token(TokenType.And, '&', index)), index + 1)
      case '=' =>
        if isMatching(text, '=', tokens, index) then
          (Some(Token(TokenType.EqualEqual, "==", index)), index + 2)
        else (Some(Token(TokenType.Equal, '=', index)), index + 1)
      case '<' =>
        if isMatching(text, '=', tokens, index) then
          (Some(Token(TokenType.LessEqual, "<=", index)), index + 2)
        else (Some(Token(TokenType.Less, '<', index)), index + 1)
      case '>' =>
        if isMatching(text, '=', tokens, index) then
          (Some(Token(TokenType.GreaterEqual, ">=", index)), index + 2)
        else (Some(Token(TokenType.Greater, '>', index)), index + 1)
      case char =>
        if char >= '0' && char <= '9' then
          val num = getNum(text, index)
          (
            Some(Token(TokenType.Num, num.toDouble, index)),
            index + num.length()
          )
        else
          val str = getIdentifier(text, index)
          (
            Some(
              Token(
                TokenType.Command,
                str.replaceAllLiterally("\\n", "\n"),
                index
              )
            ),
            index + str.length()
          )

  private def isMatching(
      text: String,
      char: Char,
      tokens: Array[Token],
      index: Int
  ): Boolean =
    text.length() > index && text(index + 1) == char

  @tailrec
  private def getNum(
      text: String,
      index: Int,
      endText: String = ""
  ): String =
    if index == text.length() || text(index) < '0' || text(
        index
      ) > '9' && text(index) != '.'
    then endText
    else getNum(text, index + 1, endText + text(index))

  @tailrec
  private def getString(
      text: String,
      index: Int,
      endChar: Char,
      endText: String = ""
  ): String =
    if index == text.length() then return endText
    if text(index) == endChar then
      if index == 0 then return endText
      else if text(index - 1) != '\\' then return endText

    getString(text, index + 1, endChar, endText + text(index))

  @tailrec
  private def getIdentifier(
      text: String,
      index: Int,
      endText: String = ""
  ): String =
    if index == text.length() then endText
    else if (text(index) < '*' || text(index) > '9') && (text(
        index
      ) < 'A' || text(index) > 'Z') && (text(index) < 'a' || text(index) > 'z')
    then endText
    else getIdentifier(text, index + 1, endText + text(index))

  private def isNumeric(text: String): Boolean =
    text
      .map { char =>
        (char <= '9' && char >= '0') || char == '.'
      }
      .filter(_ == false)
      .length == 0
