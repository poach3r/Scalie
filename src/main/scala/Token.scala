package org.poach3r

case class Token(lexeme: TokenType, literal: Any, pos: Int)

enum TokenType:
  case LeftParen, RightParen
  case LeftBracket, RightBracket

  case Plus, Minus
  case Slash, Star, Percent

  case DoubleQuote, Quote
  case Dot, Comma
  case Num, String, Command

  case Exclamation, Dollar

  case And, Or
  case Equal, EqualEqual
  case Less, LessEqual
  case Greater, GreaterEqual

  case Semicolon, Eof
