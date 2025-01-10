package org.poach3r

case class Token(lexeme: TokenType, literal: Any, pos: Int)

enum TokenType:
  case LeftParen, RightParen
  case LeftBracket, RightBracket
  case Plus, Minus
  case Slash, Star
  case DoubleQuote, Quote
  case Dot, Comma
  case And, Or
  case Num, String, Command
  case Equal, EqualEqual
  case Less, LessEqual
  case Greater, GreaterEqual
  case Semicolon, Eof
