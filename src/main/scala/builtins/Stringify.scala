package org.poach3r.builtins

object Stringify extends Builtin("stringify", -1):
  override def execute(args: Array[Any], silent: Boolean): Any =
    val stringy = args.map(_.toString()).mkString(" ")
    if !silent then println(stringy)
    stringy
