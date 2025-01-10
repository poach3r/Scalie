package org.poach3r.builtins

object Exit extends Builtin("exit", 0):
  override def execute(args: Array[Any], silent: Boolean): Any =
    System.exit(0)
