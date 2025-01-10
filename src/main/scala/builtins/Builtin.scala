package org.poach3r.builtins

trait Builtin(val name: String, val arity: Int):
  def execute(args: Array[Any], silent: Boolean): Any
