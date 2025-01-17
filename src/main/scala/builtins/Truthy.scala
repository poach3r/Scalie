package org.poach3r.builtins

import org.poach3r.builtins.Builtin

object Truthy extends Builtin("truthy", 1):
  override def execute(args: Array[Any], silent: Boolean): Any =
    val truthy =
      if args(0).isInstanceOf[Boolean] then args(0).asInstanceOf
      else if args(0).isInstanceOf[String] then
        val str = args(0).asInstanceOf[String]
        if str == "true" then true
        else false
      else false

    if silent then println(truthy)
    truthy
