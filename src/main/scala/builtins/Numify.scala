package org.poach3r.builtins

import org.poach3r.builtins.Builtin

object Numify extends Builtin("numify", 1):
  override def execute(args: Array[Any], silent: Boolean): Any =
    val numy =
      if args(0).isInstanceOf[Double] then args(0).asInstanceOf[Double]
      else
        args(0).toString.toDoubleOption.getOrElse(({ () =>
          throw RuntimeException(
            s"Cannot convert object '${args(0).toString}' to number."
          )
          0.0
        })())

    if !silent then println(numy)
    numy
