package org.poach3r.builtins

import org.poach3r.Main.wd

object Cd extends Builtin("cd", 1):
  override def execute(args: Array[Any], silent: Boolean): Any =
    val cookedStr = args(0)
      .toString()
      .replaceAllLiterally("~/", s"${os.home.toString}/")
      .replaceAllLiterally("./", s"$wd/")
      .replaceAllLiterally("../", s"${wd / os.up}")

    wd = os.Path(cookedStr)
    0
