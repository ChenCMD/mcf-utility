package com.github.chencmd.mcfutility.utils

import java.nio.file.Paths

object Path {
  def relative(from: String, to: String): String = {
    Paths.get(from).relativize(Paths.get(to)).toString
  }

  def dirname(path: String): Option[String] = {
    Option(Paths.get(path).getParent).map(_.toString)
  }

  def join(paths: String*): String = {
    paths.tail.foldLeft(Paths.get(paths.head))((r, p) => r.resolve(p)).toString
  }
}
