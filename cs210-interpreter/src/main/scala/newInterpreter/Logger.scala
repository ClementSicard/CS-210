package newInterpreter

object Logger
  private var logging = false
  private var indentation = 0
  def on(): Unit =
    logging = true
    indentation = 0
  def off(): Unit =
    logging = false
  def indent(): Unit =
    indentation = indentation + 1
  def unindent(): Unit =
    indentation = indentation - 1
  def log(s: => String): Unit =
    if logging then println("|  " * indentation + s)
