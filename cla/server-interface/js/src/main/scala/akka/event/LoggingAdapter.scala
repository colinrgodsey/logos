package akka.event

trait LoggingAdapter {
  def error(msg: => String)
  def error(cause: Throwable, msg: => String)
  def warning(msg: => String)
  def warning(cause: Throwable, msg: => String)
  def info(msg: => String)
  def debug(msg: => String)

  def isDebugEnabled: Boolean
  def isWarningEnabled: Boolean
  def isErrorEnabled: Boolean
  def isInfoEnabled: Boolean
}
