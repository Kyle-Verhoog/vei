package exceptions

final case class NoTokenOnAcceptingStateException(
    private val message: String =
      "Accepting state has no token associated with it",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)
