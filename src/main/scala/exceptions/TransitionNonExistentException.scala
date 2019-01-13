package exceptions

final case class TransitionNonExistentException(
    private val message: String = "Transition does not exist",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)
