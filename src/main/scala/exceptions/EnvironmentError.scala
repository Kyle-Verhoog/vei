package exceptions

final case class EnvironmentError(
    private val message: String = "Encounter an environment error",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)
