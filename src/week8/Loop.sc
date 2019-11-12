object Loop {

  // Define WHILE function
  def WHILE(condition: => Boolean)(command: => Unit): Unit = {
    if (condition) {
      command
      WHILE(condition)(command)
    } else {}
  }

  /**  Define REPEAT until condition is TRUE
    *     REPEAT {
    *       command
    *     } (condition)
    */
  def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else REPEAT(command)(condition)
  }

  // REPEAT ( command ) UNTIL (condition)


}