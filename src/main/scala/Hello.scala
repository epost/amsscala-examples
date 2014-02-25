/*
 * This file should be in src/main/scala/nl/shinsetsu/example if
 * you want to get official, but having it in src/main/scala is a
 * bit simpler to start with.
 */
package nl.shinsetsu.example

object Main {
  
  // your program's entry point.
  def main(commandLineArguments: Array[String]): Unit = {
    println("The value of f at 3 is " + f(3))
  }

  // an example function -- the compiler infers that its return type is Int
  def f(n: Int) = 2*n
}
