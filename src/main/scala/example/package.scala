package object example {
  import scala.io.AnsiColor._
  val strGreen = (str: String) => s"${GREEN}$str${RESET}"
  val strRed = (str: String) => s"${RED}$str${RESET}"

}
