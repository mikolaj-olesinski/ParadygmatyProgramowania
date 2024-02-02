trait Low
trait High extends Low
trait Super extends High

trait User[+Read <: Low, +Write <: Low]:
  def secret : String

class Terminal[Read <: Low, Write <: Read] (private var secretMessage : String):
  def read(user : User[Read, _]): String = secretMessage
  def write(user : User[_, Write]) : Unit = secretMessage = user.secret

class HighUser(val secret: String) extends User[High, High]
class SuperUser(val secret: String) extends User[Super, Super]
class LowUser(val secret: String) extends User[Low, Low]

val highUser = HighUser("highSecretMessage")
val superUser = SuperUser("superSecretMessage")

val terminal = Terminal[Super, Super]("initialSecretMessage")

terminal.write(superUser)
terminal.read(superUser)