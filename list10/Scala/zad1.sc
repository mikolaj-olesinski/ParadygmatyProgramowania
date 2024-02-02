trait Low
trait High extends Low
trait Super extends High

trait User[+Read <: Low, +Write <: Low]:
  def secret : String
  def changeSecret(newSecret : String) : Unit

class Terminal[Read <: Low, Write <: Read] (private var secretMessage : String):
  def read(user : User[Read, _]): String = secretMessage
  def write(user : User[_, Write]) : Unit = secretMessage = user.secret



class BasicUser[+R <: Low, +W <: Low](private var _secret: String) extends User[R, W]:
  def secret: String = _secret
  def changeSecret(newSecret: String): Unit = _secret = newSecret
  
class LowUser(secret: String) extends BasicUser[Low, Low](secret)
class HighUser(secret: String) extends BasicUser[High, High](secret)
class SuperUser(secret: String) extends BasicUser[Super, Super](secret)
class LowSuperUser(secret: String) extends BasicUser[Low, Super](secret)
class SuperLowUser(secret: String) extends BasicUser[Super, Low](secret)

val lowUser = LowUser("lowUserSecretMessage")
val highUser = HighUser("highUserSecretMessage")
val superUser = SuperUser("superUserSecretMessage")
val lowSuperUser = LowSuperUser("lowSuperUserSecretMessage")
val superLowUser = SuperLowUser("superLowUserSecretMessage")

val SuperSuperterminal = Terminal[Super, Super]("initialSecretMessage")
val HighHighTerminal = Terminal[High, High]("initialSecretMessage")
val LowLowTerminal = Terminal[Low, Low]("initialSecretMessage")

SuperSuperterminal.write(superUser)
SuperSuperterminal.read(superUser)

SuperSuperterminal.write(lowSuperUser)
SuperSuperterminal.read(superUser)

LowLowTerminal.write(lowUser)
LowLowTerminal.read(lowUser)

LowLowTerminal.write(HighUser("TEST"))
LowLowTerminal.read(lowUser)

SuperSuperterminal.read(lowSuperUser)
val HighLowTerminal = Terminal[High, Low]("initialSecretMessage")