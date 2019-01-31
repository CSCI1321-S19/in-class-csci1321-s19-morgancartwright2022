package mud

class Player(
    private var location: Room,
    private var inv: List[Item]
  ) {
  
  def processCommand(command: String): String = {
    val words = command.split(" ")
    val result = words.head match {
      case "get" => {
        val item = location.getItem(words.tail.mkString(" "))
        if(item != None) {
          addToInv(item.head)
          "You pick up the " + item.head.name
        }
        else "There is no " + words.tail.mkString(" ") + " here."
      }
      case "drop" => {
        val item = getFromInv(words.tail.mkString(" "))
        if(item != None) {
          location.dropItem(item.head)
          "You drop the " + item.head.name
        }
        else "There is no " + words.tail.mkString(" ") + " in your inventory."
      }
      case "help" => "Commands\nn/e/s/w: Move north, east, south or west.\nget <item>: Take that item and put it in your inventory.\ndrop <item>: Drop that item on the ground.\nlook: View the current room's description.\ninv: Look at what you are carrying.\nhelp: Bring up this info again."
      case "inv" => invListing()
      case "inventory" => invListing()
      case "look" => location.description()
      case _ if("neswud".indexOf(words.head(0)) != -1) => {
        val succ = move(words.head.substring(0, 1))
        if(succ) location.description()
        else "You cannot move in that direction."
      }
      case _ => "You cannot do that."
    }
    "\n" + result
  }
  
  def getFromInv(itemName: String): Option[Item] = {
    val item = inv.find(i => itemName == i.name)
    if(item != None) inv = inv.patch(inv.indexOf(item.head), Nil, 1)
    item
  }
  
  def addToInv(item: Item): Unit = {
    inv = item :: inv
  }
  
  def invListing(): String = {
    "Inventory:\n" + inv.map(i => i.name + " - " + i.desc).mkString("\n")
  }
  
  def move(dir: String): Boolean = {
    val n = dir match {
      case "n" => 0
      case "e" => 1
      case "s" => 2
      case "w" => 3
      case "u" => 4
      case "d" => 5
    }
    val newLoc = location.getExit(n)
    if(newLoc != None) location = newLoc.head
    newLoc != None
  }
}