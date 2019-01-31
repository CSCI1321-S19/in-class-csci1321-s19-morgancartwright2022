package mud

class Room(
    val name: String,
    val desc: String,
    private var items: List[Item],
    private val exits: Array[Int]
    ) {
  
  def description(): String = {
      def cardinalDir(dir: Int): String = {
        dir match {
          case 0 => "north"
          case 1 => "east"
          case 2 => "south"
          case 3 => "west"
          case 4 => "up"
          case 5 => "down"
          case _ => ""
        }
      }
      val exitsDesc = exits.filter(_ != -1).map(e => cardinalDir(exits.indexOf(e))).mkString(", ")
      val itemsDesc = if(items.length > 0) items.map(_.name).mkString(", ") else "None"
      name + "\n" + desc + "\nExits: " + exitsDesc + "\nItems: " + itemsDesc
  }
  
  def getExit(dir: Int): Option[Room] = {
    if(exits(dir) == -1) None
    else Some(Room.rooms(exits(dir)))
  }
  
  def getItem(itemName: String): Option[Item] = {
    val item = items.find(i => itemName == i.name)
    if(item != None) items = items.patch(items.indexOf(item.head), Nil, 1)
    item
  }
  
  def dropItem(item: Item): Unit = {
    items = item :: items
  }
  
}
object Room {
  val rooms = readRooms("map.txt")
  
  def readRoom(lines: Iterator[String]): Room = {
    val name = lines.next.trim
    val desc = lines.next.trim
    val items = List.fill(lines.next.trim.toInt) {
      Item(lines.next, lines.next)
    }
    val exits = lines.next.split(",").map(_.toInt)
    new Room(name, desc, items, exits)
  }
  
  def readRooms(srcFile: String): Array[Room] = {
    val source = scala.io.Source.fromFile(srcFile)
    val lines = source.getLines()
    val rooms = Array.fill(lines.next.trim.toInt)(readRoom(lines))
    source.close()
    rooms
  }
}