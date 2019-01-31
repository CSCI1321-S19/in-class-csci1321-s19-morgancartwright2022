package mud

object Main {
  def main(args: Array[String]): Unit = {
    val player = new Player(Room.rooms(0), Nil)
    
    def act(input: String): Unit = {
      if(input != "exit") {
        val output = player.processCommand(input)
        println(output)
        act(scala.io.StdIn.readLine("Enter a command: ").toLowerCase())
      }
      else
        println("Goodbye!")
    }
    
    act("look")
  }
}