case class Item(id: String, name: String, description: String)

case class NPC(id: String, name: String, dialogue: Map[String, String], itemWanted: Option[String] = None, rewardItem: Option[Item] = None)

case class Room(id: String, name: String, description: String, exits: Map[String, String], items: List[Item], npcs: List[NPC] = List())

case class GameState(
                      currentRoomID: String,
                      inventory: List[Item],
                      world: Map[String, Room],
                      flags: Set[String], // track puzzles solved by user.
                      message: String,
                      isFinished: Boolean=false,
                      turnCount: Int=0, // time tracking for dynamic events
                      health: Int=100,  // time tracking for dynamic events
                      hunger: Int=100)

sealed trait Command
case object Quit extends Command
case object Look extends Command
case object Inventory extends Command
case object Help extends Command
case object Unknown extends Command
case class Move(direction: String) extends Command
case class Take(itemName: String) extends Command
case class Use(itemName: String) extends Command
case class Talk(npcName: String) extends Command
case class Give(npcName: String, itemName: String) extends Command

object Main {
  def initGame(): GameState = {
    val ration = Item("ration", "Ration Bar", "A vacuum-sealed nutritious bar found from the crash site.  Restore hunger")
    val humanArm = Item("arm", "Severed Arm", "The arm of your crewmate, Matthew. Sometimes I feel like I see his fingers move.")
    val battery = Item("battery", "Power Cell", "A heavy, radioactive battery scavenged from the ship's core")
    val alienArtifact = Item("artifact", "Glowing Orb", "It hums with mysterious energy.  A hole in the ship looks to be the same size as the orb")
    val cookedMeat = Item("meat", "Cooked Steak", "It looks similar to a medium rare steak back on Earth")

    val robot = NPC(
      id = "robot",
      name = "Unit 902",
      dialogue = Map(
        "default" -> "BEEP. BEEP. BOOP. ERROR DETECTED: CRASH DATA CORRUPTED. POWER CELL REQUIRED FOR NAVIGATION",
        "helped_robot" -> "BEEP. BEEP. BEEP. SYSTEMS ONLINE. THANK YOU, LIFEFORM. ANALYZING TERRAIN...  ANALYZING TERRAIN...  TERRAIN ANALYZED: GO NORTH TO FIND CIVILIZATION"
      ),
      itemWanted = Some("battery"),
      rewardItem = Some(alienArtifact)
    )

    val travelingTrader = NPC(
      id = "trader",
      name = "Traveling Merchant",
      dialogue = Map(
        "default" -> "Greetings, Ugly Creature!  I collect organic materials.  Do you have anything of interest?",
        "helped_trader" -> "Oh, what a delicious-- I mean, fascinating item!  Here, take this battery I found"
      ),
      itemWanted = Some("arm"),
      rewardItem = Some(cookedMeat)
    )

    val rooms = Map(
      "crash_site" -> Room(
        "crash_site", "Crash Site", "Smoke rises from the twisted metal of your ship. Your crew is dead.",
        Map("north" -> "gas_field"),
        List(ration, ration, humanArm) // 2 rations, 1 arm
      ),
      "gas_field" -> Room(
        "gas_field", "Sulfuric Fields", "Yellow gas swirls around your ankles. It burns your lungs to breathe here.",
        Map("south" -> "crash_site", "north" -> "ruins", "east" -> "cave"),
        List()
      ),
      "cave_entrance" -> Room(
        "cave_entrance", "Crystal Cave", "A dark opening in the rocks. The air is much cooler here",
        Map("west" -> "gas_field", "east" -> "cave"),
        List(battery),
        npcs = List(travelingTrader) // Trader starts here but moves
      ),
      "cave" -> Room(
        "cave", "Crystal Cave", "The air is clear here. Crystals illuminate the walls.",
        Map("west" -> "cave_entrance"),
        List(battery),
        npcs = List(travelingTrader) // Trader starts here but moves
      ),
      "ruins_entrance" -> Room(
        "ruins_entrance", "Ancient Gates", "Giant rusted pillars mark the entrance to an old city.",
        Map("south" -> "gas_field", "north" -> "ruins_hall"),
        List(),
        npcs = List(robot)
      ),
      "ruins_hall" -> Room(
        "ruins", "Alien Ruins", "A long corridor with strange writing on the walls.  Possibly the language of a past civilization.",
        Map("south" -> "ruins_entrance", "north" -> "ruins"),
        List(),
        npcs = List(robot)
      ),
      "control_room" -> Room(
        "control_room", "Signal Control Center", "Advanced machinery that looks left behind by an advanced civilization.  It looks like there is a machine to signal back Earth for help",
        Map("south" -> "cave"),
        List()
      )
    )

    val introMessage =
      """
        |==================================================
        |                  CRASH LANDING
        |==================================================
        |You wake up in a daze, blood dripping from your face.
        |Memories flood back: the alarms, the fire, the scream of engines...
        |You have crash-landed on an unknown planet.
        |
        |Scanning the wreckage... No survivors found.
        |
        |MISSION UPDATE:
        |[CANCELLED] Original Objective: Discover Chance of Habitability
        |[ACTIVE]    New Objective:      Signal for Help
        |
        |--------------------------------------------------
        |COMMANDS:
        | - move [direction]      (e.g., 'move north', 'n')
        | - look                (examine surroundings - e.g. 'look')
        | - take [item]         (pickup item from ground - e.g. 'take power cell')
        | - use [item]          (consume an item - e.g. 'use steak')
        | - inventory           (check supplies and status e.g. 'inventory')
        | - talk [npc]          (talk to npc - e.g. 'talk merchant')
        | - give [npc] [item]   (trade an npc an item - e.g. 'give merchant arm')
        | - quit                (exit game e.g. 'quit')
        |
        |SURVIVAL TIPS:
        | - Hunger increases with every turn. Find food!
        | - Avoid toxic gas areas to preserve Health.
        |==================================================
        |""".stripMargin

    GameState(
      currentRoomID = "crash_site",
      inventory = List(),
      world = rooms,
      flags = Set(),
      message = introMessage,
    )
  }

  def parseCommand(input: String): Command = {
    val tokens = input.trim.toLowerCase.split("\\s+").toList
    tokens match {
      case "quit" :: Nil | "exit" :: Nil => Quit
      case "look" :: Nil | "l" :: Nil => Look
      case "inventory" :: Nil | "i" :: Nil => Inventory
      case "help" :: Nil => Help
      case "go" :: dir :: Nil => Move(dir)
      case "n" :: Nil | "north" :: Nil => Move("north")
      case "s" :: Nil | "south" :: Nil => Move("south")
      case "e" :: Nil | "east" :: Nil => Move("east")
      case "w" :: Nil | "west" :: Nil => Move("west")
      case "take" :: itemWords => Take(itemWords.mkString(" "))
      case "use" :: itemWords => Use(itemWords.mkString(" "))
      case "talk" :: npcWords => Talk(npcWords.mkString(" ").stripPrefix("to ").trim)
      case "give" :: npc :: itemWords => Give(npc, itemWords.mkString(" "))
      case _ => Unknown
    }
  }

  def updateState(state: GameState, command: Command): GameState = {
    val currentRoom = state.world(state.currentRoomID)

    command match {
      case Quit =>
        state.copy(message = "Game Over.", isFinished = true)

      case Help =>
        state.copy(message = "Commands: go [dir], look, take [item], use [item], talk [npc], give [npc] [item], inventory, quit")

      case Unknown =>
        state.copy(message = "I don't understand that command.")

      case Inventory =>
        val names = if (state.inventory.isEmpty) "Empty" else state.inventory.map(_.name).mkString(", ")
        state.copy(message = s"Backpack: $names\nHealth: ${state.health}%\nHunger: ${state.hunger}%")

      case Look =>
        val iDesc = if (currentRoom.items.isEmpty) "" else "\nItems: " + currentRoom.items.map(_.name).mkString(", ")
        val nDesc = if (currentRoom.npcs.isEmpty) "" else "\nNPCs: " + currentRoom.npcs.map(_.name).mkString(", ")
        state.copy(message = s"[${currentRoom.name}]\n${currentRoom.description}$iDesc$nDesc")

      case Move(direction) => handleMove(state, direction)

      case Take(itemName) =>
        currentRoom.items.find(_.name.toLowerCase == itemName) match {
          case Some(item) =>
            val newRoom = currentRoom.copy(items = currentRoom.items.filterNot(_ == item))
            val newWorld = state.world + (state.currentRoomID -> newRoom)
            state.copy(world = newWorld, inventory = item :: state.inventory, message = s"Taken: ${item.name}")
          case None =>
            state.copy(message = "You don't see that here.")
        }

      case Use(itemName) =>
        state.inventory.find(_.name.toLowerCase == itemName) match {
          case Some(item) =>
            if (item.id == "ration") {
              val newInv = state.inventory.filterNot(_ == item)
              val newHunger = Math.min(100, state.hunger + 30)
              state.copy(inventory = newInv, hunger = newHunger, message = "You eat the ration. It tastes like cardboard, but you feel better.")
            }
            else if (item.id == "meat") {
              state.copy(message = "You lift the steak to your mouth but hesitate.\nIt has a strong scent that might be useful for distracting a large creature later...\nBetter save it.") // Prevent user from eating steak since you need this to win the game. Yes I know it's a bad implementation but it's funny if I let the player eat the steak, they can't win the game lol.
            }
            else {
              state.copy(message = s"You can't use the ${item.name} right now.")
            }

          case None =>
            state.copy(message = "You don't have that.")
        }

      case Talk(npcName) =>
        currentRoom.npcs.find(_.name.toLowerCase.contains(npcName)) match {
          case Some(npc) =>
            val key = if (state.flags.contains(s"helped_${npc.id}")) s"helped_${npc.id}" else "default"
            val diag = npc.dialogue.getOrElse(key, "...")
            state.copy(message = s"""${npc.name}: "$diag" """)
          case None => state.copy(message = "No one by that name is here.")
        }

      case Give(npcName, itemName) =>
        val npcOpt = currentRoom.npcs.find(_.name.toLowerCase.contains(npcName))
        val itemOpt = state.inventory.find(_.name.toLowerCase.contains(itemName))

        (npcOpt, itemOpt) match {
          case (Some(npc), Some(item)) if npc.itemWanted.contains(item.id) =>
            val newInv = state.inventory.filterNot(_ == item)
            val newFlags = state.flags + s"helped_${npc.id}"

            val (finalInv, message) = npc.rewardItem match {
              case Some(reward) =>
                val baseMessage = s"He trades you a ${reward.name}"

                val hintMessage = if (reward.id == "meat") "\nTrader says: \"You can eat that now if you're starving, or save it... some creatures love fresh meat.\"" else ""
                (reward :: newInv, baseMessage + hintMessage)

              case None => (newInv, "He accepts your gift.")
            }
            state.copy(inventory = finalInv, flags = newFlags, message = s"You gave ${item.name} to ${npc.name}. $message")

          case (Some(npc), Some(item)) =>
            state.copy(message = s"${npc.name} doesn't want the ${item.name}.")

          case (None, _) => state.copy(message = "That person isn't here.")

          case (_, None) => state.copy(message = "You don't have that item.")
        }
    }
  }

  def applyDynamicEvents(state: GameState): GameState = {
    val newHunger = state.hunger - 2

    val (newHealth, gasMessage) =
      if (state.currentRoomID == "gas_field") { (state.health - 1, "\nWARNING: Toxic gas is burning your skin! -1 HP")}
      else {(state.health, "")}

    if( newHealth <= 0 ) { state.copy(health = 0, isFinished = true, message = state.message + gasMessage + "\n\nYou Died! The atmosphere was too toxic.")}

    else if ( newHunger <= 0 ) { state.copy( hunger = 0, isFinished = true, message = state.message + "\n\nYou Died! You starved to death")}

    else {
      val hungerMessage = newHunger match {
        case 20 => "\nYour stomach growls loudly..."
        case 15 => "\nYour are starving..."
        case 5 => "\nYou feel week from hunger..."
        case _ => ""
      }

      state.copy(turnCount = state.turnCount + 1, health = newHealth, hunger = newHunger, message = state.message + gasMessage + hungerMessage)
    }
  }

  // Helper function for complex movement
  def handleMove(state: GameState, direction: String): GameState = {
    val currentRoom = state.world(state.currentRoomID)

    currentRoom.exits.get(direction) match {
      case Some(nextId) =>
        // Move north runs into troll
        if (state.currentRoomID == "cave" && direction == "north") {
          state.inventory.find(_.id == "meat") match {
            case Some(meatItem) =>
              // Success: Troll distracted -> WIN CONDITION
              val nextRoom = state.world(nextId)
              val newInv = state.inventory.filterNot(_ == meatItem)

              // We combine the move AND the win message here
              state.copy(
                currentRoomID = nextId,
                inventory = newInv,
                isFinished = true, // End the game successfully
                message = s"You toss the Cooked Steak to the side. The Troll lunges for it, ignoring you completely!\n" +
                  s"You sneak past into the [${nextRoom.name}].\n\n" +
                  s"CONGRATULATIONS!\nYou enter the Signal Control Center and power up the ancient beacon.\n" +
                  s"A rescue ship acknowledges your signal. You have saved the mission!\n\nTHE END."
              )

            case None =>
              // Failure: Blocked
              state.copy(message = "A massive Alien Troll is sleeping in front of the north exit! You can't get past without distracting it with food...")
          }
        }
        // Basic Movement
        else {
          val nextRoom = state.world(nextId)
          val iDesc = if (nextRoom.items.isEmpty) "" else "\nItems: " + nextRoom.items.map(_.name).mkString(", ")
          val nDesc = if (nextRoom.npcs.isEmpty) "" else "\nNPCs: " + nextRoom.npcs.map(_.name).mkString(", ")

          state.copy(
            currentRoomID = nextId,
            message = s"You moved $direction.\n[${nextRoom.name}]\n${nextRoom.description}$iDesc$nDesc"
          )
        }

      case None =>
        state.copy(message = "You can't go that way.")
    }
  }
  @scala.annotation.tailrec
  def gameLoop(state: GameState): Unit = {
    println("-"*50)
    println(state.message)

    if( state.isFinished) return

    print("\n> ")
    val input = scala.io.StdIn.readLine()
    if( input != null ) {
      val cmd = parseCommand(input)
      val stateAfterCmd = updateState(state, cmd)


      val finalState = cmd match {
        case Unknown | Help | Quit => stateAfterCmd
        case _ => applyDynamicEvents(stateAfterCmd)
      }

      gameLoop(finalState)
    }
  }

  def main(args: Array[String]): Unit = {
    val initState = initGame()
    gameLoop(initState)
  }
}

