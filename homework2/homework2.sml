val seed = LargeInt.toInt (Time.toSeconds (Time.now()) mod 1000000)
val gen = Random.rand(seed)
fun rollDie() = Random.randRange(1, 6) gen
val board [ 0, 1, ~2, 0, ~1, 1, 0, ~5, 0, 0, 3, 6, ~5, 0]

fun playGame position board =
    if position >= 14 then
        "You have beaten this game, you have won.... nothing >:D"
    else
        let 
            val _ = print("Press the ENTER key to roll the dice.\n")
            val _ TextIO.inputLine TextIO.stdIn

            val roll = rollDie()
            val _ = print("You rolled a " ^Int.toString roll^ "!\n")
            
            val newPosition = position+roll
            val distanceToFinalPosition = 
                if newPosition < List.length board then List.nth(board, newPosition)
                else 0
            
            val finalPosition = newPosition + distanceToFinalPosition
            
            val _ =
                if adjustment = 0 then 
                    print("You move to square " ^Int.toString newPosition^ ".\n")
                else if adjustment > 0 then
                    print("You hit a ladder!  You move forward " ^Int.toString distanceToFinalPosition^ "spaces\n")
                else
                    print("Snake!  You move backward " ^Int.toString ~distanceToFinalPosition^ "spaces\n")
                
        in
            playGame finalPosition board
        end

val _ = playGame 0

