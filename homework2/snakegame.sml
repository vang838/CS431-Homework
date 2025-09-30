structure R = Random

(* used for testing *)
val globalGenerator = R.rand(42, 99) 
(* val seed = R.rand(0, 0)
val globalGenerator = R.rand(seed, 0) *)

fun rollDice () = ((R.randInt globalGenerator) mod 6) + 1 

val board = [
    0, 1, 0, (~2), 0, 3, 0, (~1), 0, 2, 
    0, 0, (~3), 0, 1, 0, (~2), 0, 0, 0
]

(* Main Function *)
fun play position board =
    if position >= length board then
        print("You have reached the end of the board!\nYou win!\nGame Overr....")
    
    else
        let
            val _ = print("Press the ENTER key to roll the dice.\n")
            val _ = TextIO.inputLine TextIO.stdIn

            val roll = rollDice ()
            val newPosition = position + roll 

            val adjustment = List.nth( board, Int.min(newPosition, length board-1) )
            val finalPosition = newPosition + adjustment
            val _ = print("You rolled " ^ Int.toString(roll) ^ ".\nYour new position is " ^ Int.toString(finalPosition) ^ ".\n")

        in
            play finalPosition board
        end

val _ = play 0 board