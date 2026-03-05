val seed = LargeInt.toInt (Time.toSeconds (Time.now()) mod 1000000)
val gen = Random.rand(seed, seed+1)

fun rollDie () = Random.randRange(1,6) gen

val board = [0, 1, ~2, 0, ~1, 1, 0, ~5, 0, 0, 3, 6, ~5, 0]

fun playGame position board =
    if position >= List.length board then
        print("You have beaten this game, you have won.... nothing >:D\n")
    else
        let
            val _ = print("Press the ENTER key to roll the dice.\n")
            val _ = TextIO.inputLine TextIO.stdIn

            val roll = rollDie ()
            val _ = print("You rolled a " ^ Int.toString roll ^ "!\n")

            val newPosition = position + roll
            val distanceToFinalPosition =
                if newPosition < List.length board then List.nth(board, newPosition)
                else 0

            val finalPosition = newPosition + distanceToFinalPosition

            val cappedFinalPos =
                if finalPosition >= List.length board then List.length board - 1
                else finalPosition

            val _ =
                if distanceToFinalPosition = 0 then
                    print("You move to square " ^ Int.toString cappedFinalPos ^ ".\n")
                else if distanceToFinalPosition > 0 then
                    print("You hit a ladder! You move forward " ^
                          Int.toString distanceToFinalPosition ^
                          " spaces to square " ^
                          Int.toString cappedFinalPos ^ ".\n")
                else
                    print("Snake! You move backward " ^
                          Int.toString (~distanceToFinalPosition) ^
                          " spaces to square " ^
                          Int.toString cappedFinalPos ^ ".\n")
        in
            playGame finalPosition board
        end

val _ = playGame 0 board