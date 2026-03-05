val seed = LargeInt.toInt (Time.toSeconds (Time.now()) mod 1000000);
val gen = Random.rand(seed);
fun rollDie() = Random.randRange(1, 6) gen;
val board [ 0, 1, ~2, 0, ~1, 1, 0, ~5, 0, 0, 3, 6, ~5, 0]
