# Haskell-Snake
command line snake game in haskell with pixelated lettering


How to play:

Open terminal and navigate to the project folder
~> ghci
ghci> :set -package mtl # if you see an error instructing you to do this
ghci> :l SnakeGame.hs
ghci> main

This will open the game interface, which will provide brief instructions regarding snake controls, and prompt the user to press SPACE to begin.

The snake is controlled using the IJKL keys instead of the UP, LEFT, DOWN, RIGHT arrow keys
move the snake around the field, avoiding collisions with the walls and with itself, and try to 'eat' as much as the food as you can

Enjoy!