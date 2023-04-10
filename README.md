# TASM x86 Mastermind Game

This is a Mastermind game written in TASM x86 assembly language. The game has two modes: Player vs Bot and Training. In the Player vs Bot mode, the player tries to guess the secret code generated by the computer and the bot guesses the players secret code, while in the Training mode, the player tries to guess the bot's secret code.

## Getting Started

To run the game, you will need a TASM x86 assembler and linker. You can download  an extension for running TASM from this [link](https://marketplace.visualstudio.com/items?itemName=xsro.masm-tasm)

## How to Play

When you run the game, you will be prompted to choose the game mode: Player vs Bot or Training with the mouse. After choosing you will be able to play the game with the mouse (by clicking on the colors at the bottom of the screen) or with the keyboard (by clicking on the 1-6 numbers as displayed in the bottom of the screen).

## Player vs Bot Mode

In this mode, the computer generates a secret code consisting of four colors. The player creates a secret code for the bot to guss and then the player has to guess the secret code by entering four colors. The computer will then give feedback to the player in the form of two colors on a 2x2 grid: the number of correct colors in the correct position as the color black, and the number of correct colors in the wrong position as the color white.

The computer will use a brute-force algorithm to guess the secret code. It will try all possible combinations of colors and use the feedback from the player to eliminate impossible combinations.

For example, if the secret code is "blue, red, green, yellow" and the player guesses "red, green, blue, orange", the computer will give feedback 1 black and 2 whites, which means one color is in the correct position (red) and two colors are correct but in the wrong position (green, blue).

The player has a maximum of 8 attempts to guess the secret code. If the player succeeds in guessing the code within 8 attempts before the bot guesses the player's secret code, the player wins. Otherwise, the bot wins.

## Training Mode

In this mode, the bot generates a secret code consisting of four colors. Then the player has to guess the secret code by entering four colors. The bot will then give feedback to the player in the form of two colors on a 2x2 grid: the number of correct colors in the correct position as the color black, and the number of correct colors in the wrong position as the color white.

The player has 10 attempts to guess the secret code. If the player succeeds in guessing the code within 10 attempts, the player wins.

## Code Structure

The code is divided into several parts:

- The main menu - at the beginning of the game a main menu will be displayed with the 3 following buttons: 'Play', 'Train', 'Quit'. When a player presses the 'Play' button, then it goes to the play vs bot part, if the player presses the 'Train' button the it goes to the training, if he presses the 'Quit' button the game exits.
- Play Vs Bot - after the player presses the 'Play' button the game board will be displayed and the player will enter his secret code, then the bot generates his secret code and guesses after guessing his score will be displayed next to his entered code and he will run over the array of all possible combinations to mark all the impossible combinations. After the bot finishes marking all the impossible combinations the player will enter his guess and get his score. When one of them wins a pop up image will be displayed that shows the score and the following buttons: 'Menu' and 'Quit'. If the player presses the 'Menu' button he will be sent to the main menu part, if he pressed quit the program will exit.
- Train - after the player presses the 'Train' button the training board will be displayed and the player will need to guess the bots secret code within 8 attempts. After the player guesses the program will check the player's score and display it. When the player wins or he finished all of his 8 guesses a pop up image will be displayed that shows the score and the following buttons: 'Menu' and 'Quit'. If the player presses the 'Menu' button he will be sent to the main menu part, if he pressed quit the program will exit.
