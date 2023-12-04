#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

const int WINNING_NUMBERS_LEN = 5;
const int NUMBERS_LEN = 8;

struct intArray {
  int capacity;
  int length;
  int* values;
};

struct cardNumbers{
  int gameNumber;
  struct intArray pickedNumbers;
  struct intArray winningNumbers;
};

struct inputLine {
  size_t length;
  char* text;
};

struct intArray newIntArray(int capacity) {
    struct intArray array;
    array.capacity = capacity;
    array.length = 0;
    array.values = malloc(sizeof(int) * capacity);
    return array;
}

struct intArray growArray(struct intArray array) {
  int* prevValues = array.values;
  array.capacity = array.capacity * 2;
  array.values = malloc(sizeof(int) * array.capacity);
  memcpy(array.values, prevValues, sizeof(int) * array.length);
  free(prevValues);
  return array;
}

struct intArray appendInt(struct intArray array, int number) {
  if (array.capacity < array.length + 1) {
    array = growArray(array);
  }
  array.values[array.length] = number;
  array.length++;
  return array;
}

const int STATE_SEEK_GAME_NUMBER = 0;
const int STATE_SEEK_WINNING_NUMBER = 1;
const int STATE_SEEK_PICKED_NUMBER = 2;

int isDigit(char value) {
  return value >= '0' && value <= '9';
}

int digitToInt(char value) {
  return value - '0';
}

int parseNumber(struct inputLine line, int* index) {
  int result = 0;
  for (; *index < line.length; *index += 1) {
    char currentChar = line.text[*index];
    if (!isDigit(currentChar)) {
      break;
    }
    result = result * 10 + digitToInt(currentChar);
  }
  return result;
}

struct cardNumbers parseCardNumbers(struct inputLine line) {
  struct cardNumbers result;
  result.pickedNumbers = newIntArray(4);
  result.winningNumbers = newIntArray(4);
  int state = STATE_SEEK_GAME_NUMBER;
  for (int index = 0; index < line.length; index++) {
    char currentChar = line.text[index];
    switch (state) {
      case STATE_SEEK_GAME_NUMBER:
        if (isDigit(currentChar)) {
          result.gameNumber = parseNumber(line, &index);
          state = STATE_SEEK_WINNING_NUMBER;
        }
        break;
      case STATE_SEEK_WINNING_NUMBER:
        if (currentChar == '|') {
          state = STATE_SEEK_PICKED_NUMBER;
          continue;
        }
        if (isDigit(currentChar)) {
          int number = parseNumber(line, &index);
          result.winningNumbers = appendInt(result.winningNumbers, number);
        }
        break;
      case STATE_SEEK_PICKED_NUMBER:
        if (isDigit(currentChar)) {
          int number = parseNumber(line, &index);
          result.pickedNumbers = appendInt(result.pickedNumbers, number);
        }
        break;
    }
  }
  return result;
}

struct inputLine readInputLine() {
  struct inputLine line;
  memset(&line, 0, sizeof(line));
  if (getline(&line.text, &line.length, stdin) == -1) {
    free(line.text);
    line.length = 0;
    line.text = NULL;
  }
  return line;
}

int countMatchedNumbers(struct cardNumbers numbers) {
    int matchedNumbers = 0;
    for (int pickedIndex = 0; pickedIndex < numbers.pickedNumbers.length; pickedIndex++) {
      int pickedNumber = numbers.pickedNumbers.values[pickedIndex];
      for (int winningIndex = 0; winningIndex < numbers.winningNumbers.length; winningIndex++) {
        int winningNumber = numbers.winningNumbers.values[winningIndex];
        if (pickedNumber == winningNumber) {
          matchedNumbers += 1;
          break;
        }
      }
    }
    return matchedNumbers;
}

int main() {
  int totalScore = 0;
  struct intArray gameResults = newIntArray(4);
  struct intArray gameCardCounts = newIntArray(4);
  while (1) {
    struct inputLine line = readInputLine();
    if (line.text == NULL) {
      break;
    }
    struct cardNumbers numbers = parseCardNumbers(line);
    int matchedNumbers = countMatchedNumbers(numbers);
    gameResults = appendInt(gameResults, matchedNumbers);
    gameCardCounts = appendInt(gameCardCounts, 1);
    if (matchedNumbers > 0) {
      totalScore += (int)pow(2.0, (double)(matchedNumbers - 1));
    }
    free(line.text);
    free(numbers.winningNumbers.values);
    free(numbers.pickedNumbers.values);
  }
  printf("\nPart 1 score: %d", totalScore);
  int totalCards = 0;
  for (int cardIndex = 0; cardIndex < gameResults.length; cardIndex++) {
    int points = gameResults.values[cardIndex];
    int cardCount = gameCardCounts.values[cardIndex];
    totalCards += cardCount;
    for (
      int nextCardIndex = cardIndex + 1;
      nextCardIndex <= cardIndex + points && cardIndex < gameResults.length;
      nextCardIndex++
    ) {
      gameCardCounts.values[nextCardIndex] += cardCount; 
    }
  }
  printf("\nPart 2 score: %d", totalCards);
  free(gameResults.values);
  free(gameCardCounts.values);
  return 0;
}
