#include <iostream>
#include <limits>
#include <stdlib.h>
#include <time.h>
using namespace std;

int main() {
  int guess = 0;
  int result;
  int low = 1;
  int high = 100;

  srand(time(NULL));
  result = rand() % 100 + 1;
  // printf("%d\n",result);

  do {
    printf("Guess a number between %3d and %3d:\n", low, high);
    if (! (cin >> guess) ) {
      printf("Error!\n");
      break;
    }
    if (guess < result) {
      printf("The secret number is higher.\n");
      if (guess >= low) {
        low = guess + 1;
      }
    }
    else if (guess > result) {
      printf("The secret number is lower.\n");
      if (guess <= high) {
        high = guess - 1;
      }
    }
  } while (guess != result);

  if (guess == result) {
    printf("You won!");
  }
  else {
    printf("Too bad.");
  }
  printf(" The secret number was %3d.\n", result);
  
  return 0;
}




