#include <iostream>
#include <limits>
using namespace std;

int main() {
  int input_var = 0;
  int sum = 0;
  do {
    cout << "Enter a number (-1 to quit):" << endl;
    if (! (cin >> input_var)) {
      cout << "Error!" << endl;
      cin.clear();
      cin.ignore(numeric_limits<streamsize>::max(), '\n');
    }
    else {
      if (input_var != -1) {
        sum += input_var;
        cout << "You entered " << input_var << endl;
      }
    }
  } while (input_var != -1);
  
  cout << endl << "Done." << endl;
  cout << "The total sum entered is: " << sum << endl;
  return 0;
}

