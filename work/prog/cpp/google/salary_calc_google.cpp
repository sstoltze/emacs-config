

// compensation.cpp, Maggie Johnson
// Description: A program to decide the best of three methods of compensation

#include <iostream>
using namespace std;

// constants which are used throughout the program
#define kPricePerUnit 225  // average price of a pair of shoes
#define kWeeklyWage 600    // current weekly wage - Method 1
#define kSalary 7.0        // hourly salary - Method 2
#define kHoursPerWeek 40    // number of hours worked - Method 2
#define kCommission2  0.10  // commission - Method 2
#define kCommission3 0.2    // commission - Method 3
#define kBonusPerUnit 20    // bonus  - Method 3

// A function to get the weekly sales of units
int GetInput() {
  int units;
  cout << "Enter number of units sold per week: ";
  if (!(cin >> units)) {
    cout << "Numbers only" << endl;
    return 0;
  } else {
    return units;
  }
}

// This one is easy - always the same: $600 per week
void CalcMethod1() {
  cout << "Method 1: " << kWeeklyWage << endl;
}

// Method 2: A salary of $7.00 per hour plus a 10% commission on sale
void CalcMethod2(int Sales) {
  double PerHour, TotalPay, Commission;

  PerHour = kSalary * kHoursPerWeek;
  Commission = (Sales * kPricePerUnit) * kCommission2;
  TotalPay = PerHour + Commission;
  cout << "Method 2: " << TotalPay << endl;
}

// Method 3: No salary, but 20% commissions and $20 for each pair of shoes sold
void CalcMethod3(int Sales) {
  int Extra;
  double TotalPay, Commission;

  Extra = kBonusPerUnit * Sales;
  Commission = (Sales * kPricePerUnit) * kCommission3;
  TotalPay = Extra + Commission;
  cout << "Method 3: " <<  TotalPay << endl;
}

main() {
  int WeeklySales;    // our input variable

  WeeklySales = GetInput();
  if (WeeklySales == 0)
    return 0;
  CalcMethod1();
  CalcMethod2(WeeklySales);
  CalcMethod3(WeeklySales);
}

