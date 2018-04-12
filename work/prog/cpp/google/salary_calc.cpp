#include <iostream>
using namespace std;

int get_weekly_sales();
double calculate_method_one(int s);
double calculate_method_two(int s);
double calculate_method_three(int s);



int main() {
  int weekly_sales;
  double straight_salary;
  double salary_plus_commision;
  double only_commision;

  weekly_sales = get_weekly_sales();
  if (weekly_sales < 0) {
    printf("Error. Could not read sales.\n");
    return 1;
  }

  straight_salary = calculate_method_one(weekly_sales);
  salary_plus_commision = calculate_method_two(weekly_sales);
  only_commision = calculate_method_three(weekly_sales);
  printf("\nWith %d weekly sales, the result is:\n", weekly_sales);
  printf("Method 1:\tMethod 2:\tMethod 3:\n");
  printf("%f\t%f\t%f\n", straight_salary, salary_plus_commision, only_commision);

  printf("\n");
  printf("A table for comparison:\n");
  printf("Sales:\tMethod 1:\tMethod 2:\tMethod 3:\n");
  for (int i = 0; i < 20; i++) {
    printf("%d\t%7.2f\t%7.2f\t%7.2f\n", i, calculate_method_one(i),
           calculate_method_two(i), calculate_method_three(i));
  }
  return 0;
}

int get_weekly_sales() {
  int result;
  printf("Enter weekly sales:\n");
  if (! (cin >> result)) {
    return -1;
  }
  return result;
}

double calculate_method_one(int sales) {
  return 600;
}

double calculate_method_two(int sales) {
  return 7 * 8 * 5 + 0.1 * 225 * sales;
}

double calculate_method_three(int sales) {
  return (20 + 0.2 * 225) * sales;
}




