#include <iostream>
#include "mtm_c.h"


using namespace mtm;


int main() {
	
	std::vector<int> profits = {78, 35, 89, 36, 94, 75, 74, 79, 80, 16, 15, 15};
	std::vector<int> weights = {18, 9, 23, 20, 59, 61, 70, 75, 76, 30, 40, 40};
	std::vector<int> capacities = {82, 85, 87, 100};
	MTMSolver MTM(profits, weights, capacities);
	MTM.solve();
	
	std::vector<int> profits2 = {78, 35, 89, 36, 94, 75, 74, 79, 80, 16};
	std::vector<int> weights2 = {18, 9, 23, 20, 59, 61, 70, 75, 76, 30};
	std::vector<int> capacities2 = {103, 130, 140};
	MTMSolver MTM2(profits2, weights2, capacities2);
	MTM2.solve();
	
	std::vector<int> profits3 = {78, 78, 35, 35, 89, 89, 36, 36, 94, 94, 75, 75, 74, 74, 79, 79, 80, 80, 16, 16, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
	std::vector<int> weights3 = {18, 18, 9, 9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49};
	std::vector<int> capacities3 = {80, 90, 100, 110};
	MTMSolver MTM3(profits3, weights3, capacities3);
	MTM3.solve();
	return 0;
}
