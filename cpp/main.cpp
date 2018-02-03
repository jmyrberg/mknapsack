#include <iostream>
#include "mtm_c.h"


using namespace mtm;


int main() {
	/*
	std::vector<int> profits = {78, 35, 89, 36, 94, 75, 74, 79, 80, 16, 15, 15};
	std::vector<int> weights = {18, 9, 23, 20, 59, 61, 70, 75, 76, 30, 40, 40};
	std::vector<int> capacities = {82, 85, 87, 100};
	MTMSolver MTM(profits, weights, capacities);
	MTM.solve();
	std::cout << std::endl << std::endl;
	
	std::vector<int> profits2 = {78, 35, 89, 36, 94, 75, 74, 79, 80, 16};
	std::vector<int> weights2 = {18, 9, 23, 20, 59, 61, 70, 75, 76, 30};
	std::vector<int> capacities2 = {103, 130, 140};
	MTMSolver MTM2(profits2, weights2, capacities2);
	MTM2.solve();
	std::cout << std::endl << std::endl;

	std::vector<int> profits3 = {78, 78, 35, 35, 89, 89, 36, 36, 94, 94, 75, 75, 74, 74, 79, 79, 80, 80, 16, 16, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
	std::vector<int> weights3 = {18, 18, 9, 9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49};
	std::vector<int> capacities3 = {80, 90, 100, 110};
	MTMSolver MTM3(profits3, weights3, capacities3);
	MTM3.solve();
	std::cout << std::endl << std::endl;
*/
	std::vector<int> profits4 = {78, 77, 35, 34, 89, 88, 36, 35, 94, 93, 75, 74, 74, 73, 79, 78, 80, 79, 16, 15, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
	std::vector<int> weights4 = {18, 18, 9, 9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49};
	std::vector<int> capacities4 = {80, 90, 100, 110};
	MTMSolver MTM4(profits4, weights4, capacities4);
	MTM4.solve();
	std::cout << std::endl << std::endl;
/*
	std::vector<int> profits5 = {78, 35, 89, 36, 94, 75, 74, 79, 80, 16};
	std::vector<int> weights5 = {18, 9, 23, 20, 59, 61, 70, 75, 76, 30};
	std::vector<int> capacities5 = {76, 110, 112};
	MTMSolver MTM5(profits5, weights5, capacities5);
	MTM5.solve();
	std::cout << std::endl << std::endl;

	std::vector<int> profits6 = {63, 66, 65, 64, 64, 64, 64, 63, 64, 65, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6};
	std::vector<int> weights6 = {420, 510, 510, 420, 420, 390, 420, 510, 420, 510, 270, 300, 330, 360, 390, 420, 450, 480, 510, 540, 570, 600};
	std::vector<int> capacities6 = {1320, 1320, 1320, 1320};
	MTMSolver MTM6(profits6, weights6, capacities6);
	MTM6.solve();
	std::cout << std::endl << std::endl;
*/
	return 0;
}
