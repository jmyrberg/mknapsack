#ifndef MTM_C_H
#define MTM_C_H

#include <vector>
#include <map>
#include <list>


namespace mtm {


class MTMSolver {
	/*	Solves the Multiple 0-1 Knapsack Problem (MKP) with MTM algorithm.
	
	Implementation reference:
	S. Martello, P. Toth
	A Bound and Bound algorithm for the zero-one multiple knapsack problem
	Discrete Applied Mathematics, 3 (1981), pp. 257-288
	*/
	private:

		std::vector<int> p; // Item profits
		std::vector<int> w; // Item weights
		std::vector<int> c; // Item capacities

		int n; // Number of items
		int m; // Number of knapsacks
		int bt; // Number of backtracks performed
		int btl; // Maximum number of backtracks to perform
        int tl; // Maximum number of seconds to run the algorithm

		std::vector<int> xh; // Current solution
		int z; // Current best solution value
		int i; // Current knapsack
		int L; // Lower bound for current solution
		int U; // Upper bound for current solution
		int ph; // Total profit of current solution
		std::vector<int> cr; // Knapsack residual capacities for current solution

		int Ur; // Upper bound at root node
		std::vector<int> xr; // Root solution (parametric upper bound)

		int Ul; // Upper bound of last solution (parametric upper bound)
		int il; // Knapsack considered in last solution (parametric upper bound)
		std::vector<int> xl; // Last current solution (parametric upper bound)
		int cl; // Residual capacity of last solution (parametric upper bound)

		std::vector<int> x; // Current best solution (knapsack for each item)
		std::vector<int> xt; // Latest solution calculated in lower bound

		std::map<int,std::list<int> > S; // Unlabeled (=assigned) items for each knapsack
		std::vector<int> jhuse; // Whether an item is assigned to a knapsack
		std::vector<int> Uj; // Upper bound of father node before setting xh[i][j] = 1
        
        bool glopt; // Indicates whether current solution is guaranteed to be global optimum or not

		void ParametricUpperBound(); // Compute parametric upper bound, if possible
		void UpperBound(); // Compute upper bound
		void LowerBound(); // Compute lower bound
	
	public:
		MTMSolver(std::vector<int> profits, std::vector<int> weights, std::vector<int> capacities, int max_backtracks = -1, int max_time = 3600);
		std::vector<int> solve(); // Run the algorithm
};
}

#endif
