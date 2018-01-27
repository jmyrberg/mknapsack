#ifndef C_MTM_H
#define C_MTM_H

#include <vector>
#include <map>
#include <list>


namespace mtm {


class MTMSolver {
	/*	Solves the Multiple Knapsack Problem (MKP) with MTM algorithm.
	
	Implementation reference:
	S. Martello, P. Toth
	A Bound and Bound algorithm for the zero-one multiple knapsack problem
	Discrete Applied Mathematics, 3 (1981), pp. 257-288
	*/
	private:
		std::vector<int> p,w,c,x;
		int n,m,z,i,L,U,UB,bt;
		std::map<int,std::list<int> > S;
		std::vector<std::vector<int> > xh,xt;
		void upperBound();
		void lowerBound();
	
	public:
		MTMSolver(std::vector<int> profits, std::vector<int> weights, std::vector<int> capacities);
		std::vector<int> solve();
};
}

#endif
