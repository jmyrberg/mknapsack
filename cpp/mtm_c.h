#ifndef MTM_C_H
#define MTM_C_H

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
		std::vector<int> p,w,c,x,cr;
		int n,m,z,i,L,U,UB,bt,Ul,cl,il,pr;
		std::map<int,std::list<int> > S;
		std::vector<std::vector<int> > xh,xt,xl;
		void UpperBound();
		void LowerBound();
		void ParametricUpperBound();
	
	public:
		MTMSolver(std::vector<int> profits, std::vector<int> weights, std::vector<int> capacities);
		std::vector<int> solve();
};
}

#endif
