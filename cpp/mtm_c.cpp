#include <iostream>
#include <algorithm>
#include <tuple>
#include <iterator>
#include <vector>
#include <math.h>
#include "mtm_c.h"


namespace mtm {


int max(int a, int b) {
	return a > b ? a : b;
}


std::tuple<int,std::vector<int>> SolveSingleKnapsack(int capacity, std::vector<int> weights, std::vector<int> profits, int n, bool return_picked) {
	
	// Impossible cases
	int j;
	std::vector<int> picked(n);
	if ((capacity <= 0) || (n == 0)) {
		for (j = 0; j < n; j++)
			picked[j] = 0;
		return std::make_tuple(0, picked);
	}
	
	// Remove items where weight > capacity
	std::map<int,int> idx2j;
	for (j = 0; j < n; j++)
		idx2j[j] = j;
	int wmax = *std::max_element(weights.begin(), weights.end());
	if (wmax > capacity) {
		std::vector<int> profits_,weights_;
		int cnt = 0;
		for (j = 0; j < n; j++) {
			picked[j] = 0;
			if (weights[j] <= capacity) {
				profits_.push_back(profits[j]);
				weights_.push_back(weights[j]);
				idx2j[cnt] = j;
				cnt++;
			}
		}
		profits = profits_;
		weights = weights_;
		n -= weights.size();
	}

	// Run algorithm
	int l, w;
	int K[n+1][capacity+1];
	
	// Build DP table
	for (w = 0; w <= capacity; w++)
		K[0][w] = 0;
	for (l = 0; l <= n; l++)
		K[l][0] = 0;
	for (l = 1; l <= n; l++) {
		for (w = 1; w <= capacity; w++) {
			if (weights[l-1] <= w) {
				K[l][w] = max(profits[l-1] + K[l-1][w-weights[l-1]],  K[l-1][w]);
			} else {
				K[l][w] = K[l-1][w];
			};
	   };
	};	
	
	// Get picked up items as a vector
	if (return_picked) {
		l = n;
		w = capacity;
		while (l > 0) {
			if ((K[l][w] - K[l-1][w-weights[l-1]]) == profits[l-1]) {
				l--;
				w = w - weights[l];
				picked[idx2j[l]] = 1;
			} else {
				l--;
				picked[idx2j[l]] = 0;
			}
		}
	}
	return std::make_tuple(K[n][capacity], picked);
}


MTMSolver::MTMSolver(std::vector<int> profits, std::vector<int> weights, std::vector<int> capacities) {
	p = profits;
	w = weights;
	c = capacities;

	n = profits.size();
	m = capacities.size();
	z = 0;
	i = 0;
	L = 0;
	U = 0;
	UB = 0;
	bt = 0;
	ph = 0;

	Ul = 0;
	il = 0;

	x.resize(n);
	cr.resize(m);
	jhuse.resize(n);
	Uj.resize(n);

	xh.resize(m);
	xt.resize(m);
	xl.resize(m);

	for (int k = 0; k < m; k++) {
		cr[k] = c[k];

		S[k] = {};

		xh[k].resize(n);
		xt[k].resize(n);
		xl[k].resize(n);
	}
	for (int j = 0; j < n; j++) {
		x[j] = -1;
		jhuse[j] = 0;
		Uj[j] = -1;
	}

	UpperBound(); // Sets U
	UB = U;

}


void MTMSolver::ParametricUpperBound() {
	int k,j;

	// Condition (1)
	bool cond1 = true;
	for (k = il; k <= i; k++)
		for (j = 0; j < n; j++)
			if ((xh[k][j] == 1) && (xl[k][j] == 0))
				cond1 = false;

	// Condition (2)
	int ks = 0;
	for (k = il; k <= i+1; k++)
		ks += cr[k];
	int cl = 0;
	for (k = il; k < m; k++) {
		cl += c[k];
		for (j = 0; j < n; j++)
			cl -= w[j] * xl[k][j];
	}
	bool cond2 = (cl >= ks) ? true : false;

	// Use previous upper bound when possible
	if (!(cond1 && cond2) || (Ul == 0)) {
		UpperBound();
		Ul = U;
	} else {
		U = Ul;
	}
	xl = xt;
	il = i;
}
		

void MTMSolver::UpperBound() {
	int k,j;

	// // Profits and weights of remaining items
	int n_ = 0;
	for (j = 0; j < n; j++)
		if (jhuse[j] == 0)
			n_ += 1;
	std::vector<int> N_(n_),p_(n_),w_(n_);
	int cnt = 0;
	for (j = 0; j < n; j++) {
		if (jhuse[j] == 0) {
			N_[cnt] = j;
			p_[cnt] = p[j];
			w_[cnt] = w[j];
			cnt++;
		}
	}
	
	// Remaining capacity
	int c_ = 0;
	for (k = i; k < m; k++)
		c_ += cr[k];

	// Maximum available profit
	int wt = 0;
	int pt = 0;
	for (j = 0; j < n_; j++) {
		if (wt + w_[j] >= c_) {
			pt += (int) ceil((c_ - wt) * p_[j] / w_[j]);
			break;
		}
		wt += w_[j];
		pt += p_[j];
	}
	
	// Solve knapsack, if maximum available profit exceeds current best profit
	U = ph;
	if (pt + U > z) {
		auto sol = SolveSingleKnapsack(c_, w_, p_, n_, false);
		int z_ = std::get<0>(sol);
		U += z_;
	} else {
		U += pt;
	}
}


void MTMSolver::LowerBound() {
	int k,j;
	
	// Total profit for current solution
	L = ph;
	
	// Remaining items
	std::list<int> Nd;
	for (j = 0; j < n; j++)
		if (jhuse[j] == 0)
			Nd.push_back(j);
	std::list<int> N_;
	std::list<int> Si = S[i];
	std::list<int>::iterator jit,fit;
	for (jit = Nd.begin(); jit != Nd.end(); jit++) {
		fit = std::find(Si.begin(), Si.end(), *jit);
		if (!(fit != Si.end()))
			N_.push_back(*jit);
	}

	// Remaining capacity
	int c_ = cr[i];
	
	// Initialize solution
	for (k = 0; k < m; k++)
		for (j = 0; j < n; j++)
			xt[k][j] = 0;
	
	k = i;

	int n_,z_,cnt;
	std::vector<int> p_,w_,xtt;
	while (k < m) {

		// Update profits and weights
		n_ = N_.size();
		p_ = {};
		w_ = {};
		for (jit = N_.begin(); jit != N_.end(); jit++) {
			p_.push_back(p[*jit]);
			w_.push_back(w[*jit]);
		}

		auto sol = SolveSingleKnapsack(c_, w_, p_, n_, true);
		z_ = std::get<0>(sol);
		xtt = std::get<1>(sol);
		
		// Update solution for knapsack k
		cnt = 0;
		for (jit = N_.begin(); jit != N_.end(); jit++) {
			xt[k][*jit] = xtt[cnt];
			cnt++;
		}
		L += z_;

		// Remove solution items
		for (j = 0; j < n; j++)
			if (xt[k][j] == 1)
				Nd.remove(j);
		N_ = Nd;

		k++;
		
		// Update capacity
		if (k < m)
			c_ = c[k];
	}
}
		

std::vector<int> MTMSolver::solve() {
	int k,l,j;
	std::list<int> Si,I;
	bool heuristic,update,backtrack,stop_update;
	
	heuristic = true;
	while (heuristic) {
		
		// HEURISTIC
		update = true;
		backtrack = true;
		
		LowerBound();
		
		// Current solution is better than any previous
		if (L > z) {


			// ** TODO: FIX HERE!! ** //
			std::cout << "L = " << L << " | z = " << z << std::endl;

			std::cout << "x = ";
			for (j = 0; j < n; j++)
				std::cout << " " << x[j];
			std::cout << std::endl;

			for (k = 0; k < m; k++) {
				std::cout << "xh[" << k << "] = ";
				for (j = 0; j < n; j++)
					if (xh[k][j] == 1)
						std::cout << " " << j;
				std::cout << std::endl;
			}
			std::cout << std::endl;

			for (k = 0; k < m; k++) {
				std::cout << "xt[" << k << "] = ";
				for (j = 0; j < n; j++)
					if (xt[k][j] == 1)
						std::cout << " " << j;
				std::cout << std::endl;
			}
			std::cout << std::endl;

			std::cout << "ph = " << ph << std::endl;

			// ** ADDITIONAL PRINTING ENDS ** //


			// Update new solution value z and solution x
			z = L;
			for (j = 0; j < n; j++)
				x[j] = -1;
			for (k = 0; k < m; k++)
				for (j = 0; j < n; j++)
					x[j] = (xh[k][j] == 1) ? k : x[j];
			for (k = i; k < m; k++)
				for (j = 0; j < n; j++)
					if (xt[k][j] == 1)
						x[j] = k;
			
			// Optimal solution has been found globally
			if (z == UB) {
				break; // stop search
			}
			
			// Best solution has been found for the current node
			if (z == U) {
				backtrack = true;
				update = false; // go to backtrack
			}
		}
		
		// UPDATE
		if (update) {
			stop_update = false;
			while (i < m - 1) {
				
				// Add previous LB solution to node candidates
				I = {};
				for (l = 0; l < n; l++)
					if (xt[i][l] == 1)
						I.push_back(l);
					
				while (I.size() > 0) {
					j = *std::min_element(I.begin(), I.end());
					I.remove(j);

					// Add item j to current solution
					S[i].push_back(j);
					xh[i][j] = 1;
					cr[i] -= w[j];
					ph += p[j];
					jhuse[j] = 1;
					Uj[j] = U;
					
					ParametricUpperBound();

					// Current solution cannot be better than the best solution so far
					if (U <= z) {
						stop_update = true; // go to backtrack
						break;
					}
				}
				if (stop_update)
					break;
				else
					i++;
			}
			if ((i == m - 1) && (!stop_update))
				i = m - 2;
		}
		
		// BACKTRACK
		if (backtrack) {
			heuristic = false;
			backtrack = false;
			bt++;
			while (i >= 0) {
				while (S[i].size() > 0) {
					j = S[i].back();
					
					// Backtracking was called with item not in the current solution
					if (xh[i][j] == 0) {
						S[i].pop_back();
					} else {	
						
						// Remove j from current solution
						xh[i][j] = 0;
						cr[i] += w[j];
						ph -= p[j];
						jhuse[j] = 0;

						if (Uj[j] == -1)
							UpperBound();
						else
							U = Uj[j];

						// Current solution is better than the best solution so far
						if (U > z) {
							heuristic = true; // go to heuristic
							break;
						}
					}
					
				}
				if (heuristic)
					break;
				else 
					i--;
			}
		}
	} // heuristic
	
	std::vector<int> ret(n+2);
	for (j = 0; j < n+2; j++) {
		if (j < n)
			ret[j] = x[j];
		else if (j == n)
			ret[j] = z;
		else
			ret[j] = bt;
	}

	/*std::cout << "x = ";
	for (j = 0; j < n; j++)
		std::cout << " " << x[j]+1;
	std::cout << std::endl;
	std::cout << "SOLUTION = " << z << std::endl;
	std::cout << "BACKTRACKS = " << bt << std::endl;*/

	return ret;
}
} // namespace
