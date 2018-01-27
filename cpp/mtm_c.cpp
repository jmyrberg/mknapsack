#include <iostream>
#include <algorithm>
#include <tuple>
#include <iterator>
#include <vector>
#include "mtm_c.h"


namespace mtm {


std::tuple<int,std::vector<int>> SolveSingleKnapsack(int capacity, std::vector<int> weights, std::vector<int> profits, int n) {
	
	// Impossible cases
	int j;
	std::vector<int> picked(n);
	if ( (capacity == 0) || (weights.size() == 0) || (profits.size() == 0)  || (n == 0) ) {
		for (j = 0; j < n; j++)
			picked.push_back(0);
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
	for (l = 0; l <= n; l++) {
		for (w = 0; w <= capacity; w++) {
			if (l==0 || w==0) {
				K[l][w] = 0;
			} else if (weights[l-1] <= w) {
				K[l][w] = std::max(profits[l-1] + K[l-1][w-weights[l-1]],  K[l-1][w]);
			} else {
				K[l][w] = K[l-1][w];
			};
	   };
	};	
	
	// Get picked up items as a vector
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
	return std::make_tuple(K[n][capacity], picked);
}


MTMSolver::MTMSolver(std::vector<int> profits, std::vector<int> weights, std::vector<int> capacities) {
	n = profits.size();
	m = capacities.size();
	p = profits;
	w = weights;
	c = capacities;
	
	xh.resize(m);
	xt.resize(m);
	x.resize(n);
	for (int k = 0; k < m; k++) {
		S[k] = {};
		xh[k].resize(n);
		xt[k].resize(n);
	}
	for (int j = 0; j < n; j++) {
		x[j] = -1;
	}
	
	z = 0;
	i = 0;
	bt = 0;
	upperBound();
	UB = U;
}
		

void MTMSolver::upperBound() {
	int k,j;
	
	// Remaining items at i
	std::vector<int> N_;
	for (j = 0; j < n; j++) {
		bool usej = true;
		for (k = 0; k < i+1; k++)
			if (xh[k][j] == 1)
				usej = false;
		if (usej)
			N_.push_back(j);
	}
	
	// Profits and weights of remaining items
	std::list<int> pl,wl;
	int n_ = N_.size();
	for (j = 0; j < n_; j++) {
		pl.push_back(p[N_[j]]);
		wl.push_back(w[N_[j]]);
	}
	std::vector<int> p_{std::make_move_iterator(std::begin(pl)), std::make_move_iterator(std::end(pl))};
	std::vector<int> w_{std::make_move_iterator(std::begin(wl)), std::make_move_iterator(std::end(wl))};
	
	// Capacity at i
	int c_ = c[i];
	std::list<int> Si = S[i];
	for (std::list<int>::iterator jit = Si.begin(); jit != Si.end(); jit++)
		c_ -= w[*jit] * xh[i][*jit];
	int wmin = *std::min_element(wl.begin(), wl.end());
	if (wmin > c_)
		c_ = 0;
	for (k = i+1; k < m; k++)
		c_ += c[k];
	
	// Solve knapsack
	auto sol = SolveSingleKnapsack(c_, w_, p_, p_.size());
	int z_ = std::get<0>(sol);
	
	// Calculate upper bound
	U = z_;
	for (k = 0; k < i+1; k++) {
		std::list<int> Sk = S[k];
		for (std::list<int>::iterator jit = Sk.begin(); jit != Sk.end(); jit++) {
			U += p[*jit] * xh[k][*jit];
		}
	}
}


void MTMSolver::lowerBound() {
	int k,j;
	bool usej;
	
	// Total profit for current solution
	L = 0;
	for (k = 0; k < i+1; k++) {
		std::list<int> Sk = S[k];
		for (std::list<int>::iterator jit = Sk.begin(); jit != Sk.end(); jit++)
			L += p[*jit] * xh[k][*jit];
	}
	
	// Remaining items
	std::list<int> Nd;
	for (j = 0; j < n; j++) {
		usej = true;
		for (k = 0; k < i+1; k++)
			if (xh[k][j] == 1)
				usej = false;
		if (usej)
			Nd.push_back(j);
	}
	std::list<int> N_;
	std::list<int> Si = S[i];
	for (std::list<int>::iterator jit = Nd.begin(); jit != Nd.end(); jit++) {
		std::list<int>::iterator iter = std::find(Si.begin(), Si.end(), *jit);
		if (!(iter != Si.end()))
			N_.push_back(*jit);
	}
	
	// Remaining capacity
	int c_ = c[i];
	for (std::list<int>::iterator jit = Si.begin(); jit != Si.end(); jit++)
		c_ -= w[*jit] * xh[i][*jit];
	
	// Initialize solution for remaining capacities i,...,m
	for (k = 0; k < m; k++)
		for (j = 0; j < n; j++)
			xt[k][j] = 0;
	
	k = i;
	while (k < m) {
		
		// Update profits and weights
		std::list<int> pl;
		std::list<int> wl;
		for (std::list<int>::iterator jit = N_.begin(); jit != N_.end(); jit++) {
			pl.push_back(p[*jit]);
			wl.push_back(w[*jit]);
		}
		std::vector<int> p_{std::make_move_iterator(std::begin(pl)), std::make_move_iterator(std::end(pl))};
		std::vector<int> w_{std::make_move_iterator(std::begin(wl)), std::make_move_iterator(std::end(wl))};
		
		auto sol = SolveSingleKnapsack(c_, w_, p_, p_.size());
		int z_ = std::get<0>(sol);
		std::vector<int> xtt = std::get<1>(sol);
		
		// Update solution for knapsack k
		int cnt = 0;
		for (std::list<int>::iterator jit = N_.begin(); jit != N_.end(); jit++) {
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
	bool heuristic,update,backtrack,stopUpdate;
	
	heuristic = true;
	while (heuristic) {
		
		// HEURISTIC
		update = true;
		backtrack = true;
		
		lowerBound();
		
		// Current solution is better than any previous
		if (L > z) {
			
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
				std::cout << "BREAK" << std::endl;
				break; // stop search
			}
			
			// Best solution has been found for the current node
			if (z == U) {
				backtrack = true;
				update = false; // go to backtrack
			}
		};
		
		// UPDATE
		if (update) {
			
			stopUpdate = false;
			while (i < m - 1) {
				
				// Add previous LB solution to node candidates
				I = {};
				for (l = 0; l < n; l++)
					if (xt[i][l] == 1)
						I.push_back(l);
					
				while (I.size() > 0) {
					j = *std::min_element(std::begin(I), std::end(I));
					I.remove(j);
					
					// Add item j to current solution
					S[i].push_back(j);
					xh[i][j] = 1;
					
					upperBound();
					
					// Current solution cannot be better than the best solution so far
					if (U <= z) {
						stopUpdate = true; // go to backtrack
						break;
					}
				};
				if (stopUpdate)
					break;
				else
					i++;
			};
			if ((i == m - 1) && (!stopUpdate))
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
						
						upperBound();
						
						// Current solution is better than the best solution so far
						if (U > z) {
							heuristic = true; // go to heuristic
							break;
						}
					};
					
				};
				if (heuristic)
					break;
				else 
					i--;
			};
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
	//std::cout << "SOLUTION = " << z << std::endl;
	//std::cout << "BACKTRACKS = " << bt << std::endl;
	return ret;
}
}
