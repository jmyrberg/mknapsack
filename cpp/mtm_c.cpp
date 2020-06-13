#include <iostream>
#include <algorithm>
#include <tuple>
#include <iterator>
#include <vector>
#include <math.h>
#include <time.h>
#include "mtm_c.h"


namespace mtm {


int max(int a, int b) {
	return a > b ? a : b;
}


std::tuple<int,std::vector<int>> SolveSingleKnapsack(std::vector<int> profits, std::vector<int> weights, int capacity, int n_items) {

	std::vector<int> p = profits;
	std::vector<int> w = weights;
	int c = capacity;
	int n = n_items;

	// Impossible cases
	std::vector<int> picked(n,0);
	if ((c == 0) || (n == 0))
		return std::make_tuple(0, picked);

	// Remove items where weight > capacity
	int j;
	std::vector<int> idx2j(n);
	for (j = 0; j < n; j++)
		idx2j[j] = j;
	if (*std::max_element(w.begin(), w.end()) > c) {
		p = {};
		w = {};
		int cnt = 0;
		for (j = 0; j < n; j++)
			if (weights[j] <= c) {
				p.push_back(profits[j]);
				w.push_back(weights[j]);
				idx2j[cnt] = j;
				cnt++;
			}
		n = cnt;
		if (n == 0)
			return std::make_tuple(0, picked);
	}

	// Run algorithm
	int i, k;
	std::vector<int> K((n+1)*(c+1));

	// Build DP table
	for (i = 0; i <= n; i++) {
		K[i*(c+1) + 0] = 0;
	}
	for (k = 0; k <= c; k++)
		K[0*(c+1) + k] = 0;
	for (i = 1; i <= n; i++)
		for (k = 1; k <= c; k++)
			K[i*(c+1) + k] = (w[i-1] <= k) ? max(p[i-1] + K[(i-1)*(c+1) + k-w[i-1]],  K[(i-1)*(c+1) + k]) : K[(i-1)*(c+1) + k];

	// Get picked up items as a vector
	int wn;
	i = n;
	k = c;
	while (i > 0) {
		wn = k - w[i-1];
		if (wn >= 0) {
			if (K[i*(c+1) + k] - K[(i-1)*(c+1) + wn] == p[i-1]) {
				i--;
				k -= w[i];
				picked[idx2j[i]] = 1;
			} else {
				i--;
				picked[idx2j[i]] = 0;
			}
		} else {
			i--;
		}
	}
	return std::make_tuple(K[n*(c+1) + c], picked);
}


MTMSolver::MTMSolver(std::vector<int> profits, std::vector<int> weights, std::vector<int> capacities, int max_backtracks, int max_time) {
	p = profits;
	w = weights;
	c = capacities;
    
    glopt = true;

	n = profits.size();
	m = capacities.size();
	z = 0;
	i = 0;
	L = 0;
	U = 0;
	Ur = 0;
	bt = 0;
	btl = max_backtracks;
    tl = max_time;
	ph = 0;

	Ul = 0;
	il = 0;

	x.resize(n);
	cr.resize(m);
	jhuse.resize(n);
	Uj.resize(n);

	xh.resize(n*m);
	xt.resize(n*m);
	xl.resize(n);
	xr.resize(n);

	int ct = 0;
	for (int k = 0; k < m; k++) {
		cr[k] = c[k];
		cl += c[k];
		ct += c[k];

		S[k] = {};
	}
	for (int j = 0; j < n; j++) {
		x[j] = -1;
		jhuse[j] = 0;
		Uj[j] = -1;
	}

	auto sol = SolveSingleKnapsack(p, w, ct, n);
	U = std::get<0>(sol);
	xr = std::get<1>(sol);
	Ur = U;
}


void MTMSolver::ParametricUpperBound() {
	int k,j,kq;
	bool calc_ub = true;

	// Last solution
	while (true) {

		// Condition (1)
		bool condl1 = true;
		for (k = il; k <= i; k++)
			for (j = 0; j < n; j++)
				if ((xh[k*n + j] == 1) && (xl[j] == 0)) {
					condl1 = false;
					break;
				}

		// Condition (2)
		kq = 0;
		for (k = il; k < i; k++)
			kq += cr[k];
		bool condl2 = (cl >= kq) ? true : false;

		// Use previous upper bound from last solution?
		if (condl1 && condl2) {
			U = Ul;
			calc_ub = false;
		}
		break;
	}

	// Root solution
	while (true && calc_ub) {

		// Condition (1)
		bool condr1 = true;
		for (k = 0; k <= i; k++)
			for (j = 0; j < n; j++)
				if ((xh[k*n + j] == 1) && (xr[j] == 0)) {
					condr1 = false;
					break;
				}

		// Condition (2)
		kq = 0;
		for (k = 0; k < i; k++)
			kq += cr[k];
		bool condr2 = (cl >= kq) ? true : false;

		// Use previous upper bound from last solution?
		if (condr1 && condr2) {
			U = Ur;
			calc_ub = false;
		}
		break;
	}

	// Calculate new upper bound
	if (calc_ub)
		UpperBound();
}
		

void MTMSolver::UpperBound() {
	int k,j;

	// // Profits and weights of remaining items
	int n_ = 0;
	for (j = 0; j < n; j++)
		n_ += 1 - jhuse[j];
	std::vector<int> N_(n_),p_(n_),w_(n_);
	int cnt = 0;
	int wt = 0;
	int pt = 0;
	for (j = 0; j < n; j++) {
		if (jhuse[j] == 0) {
			N_[cnt] = j;
			p_[cnt] = p[j];
			w_[cnt] = w[j];
			wt += w[j];
			pt += p[j];
			cnt++;
		}
	}
	
	// Remaining capacity
	int c_ = (*std::min_element(w_.begin(), w_.end()) > cr[i]) ? 0 : cr[i];
	for (k = i+1; k < m; k++)
		c_ += cr[k];
	
	// Solve knapsack, if maximum available profit exceeds current best profit
	U = ph;
	std::vector<int> xtt(n_);
	std::fill(xl.begin(), xl.end(), 0);
	if (wt > c_) {
		auto sol = SolveSingleKnapsack(p_, w_, c_, n_);
		int z_ = std::get<0>(sol);
		xtt = std::get<1>(sol);
		U += z_;

		cl = c_;
		cnt = 0;
		for (auto jit = N_.begin(); jit != N_.end(); jit++) {
			xl[*jit] = xtt[cnt];
			if (xtt[cnt] == 1)
				cl -= w_[cnt];
			cnt++;
		}
	} else {
		for (auto jit = N_.begin(); jit != N_.end(); jit++)
			xl[*jit] = 1;
		U += pt;
		cl = c_ - wt;
	}
	Ul = U;
	il = i;
}


void MTMSolver::LowerBound() {
	int k,j;
	
	// Total profit for current solution
	L = ph;
	
	// Remaining items
	std::list<int> Nd,N_;
	std::list<int> Si = S[i];
	std::list<int>::iterator jit,fit;
	for (j = 0; j < n; j++)
		if (jhuse[j] == 0)
			Nd.push_back(j);
	for (jit = Nd.begin(); jit != Nd.end(); jit++) {
		fit = std::find(Si.begin(), Si.end(), *jit);
		if (!(fit != Si.end()))
			N_.push_back(*jit);
	}

	// Remaining capacity
	int c_ = cr[i];
	
	// Initialize solution
	std::fill(xt.begin(), xt.end(), 0);
	
	k = i;

	int n_,z_,cnt;
	std::vector<int> p_,w_,xtt;
	while (k < m) {

		// Update profits and weights
		n_ = N_.size();
		p_.resize(n_);
		w_.resize(n_);
		cnt = 0;
		for (jit = N_.begin(); jit != N_.end(); jit++) {
			p_[cnt] = p[*jit];
			w_[cnt] = w[*jit];
			cnt++;
		}

		auto sol = SolveSingleKnapsack(p_, w_, c_, n_);
		z_ = std::get<0>(sol);
		xtt = std::get<1>(sol);

		// Update solution for knapsack k
		cnt = 0;
		for (jit = N_.begin(); jit != N_.end(); jit++) {
			xt[k*n + (*jit)] = xtt[cnt];
			cnt++;
		}
		L += z_;

		// Remove solution items
		for (j = 0; j < n; j++)
			if (xt[k*n + j] == 1)
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
    
    clock_t start = clock() / CLOCKS_PER_SEC;
    clock_t point = clock() / CLOCKS_PER_SEC;
    
    heuristic = true;
    while (heuristic) {
        
        // HEURISTIC
        update = true;
        backtrack = true;
        
        LowerBound();
        
        // Current solution is better than any previous
        if (L > z) {

            // Update new solution value z and solution x
            z = L;
            for (j = 0; j < n; j++)
                x[j] = -1;
            for (k = 0; k < m; k++)
                for (j = 0; j < n; j++)
                    x[j] = (xh[k*n + j] == 1) ? k : x[j];
            for (k = i; k < m; k++)
                for (j = 0; j < n; j++)
                    if (xt[k*n + j] == 1)
                        x[j] = k;
            
            // Optimal solution has been found globally
            if (z == Ur) {
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
                    if (xt[i*n + l] == 1)
                        I.push_back(l);
                    
                while (I.size() > 0) {
                    j = *std::min_element(I.begin(), I.end());
                    I.remove(j);

                    // Add item j to current solution
                    S[i].push_back(j);
                    xh[i*n + j] = 1;
                    cr[i] -= w[j];
                    ph += p[j];
                    jhuse[j] = 1;
                    Uj[j] = U;

                    ParametricUpperBound();

                    // Current solution cannot be better than the best solution so far
                    if (U <= z) {
                        break; // go to backtrack
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
            point = clock() / CLOCKS_PER_SEC;
            if (point - start > tl) { // Time is up!
                glopt = false;
                heuristic = false;
                break;
            }
            heuristic = false;
            backtrack = false;
            bt++;
            if (bt == btl) {
                glopt = false;
                break;
            }
            while (i >= 0) {
                while (S[i].size() > 0) {
                    j = S[i].back();
                    
                    // Backtracking was called with item not in the current solution
                    if (xh[i*n + j] == 0) {
                        S[i].pop_back();
                    } else {	
                        
                        // Remove j from current solution
                        xh[i*n + j] = 0;
                        cr[i] += w[j];
                        ph -= p[j];
                        jhuse[j] = 0;

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
                else {
                    i--;
                    il -= 1;
                }
            }
        }
    } // heuristic
	
    std::vector<int> res(n+3);
	for (j = 0; j < n+3; j++) {
		if (j < n)
			res[j] = x[j];
        else if (j == n)
            res[j] = glopt;
        else if (j == n+1)
            res[j] = z;
		else
			res[j] = bt;
	}

	/*std::vector<int> ksack_weights(m);
	for (k = 0; k < m; k++)
		ksack_weights[k] = 0;
	std::cout << "x =";
	for (j = 0; j < n; j++) {
		std::cout << " " << x[j]+1;
		k = x[j];
		if (k != -1) {
			ksack_weights[k] += w[j];
		}
	}
	std::cout << std::endl;
	for (k = 0; k < m; k++) {
		std::cout << "k[" << k+1 << "] = " << ksack_weights[k] << " / " << c[k] << std::endl;
	}
	std::cout << "SOLUTION = " << z << std::endl;
	std::cout << "BACKTRACKS = " << bt << std::endl;
	std::cout << "BACKTRACKS = " << btl << std::endl;*/
	return res;
}
} // namespace
