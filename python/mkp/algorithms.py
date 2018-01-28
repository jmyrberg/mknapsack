"""Algorithms for solving the Multiple Knapsack Problem."""


import numpy as np
import pandas as pd
 
try:
    from mkp._algorithms_cy.mtm_cy import cyMTMSolver
except:
    raise ImportError("Could not import module - most likely Cython / C++ "
                      "was not compiled properly")
  
  
def mtm(p, w, c):
    """Solves Multiple 0-1 Knapsack Problem with MTM algorithm.
      
    Args:
        p (list): Item profits.
        w (list): Item weights.
        c (list): Knapsack capacities.
          
    Returns:
        x (list): Assigned knapsacks for each item. Knapsack '-1' indicates
            that the item is not assigned to any knapsack.
        z (int): Total profit.
        bt (int): Number of backtracks performed by the algorithm.
      
    Raises:
        ValueError, if inputs are of incorrect type or size.
          
    References:
        S. Martello, P. Toth
        A Bound and Bound algorithm for the zero-one multiple knapsack problem
        Discrete Applied Mathematics, 3 (1981), pp. 257-288
          
    """
    # Validate inputs
    if not all(isinstance(vec,list) for vec in [p,w,c]):
        raise ValueError("All inputs are not of type 'list'")
    if len(p) != len(w):
        raise ValueError("Profit and weight lengths are not equal (%d != %d)" %
                         len(p),len(w))
     
    items = np.arange(len(p), dtype=int)
    ksacks = np.arange(len(c), dtype=int)
    
    # Decreasing profit/weight ratio for items
    p_ar = np.array(p, dtype=int)
    w_ar = np.array(w, dtype=int)
    pw_ord = np.argsort(p_ar / w_ar)[::-1]
    pw_map = np.zeros(len(p), dtype=int)
    pw_map[pw_ord] = items
    
    # Increasing capacity for knapsacks
    c_ar = np.array(c, dtype=int)
    c_ord = np.argsort(c_ar)
    c_map = dict((i,v) for i,v in enumerate(c_ord))
    c_map[-1] = -1
    
    # Sort arrays
    p_srt = p_ar[pw_ord].tolist()
    w_srt = w_ar[pw_ord].tolist()
    c_srt = c_ar[c_ord].tolist()
    
    # Solve
    res = cyMTMSolver(p_srt, w_srt, c_srt).solve()
    x = np.array(res[:-2])
    z = res[-2]
    bt = res[-1]
    
    # Sort items and knapsacks to their original order
    x = [c_map[i] for i in x[pw_map]]
    
    # Ensure solution validity
    x_ar = np.array(x)
    df = pd.DataFrame({'i': x_ar, 'j': items, 'p': p_ar, 'w': w_ar})
    df = df.merge(pd.DataFrame({'i': ksacks, 'c': c_ar}), on='i', how='left')
    df['c'] = df['c'].fillna(-1).astype(int)
    df = df.groupby('i').agg({'p': np.sum, 'w': np.sum, 'c': np.max})
    df['valid'] = (df['c'] >= df['w']).astype(int)
    print(df)
    print('z',z)
    print(df.loc[df.index>=0,'p'].sum())
    if z != df.loc[df.index>=0,'p'].sum():
        raise ValueError("Solution value not matching " 
                         "the profits of chosen items")
    if df['valid'].sum() != ksacks.shape[0]:
        raise ValueError("Solution not valid:\n%s" % df)
    
    return z,x,bt