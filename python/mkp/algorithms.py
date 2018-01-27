"""Algorithms for solving the Multiple Knapsack Problem."""
 
 
try:
    from mkp._algorithms_cy.mtm_cy import cyMTMSolver
except:
    raise ImportError("Could not import module - most likely Cython / C++ "
                      "was not compiled properly")
  
  
def mtm(p, w, c):
    """Solves Multiple Knapsack Problem with MTM algorithm.
      
    Args:
        p (list): Item profits.
        w (list): Item weights.
        c (list): Knapsack capacities.
          
    Returns:
        x (list): Assigned knapsacks for each item. Knapsack '-1' means that 
            the item is not assigned to any knapsack.
        z (int): Total profit.
        bt (int): Number of backtracks performed by the algorithm.
      
    Raises:
        <TODO>
          
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
      
    # TODO: Check if some item is over the minimum capacity
      
    # Decreasing profit/weight ratio for items
      
    # Increasing capacity for knapsacks
      
      
    # Solve
    res = cyMTMSolver(p,w,c).solve()
    x = res[:-2]
    z = res[-2]
    bt = res[-1]
      
    # Sort items and knapsacks to their original order
      
    # Ensure solution validity
      
    return z,x,bt