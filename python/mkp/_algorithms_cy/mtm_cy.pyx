from libcpp.vector cimport vector


cdef extern from "mtm_c.h" namespace "mtm":
    
    cdef cppclass MTMSolver:
        MTMSolver(vector[int], vector[int], vector[int]) except +
        vector[int] solve()
        

cdef class cyMTMSolver:
    """Wrapper for the C++ code."""
    cdef MTMSolver *solver
    
    def __cinit__(self, vector[int] p, vector[int] w, vector[int] c):
        self.solver = new MTMSolver(p, w, c)
    
    def solve(self):
        cdef list res = self.solver.solve()
        return res