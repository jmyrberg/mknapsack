"""Test cases for algorithms."""


import time

from mkp.algorithms import mtm


def test_mtm():
    
    start_tot = time.clock()
    
    print('-'*10 + ' TEST 01 ' + '-'*10)
    start = time.clock()
    p = [92, 57, 49, 68, 60, 43, 67, 84, 87, 72]
    w = [23, 31, 29, 44, 53, 38, 63, 85, 89, 82]
    c = [70, 127]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 333
    assert x == [0,1,1,0,-1,-1,1,-1,-1,-1]
    
    print('\n' + '-'*10 + ' TEST 02 ' + '-'*10)
    start = time.clock()
    p = [110, 150, 70, 80, 30, 5]
    w = [40, 60, 30, 40, 20, 5]
    c = [65, 85]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 345
    assert x == [1,0,-1,1,-1,0]
                  
    print('\n' + '-'*10 + ' TEST 03 ' + '-'*10)
    start = time.clock()
    p = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
    w = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]
    c = [103, 156]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 452
    assert x == [0,-1,0,1,1,0,-1,-1,1,-1]
      
    print('\n' + '-'*10 + ' TEST 04' + '-'*10)
    start = time.clock()
    p = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
    w = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]
    c = [76, 110, 112]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 503
    assert x == [1,1,1,2,1,2,-1,-1,0,2]
      
    print('\n' + '-'*10 + ' TEST 05' + '-'*10)
    start = time.clock()
    p = [78, 77, 35, 34, 33, 89, 88, 36, 35, 94, 93, 75, 74, 74, 73, 79, 78, 80, 79, 16, 15, 10]
    w = [18, 18,  9,  9,  9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40]
    c = [80, 103, 109, 112]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 841
    
    print('\n' + '-'*10 + ' TEST 06' + '-'*10)
    start = time.clock()
    p = [78, 77, 35, 34, 89, 88, 36, 35, 94, 93, 75, 74, 74, 73, 79, 78, 80, 79, 16, 15, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    w = [18, 18, 9, 9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
    c = [80, 90, 100, 110]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 779
    
    print('\n' + '-'*10 + ' TEST 07' + '-'*10)
    start = time.clock()
    p = [78, 77, 35, 34, 33, 89, 88, 36, 35, 94, 93, 75, 74, 74, 73, 79, 78, 80, 79, 16, 15, 10]
    w = [18, 18,  9,  9,  9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40]
    c = [112, 109, 80, 103]
    z,x,bt = mtm(p[::-1], w[::-1], c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 841
    
    print('\n' + '-'*10 + ' TEST 08' + '-'*10)
    start = time.clock()
    p = [78, 77, 35, 34, 33, 89, 88, 36, 35, 94, 93, 75, 74, 74, 73, 79, 78, 80, 79, 16, 15, 10]
    w = [18, 18,  9,  9,  9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40]
    c = [20, 40, 60, 80, 100]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 708
    
    print('\n' + '-'*10 + ' TEST 09' + '-'*10)
    start = time.clock()
    p = [78, 77, 35, 34, 33, 89, 88, 36, 35, 94, 93, 75, 74, 74, 73, 79, 78, 80, 79, 16, 15, 10]
    w = [18, 18,  9,  9,  9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40]
    c = [20, 30, 35, 40, 45, 100]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 615
    
    print('\n' + '-'*10 + ' TEST 10' + '-'*10)
    start = time.clock()
    p = [63, 66, 65, 64, 64, 64, 64, 63, 64, 65, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6]
    w = [420, 510, 510, 420, 420, 390, 420, 510, 420, 510, 270, 300, 330, 360, 390, 420, 450, 480, 510, 540, 570, 600]
    c = [1320, 1320, 1320, 1320]
    z,x,bt = mtm(p, w, c)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 649
    
    print('\n' + '-'*10 + ' TEST 11' + '-'*10)
    start = time.clock()
    p = [63, 66, 65, 64, 64, 64, 64, 63, 64, 65, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6]
    w = [420, 510, 510, 420, 420, 390, 420, 510, 420, 510, 270, 300, 330, 360, 390, 420, 450, 480, 510, 540, 570, 600]
    c = [1320, 1320, 1320, 1320]
    z,x,bt = mtm(p, w, c, 10000)
    print('Total profit: %d' % z)
    print('Solution: %s' % x)
    print('Number of backtracks performed: %d' % bt)
    print('Time spent: %.5f' % round(time.clock()-start,5))
    assert z == 649
    
    print('\nTotal time: %.5f' % round(time.clock()-start_tot,5))
    

if __name__ == '__main__':
    test_mtm()