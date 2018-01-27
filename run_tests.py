from mkp.algorithms import mtm

print('-'*10 + ' TEST 01 ' + '-'*10)
p = [92, 57, 49, 68, 60, 43, 67, 84, 87, 72]
w = [23, 31, 29, 44, 53, 38, 63, 85, 89, 82]
c = [70, 127]
z,x,bt = mtm(p, w, c)
print('Total profit: %d' % z)
print('Solution: %s' % x)
print('Number of backtracks performed: %d' % bt)
assert z == 333
assert x == [0,1,1,0,-1,-1,1,-1,-1,-1]

print('\n\n' + '-'*10 + ' TEST 02 ' + '-'*10)
p = [110, 150, 70, 80, 30, 5]
w = [40, 60, 30, 40, 20, 5]
c = [65, 85]
z,x,bt = mtm(p, w, c)
print('Total profit: %d' % z)
print('Solution: %s' % x)
print('Number of backtracks performed: %d' % bt)
assert z == 345
assert x == [1,0,-1,1,-1,0]
              
print('\n\n' + '-'*10 + ' TEST 03 ' + '-'*10)
p = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
w = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]
c = [103, 156]
z,x,bt = mtm(p, w, c)
print('Total profit: %d' % z)
print('Solution: %s' % x)
print('Number of backtracks performed: %d' % bt)
assert z == 452
assert x == [0,-1,0,1,1,0,-1,-1,1,-1]
  
print('\n\n' + '-'*10 + ' TEST 04' + '-'*10)
p = [78, 35, 89, 36, 94, 75, 74, 79, 80, 16]
w = [18, 9, 23, 20, 59, 61, 70, 75, 76, 30]
c = [76, 110, 112]
z,x,bt = mtm(p, w, c)
print('Total profit: %d' % z)
print('Solution: %s' % x)
print('Number of backtracks performed: %d' % bt)
assert z == 503
assert x == [1,1,1,2,1,2,-1,-1,0,2]
  
print('\n\n' + '-'*10 + ' TEST 05' + '-'*10)
p = [78, 77, 35, 34, 33, 89, 88, 36, 35, 94, 93, 75, 74, 74, 73, 79, 78, 80, 79, 16, 15, 10]
w = [18, 18,  9,  9,  9, 23, 23, 20, 20, 59, 59, 61, 61, 70, 70, 75, 75, 76, 76, 30, 30, 40]
c = [80, 103, 109, 112]
z,x,bt = mtm(p, w, c)
print('Total profit: %d' % z)
print('Solution: %s' % x)
print('Number of backtracks performed: %d' % bt)
assert z == 841
assert x == [0,1,1,1,3,2,2,3,3,0,1,2,3,-1,-1,-1,-1,-1,-1,-1,-1,-1]
