yields <- matrix(c(
   4.32, 4.84, 3.96, 4.04,
   4.16, 4.36, 3.50, 5.00,
   3.06, 4.24, 4.76, 3.12,
   4.00, 4.84, 4.32, 3.72,
   4.12, 4.68, 3.46, 4.02,
   4.08, 3.96, 3.42, 3.08,
   5.16, 4.24, 4.96, 3.84,
   4.40, 4.72, 4.04, 3.98,
   4.20, 4.66, 3.64, 5.00,
   4.28, 4.36, 3.00, 3.52
), nrow = 10, byrow = TRUE)
N = 100
M = 16

#............... (i)
# cluster mean
ybar.i = rowMeans(yields)
# Over all mean
ybar = mean(ybar.i)

## Find the variance V(ybar) = (1-f1)*(sb.2/n) + (1-f2)* (sw.2/nm)
n = nrow(yields)
m = ncol(yields)

# between cluster variance
sb.2 = (1/(n-1)) * sum((ybar.i-ybar)^2)

#Within cluster variance
sw.2 = mean(apply(yields, 1, var))

f1 = n/N
f2 = m/M

v.ybar = (1-f1)*(sb.2/n) +(1-f2)*(sw.2/(n*m))
se.ybar = sqrt(v.ybar)

#............ (ii)
# Find the variance of SRS from 2SS sampling
frac = 1/(N*M-1)

part1 = (M*(N-1)*sb.2)
part2 = (N*(M-1) - ((M-m)*(N-1))/m)*sw.2

s2.srs = (part1+part2)*frac
v.srs = ((1/(m*n))-(1/(N*M)))*s2.srs

#.............. (iii)
# Find Optimum  allocations: 100 = 4n+nm
c0 = 100
c1 = 4
c2 = 1

m.opt = sqrt((c1/c2)*(sw.2/(sb.2-sw.2/m)))
m.opt
n.opt = 100/(4+m.opt)
n.opt