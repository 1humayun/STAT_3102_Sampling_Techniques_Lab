######################### Problem 10 ###############################
# Stratum sizes
n_i = c(1580, 430)
N = 2010

#weights
w_i = n_i/N

#overall mean and strata mean
ybar_i = c(19.40, 15.63)
ybar = 26.30

# Variance within strata 
sy2_i = c(312, 922)
s2 = 620

# Given cost
c0 = 100    # budget
c1 = 0.1    # cost to measure x (area size- cheap)
c2 = 1      # cost to measure y (weate- costly)

# Now 
v = sum(w_i*sy2_i)
v_prime = sum(w_i *(ybar_i - ybar)^2)

# ratio
r = sqrt((v/v_prime)*(c1/c2))

n_opt_prime = round(c0/(c1+c2*r))      # optimum x sizes
n_opt = round(n_opt_prime * r)         # optimum sample size (y)


# Optimum variance
v_opt = v/n_opt + v_prime/n_opt_prime

# variance for SRS
n=100
v_srs = s2/n

# Relative efficiency
RE = (v_srs/v_opt)*100

# a gain of 12.73% obtained from double sampling
