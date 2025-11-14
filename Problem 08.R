######################### Problem 08 ###############################

# Function for SRSWR calculation:
srs.function = function(N, n, y){
   set.seed(123)
   sample.srs = sample(N, n)
   sample.y.srs = y[sample.srs]
   
   ybar = mean(sample.y.srs)     # Estimate of Population Mean
   
   s.sq = var(sample.y.srs)      # Sample Variance
   f = n/N
   v.ybar = (1-f)* (s.sq/n)      # Estimate of Variance of Population mean
   se.ybar = sqrt(v.ybar)        #SE of Population mean
   
   CI = ybar + c(-1, 1)*qnorm(0.975)*se.ybar
   
   return(list(
      Sampled.district.srs = district[sample.srs],
      Estimated.mean = ybar,
      Estimated.variance = v.ybar,
      SE = se.ybar,
      CI = CI
   ))
}

# Function for PPSWR calculation:
pps.function = function(N,n,x,y,total){
   p_i = x/total
   z_i = y/(N*p_i)
   
   ybar = (1/n)*sum(z_i)
   
   var.ybar = (1/(n*(n-1)))*sum((z_i-mean(z_i))^2)
   se.ybar = sqrt(var.ybar)
   
   CI = ybar + c(-1,1)*qnorm(0.975)*se.ybar
   
   return(list(
      Estimated.mean = ybar,
      Estimated.variance = var.ybar,
      SE = se.ybar,
      CI = CI
   ))
}


# Single sample draw function for Cumulative Total method:
pps.cum.one.draw = function(x){
   x.total = sum(x)
   i = sample.int(x.total, 1)
   idx = which(i <= cumsum(x))[1]
   return(idx)
}

# Single sample draw function for lahiri's method:
pps.lahiri.one.draw = function(x){
   N = length(x)
   M = max(x)
   repeat{
      i = sample.int(N,1)
      j = sample.int(M, 1)
      if (j <= x[i])
         return(i)
   }
}


# Given data
district = c("Banderban", "Chittagong", "Khagrachari", "Comilla", "Noakhali", "Rangamati", "Sylhet", "Dhaka", "Faridpur", "Jamalpur", "Kishorganj", "Mymensingh", "Tangail", "Barisal", "Jessore", "Khulna", "Kushtia", "Patuakhali", "Bogra", "Dinajpur",
             "Pabna", "Rajshahi", "Rangpur")

x = c(61, 1079, 30, 1519, 1036, 48, 2309, 936, 1018, 811, 1341, 1715, 561, 133, 1452, 1134, 567, 1027, 1169, 1573, 738, 1799, 2243)

y = c(48, 994, 26, 1313, 779, 40, 1512, 859, 577, 723, 1121, 928, 483, 662, 1352, 853, 479, 543, 1093, 1069, 660, 1753, 1873)

N = length(y)
n = 5
total = sum(x)

# Draw n sample using PPS Cumulative total method
set.seed(123)
pps.cum.draws = replicate(n, pps.cum.one.draw(x))
selected.district = district[pps.cum.draws]
x.cum = x[pps.cum.draws] 
y.cum = y[pps.cum.draws]

# Draw n sample using PPS Lahiri's method
set.seed(123)
pps.lahiri.draws = replicate(n, pps.lahiri.one.draw(x))
selected.district.L = district[pps.lahiri.draws]
x.lahiri = x[pps.lahiri.draws]
y.lahiri = y[pps.lahiri.draws]


# Call the function and make calculations
SRS = srs.function(N,n,y)
Cumulative = pps.function(N, n, x.cum, y.cum, total)
Lahiri = pps.function(N, n, x.lahiri, y.lahiri, total)

# Form a matrix to represent the results
srs.data = round(c(SRS$mean, SRS$variance, SRS$SE),2)
cum.data = round(c(Cumulative$mean, Cumulative$variance, Cumulative$SE),2)
lahiri.data = round(c(Lahiri$mean, Lahiri$variance, Lahiri$SE),2)

result = matrix(c(srs.data, cum.data, lahiri.data), nrow = 3,byrow=FALSE)
rownames(result) = c("SRS", "Cumualative", "Lahiri")
colnames(result) = c("Estimated Mean", "Estimated Variance","Standard Error")
result