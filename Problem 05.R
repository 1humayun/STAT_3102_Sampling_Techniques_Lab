############################## Problem 05 #####################
pps.function = function(N,n,x, y, total){
   p_i = x/total
   z_i = y/(N*p_i)
   
   ybar = (1/n)*sum(z_i)
   
   var.ybar = (1/(n*(n-1))) * sum((z_i - mean(z_i))^2)
   se.ybar = sqrt(var.ybar)
   
   CI = ybar + c(-1,1)*qnorm(0.975)*(se.ybar)
   
   return(list(
      Estimated.mean = ybar,
      Estimated.variance = var.ybar,
      SE = se.ybar,
      CI = CI
   ))
}

# Function to find the variance of population mean for SRSWR
srs.var.function = function(N, n, y){
   s.sq = var(y)
   f = n/N
   var.ybar = (1-f) * (s.sq/n)
   return(var.ybar)
}


# Given sample data
x.cum = c(5.2,5.9,3.9,4.2,4.7,4.8,4.9,6.8,4.7,5.7,5.2,5.2,4.9,4,1.3,7.4,7.4,4.8,6.2,6.2)
y.cum = c(28,29,30,22,24,25,28,37,26,32,25,38,31,16,6,61,61,29,47,47)

x.lahiri <- c(4.8,4.1,1.3,5.2,6.6,6.0,2.0,6.3,5.2,4.2,4.8,5.9,5.8,5.8,5.1,4.7,5.6,5.2,4.0,4.6)
y.lahiri <- c(22,19,6,25,54,43,4,40,28,29,22,39,39,44,30,27,34,31,18,31)

N = 100
x.total = 484.5
n = 20

# Call the function and perform calculation
pps.cumulative = pps.function(N,n,x.cum,y.cum,x.total)
pps.cumulative 
pps.lahiri = pps.function(N, n, x.lahiri, y.lahiri, x.total)
pps.lahiri


# Gain due to PPS Cumulative sampling compared to SRS sampling
var.pps.cum = pps.cumulative$Estimated.variance
var.srs.cum = srs.var.function(N,n,y.cum)
Gain.cum.srs = ((var.srs.cum - var.pps.cum)/var.pps.cum) *100

# Gain due to PPS Lahiri's sampling compared to SRS sampling
var.pps.lahiri = pps.lahiri$Estimated.variance
var.srs.lahiri = srs.var.function(N,n,y.lahiri)
Gain.pps.lahiri = ((var.srs.lahiri-var.pps.lahiri)/var.pps.lahiri)*100
