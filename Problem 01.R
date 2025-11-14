##########################  Problem 01  ##########################
# Single sample draw function for PPS Cumulative total method
pps.cum.one.draw = function(x){
   x.total = sum(x)
   i = sample.int(x.total, 1)
   idx = which(i <= cumsum(x))[1]
   return (idx)
} 

x = c(150,50,100,200,160)
names(x) = paste0("Orchard_", 1:length(x))

# Draw n sample using PPS cumulative total method
n=3
set.seed(123)
pps.cum.draws = replicate(n, pps.cum.one.draw(x))
x[pps.cum.draws]



