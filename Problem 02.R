######################## Problem 02 ########################
pps.cum.one.draw = function(x){
   x.total = sum(x)
   i = sample.int(x.total, 1)
   idx = which(i<= cumsum(x))[1]
   return(idx)
}

x = c(50, 30, 45, 25, 40, 26, 24, 35, 28, 27)
names(x) = paste0("Holding_", 1:length(x))

n = 4
set.seed(123)
pps.cum.draws = replicate(n,pps.cum.one.draw(x))
x[pps.cum.draws]