############################## Problem 04 #####################
pps.lahiri.one.draw = function(x){
   N = length(x)
   M = max(x)
   repeat{
      i = sample.int(N, 1)
      j = sample.int(M, 1)
      if (j <= x[i])
         return(i)
   }
}

set.seed(123)
x = c(50, 30, 45, 25, 40, 26, 24, 35, 28, 27 )
names(x) = paste0("Holding_", 1:length(x))

n = 4
set.seed (123)
pps.lahiri.draws = replicate(n, pps.lahiri.one.draw(x))
x[pps.lahiri.draws]

