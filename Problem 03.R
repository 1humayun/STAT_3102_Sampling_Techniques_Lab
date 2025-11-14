######################## Problem 03 ########################
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

x = c(150, 50, 100, 200, 160, 40)
names(x) = paste0("Orchard_", 1:length(x))

n = 3
set.seed (123)
pps.lahiri.draws = replicate(n, pps.lahiri.one.draw(x))
x[pps.lahiri.draws]
