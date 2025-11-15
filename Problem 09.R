#########################  Problem 09 ######################
Mi = c(52, 56, 60, 46, 49, 51, 50, 61, 60, 45)

p1= c(12,11,12,10,13)
p2 = c(10,9,7,9,8,10)
p3 = c(6,5,7,5,6,4)
p4 = c(7, 8, 7, 7, 6)
p5 = c(10, 11, 13, 12, 12)
p6 = c(14, 15, 13, 12, 13)
p7 = c(6, 7, 6, 8, 7)
p8 = c(9, 10, 8, 9, 9, 10)
p9 = c(7, 10, 8, 9, 9, 10)
p10 = c(12, 11, 12, 13, 12, 12)


data = list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p1)
data

Mbar = mean(Mi)
mi = sapply(data, length)
mi
ybar.i = sapply(data, mean)
ybar.i

N = 50
n = 10
ybar = (1/(n*Mbar))*sum(Mi*ybar.i)
ybar

# Find variance
Wi = Mi/Mbar
s2.b = (1/(n-1))*sum((Wi*ybar.i - ybar)^2)
s2.b

s2.wi = sapply(data, var)
s2.wi

# For variance formula
part1 = ((1/n)-(1/N))*s2.b
part2 = (1/(n*N))*sum(Wi^2 *((1/mi)-(1/Mi))* s2.wi)
v.ybar = part1 + part2
v.ybar

se.ybar = sqrt(v.ybar)
se.ybar

CI = ybar + c(-1,1)*qnorm(0.975)*se.ybar
CI
