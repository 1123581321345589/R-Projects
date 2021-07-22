#newtons method
sqrt2f <- function(x){
  a <- (x^2)-2
  return(a)
}
dsqrt2f <- function(x){
  a <- 2*x
  return(a)
}
#initial guess
x0=sqrt(1)
for (i in 1:10000){
  x1=x0-(sqrt2f(x0)/dsqrt2f(x0))
  print(x1)
  d=abs(x1-x0)
  if (d <= 1*10^-5){
    break
  }else{
    print(d)
    x0=x1
  }
}
print(paste("The root is ", x1))
#NOTES:
#line 2-9 are just declaring function, output is variable a for "answer"
#if you change the 1 for 2 on line 11 it should exit loop on the first run since sqrt(2) is answer
#the d variable stands for differece for checking if the convergence criteria is met.
#Line 20 then makes x0 = x1 so that the program can move foward to second approximation and so on.
#If you put a "?" before a command and execute it will give you the help menu for that command (how to use it ect.)
#