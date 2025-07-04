##Problem 1
e_estimate_2 = function(n, k) {
  estimates = numeric(k)    # Store estimates
  for (i in 1:k) {
    x = runif(n)            # Generate n random variables from U(0, 1)
    yn = prod(x)^(1/n)      # Geometric mean of x
    estimates[i] = 1 / yn   # Estimate of e
  }
  cumulative_avg = cumsum(estimates) / seq_len(k)  # Cumulative average
  return(cumulative_avg)    # Return cumulative averages
}

n = 100    # Number of random variables per iteration
k = 500    # Total number of iterations

# Generate estimates
cumulative_estimates = e_estimate_2(n, k)


# Plotting the results
plot(cumulative_estimates, type = "l", col = "blue", lwd = 2, ylim = c(2.6, 2.8),
     xlab = "Number of Iterations (k)", ylab = "Estimate of e",
     main = "Convergence of Estimated e")
abline(h = exp(1), col = "red", lwd = 2, lty = 2)  # Reference line for true value of e
legend("bottomright", legend = c("Estimated e", "True e"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))


##PROBLEM 2

est_pi<-function(d,n){
  tot<-0
  inside<-0
  while(TRUE){
    point<-runif(d,min=-0.5,max=0.5)
    dist<-norm(point,"2")
    if(dist<0.5){
      inside<-inside+1
      
      
    }
    tot<-tot+1
    if(inside==n){
      break
    }
    
  }
  
  p<-inside/tot;                  #probabilty of point being inside the sphere 
  pi_approx<-(gamma(d/2 + 1)*(2^d)*p)^(2/d)
  return(pi_approx)
}
est_pi(10,1e4)
points<-numeric(5)
for(i in 1:5){
  points[i]<-est_pi(2*i,1e4)
}


plot(x=c(2,4,6,8,10) ,y =points,ylim = c(3,3.2),type = "l",xlab = "Dimension",ylab="Estimated valu of π")
abline(h=pi,col = "red")


##PROBLEM3
# using  recursion to find  whether a data is random of not

library(tseries)
library(lawstat)
x=rnorm(100)
Check_whether_random<-function(x){
  y=numeric(length(x))
  
  
  x_median<-median(x)
  for(i in 1:length(x)){
    y[i]<-ifelse(x[i]<x_median,0,1)
  }
  result<-runs.test(y)
  
  p_value<-result$p.value
  # print(result)
  if(p_value < 0.05     && !is.na(p_value) ){
    #print(y)
    print("generated data is not random")
  }else{
    print("generated  data is random")
  }
  x_below_median<-x[x<= median(x)]
  x_above_median<-x[x>median(x)]
  if(length(x_below_median)>20){
    Check_whether_random(x_below_median)
  }
  if(length(x_above_median)>20){
    Check_whether_random(x_above_median)
  }
  
}
Check_whether_random(x)