# PROBLEM 2-ESTIMATION OF PI


###  for the  different  dimensions of  spheres with no. of observation=10000
## estimating value of pi for each dimension and then final estimated value 
# is  average of all estimated value value

dimension<-2:14
estimated_pi<-numeric(length(dimension))
n_observation<-100000
for(i in dimension){
  coordinate_array<-array(dim=c(1,n_observation,i))
  for(k in 1:i) {
    coordinate_array[,,k]<-runif(n_observation,-1,1)
  }
  count<-0
  for(j in 1:n_observation){
    coordinates<-as.vector(coordinate_array[,j,])
    if(norm(coordinates,"2") <= 1){
      count<-count+1
    }
  }
  prob <-count/100000      # probability is  no. of points lies in n-dimensional 
                            # sphere as well as n dimensional cube
  
  estimated_pi[i-1]<-((2^i)*prob*gamma((i/2)+1))^(2/i)
  
}
# estimated_pi             # after dimension=14, output is zero
estimated_value<-mean(estimated_pi)
print(estimated_value)




# PROBLEM 3
# check whether Generated observation is Random or Not Random.

install.packages("tseries") 
install.packages("lawstat")
library(tseries)
library(lawstat)

x<-rnorm(100,0,1)
x_median<-median(x)
#Converting the Generated observations to Binary sequence  
# if no.less than median give value 0 otherwise  1
for (i in 1:100){   
  if(x[i]<x_median){
    x[i]<-0
  }else
    x[i]<-1
}
x_factor<-factor(x)
result<-runs.test(x)
p_value<-result$p.value
if(p_value>0.05){
  print("Generated ObservationS are RANDOM ")
}else{
  print("Genrated Observations are not RANDOM")
}



runif(1000)

