# Read dataset from .csv file
#DJA <- read.csv("C:\\Users\balaji\Desktop\DJA.csv")taset from .csv file
DJA <- read.csv("C:\\Users\\balaji\\Desktop\\DJA.csv")
# import library tseries
library("tseries", lib.loc="~/R/win-library/3.3")
library("GA", lib.loc="~/R/win-library/3.3")
#import GA library
#library("GA", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")

#Store the daily closing values of NSADAQ dataset into stock_values
stock_values=DJA[1:2002,2]
# Generate daily stock-return series by taking the logarithmic difference of the daily stock index * 100
log_d=diff(log(stock_values))
log_d1=log_d*100
#Get the first 2000 values
in_data=log_d1[1:1999]


alpha0=c(0.1 ,0.2,0.3,0.4,0.5)
alpha1=c(0.1,0.2,0.3,0.4,0.5)
beta1=c(0.1,0.2,0.3,0.4,0.5)
center=array(data=(0.1) , dim=c(5,5))
spread=array(data=(0.1) , dim=c(5,5))
h=array(data=0,dim=c(1999))

# Get the heteroscedasticity for each fuzzy rule
get_h <-function(l){
  h[1]=alpha0[l]
  for(t in 2 : 1900){
    h[t] = alpha0[l]+(alpha1[l]*in_data[t-1]*in_data[t-1])+(beta1[l]*h[t-1])
  }
  return(h)
}

# Get membership values for each fuzzy rule
membership <- function(l){
  ul=array(0.1, dim=c(1900))
  for(t in 6:1900){
    init = exp((-0.5)*((in_data[t-1]-center[l,1])/spread[l,1])^2)
    for(j in 2:5){
      ul[t] = init * exp((-0.5)*((in_data[t-1]-center[l,j])/spread[l,j])^2)
      init = ul[t]
    }
  }
  return(ul)
}


# Set the fitness function
parameters=array(0.1,dim=c(65))

# Create arrays for storing intermediate values
gl = array(0.1,dim=c(5,1999))
sum_ht = array(0.1,dim=c(5,1999))

predicted_value=array(0.1,dim=c(1900))

fitness <- function(parameters)
{
  center=array(data=(0.1) , dim=c(5,5))
  spread=array(data=(0.1) , dim=c(5,5))
  
  center[1,]=parameters[1:5]
  center[2,]=parameters[6:10]
  center[3,]=parameters[11:15]
  center[4,]=parameters[16:20]
  center[5,]=parameters[21:25]
  
  spread[1,]=parameters[26:30]
  spread[2,]=parameters[31:35]
  spread[3,]=parameters[36:40]
  spread[4,]=parameters[41:45]
  spread[5,]=parameters[46:50]
  
  alpha0=parameters[51:55]
  alpha1=parameters[56:60]
  beta1=parameters[61:65]
  #h=array(data=0,dim=c(1999))
  
  # Summation of membership values
  sum_membership = membership(1)+membership(2)+membership(3)+membership(4)+membership(5)
  
 
  
  # Calculate summation of h
  for(j in 1:5){
    temp_membership = membership(j)
    temp_h=get_h(j)
 #   print(temp_h)
    for(i in 1:1900){
      gl[j,i]=temp_membership[i]/sum_membership[i]
      sum_ht[j,i]=gl[j,i]*temp_h[i]
  #    print(temp_h[i])
    }
  }
  
  #Create white noise process
  noise <- rnorm(1900)
  scale <- function(noise){(noise-min(noise))/(max(noise)-min(noise))*(max(in_data)-min(in_data))+min(in_data)}
  w.noise <- scale(noise)
  
  # Predict stock market data using Fuzzy GARCH
  
  for(i in 1:1900){
    predicted_value[i] = sqrt(sum(sum_ht[,i]))*w.noise[i]
    if(is.nan(predicted_value[i])){
      predicted_value[i] = 0
    }
  }
  
  #print(predicted_value)
  
  return(sum((in_data[1:1900]-predicted_value)^2))
  
}

# Set min and max values for GA
c0=c(-0.07,-0.07,-0.07,-0.07,-0.07)
c1=c2=c3=c4=c0
s0=c(0,0,0,0,0)
s1=s2=s3=s4=s0
alpha0=c(0,0,0,0,0)
alpha1=beta1=alpha0
min_f=c(c0,c1,c2,c3,c4,s0,s1,s2,s3,s4,alpha0,alpha1,beta1)

c0=c(0.07,0.07,0.07,0.07,0.07)
c1=c2=c3=c4=c0
s0=c0
s1=s2=s3=s4=s0
alpha0=c(0.5,0.5,0.5,0.5,0.5)
alpha1=alpha0
beta1=c(1,1,1,1,1)
max_f=c(c0,c1,c2,c3,c4,s0,s1,s2,s3,s4,alpha0,alpha1,beta1)

# Call GA with the defined fitness function
GA <- ga(type = "real-valued",
         fitness = function(x) 1/fitness(parameters),
         min = min_f, max = max_f,
         popSize = 30, maxiter =100,
         pcrossover = 0.8,
         pmutation = 0.1)

# solution stores our parameter values
parameters=summary(GA)$solution


center=array(data=(0.1) , dim=c(5,5))
spread=array(data=(0.1) , dim=c(5,5))
center[1,]=parameters[1:5]
center[2,]=parameters[6:10]
center[3,]=parameters[11:15]
center[4,]=parameters[16:20]
center[5,]=parameters[21:25]

spread[1,]=parameters[26:30]
spread[2,]=parameters[31:35]
spread[3,]=parameters[36:40]
spread[4,]=parameters[41:45]
spread[5,]=parameters[46:50]

alpha0=parameters[51:55]
alpha1=parameters[56:60]
beta1=parameters[61:65]

#h=array(data=0,dim=c(1990))

membership2 <- function(l){
  ul=array(0.1, dim=c(99))
  for(t in 1901:1999){
    init = exp((-0.5)*((in_data[t-1]-center[l,1])/spread[l,1])^2)
    for(j in 2:5){
      ul[t-1901] = init * exp((-0.5)*((in_data[t-1]-center[l,j])/spread[l,j])^2)
      init = ul[t-1901]
    }
  }
  return(ul)
}


# Summation of membership values
sum_membership2=array(0.1, dim=c(99))
sum_membership2 = membership2(1)+membership2(2)+membership2(3)+membership2(4)+membership2(5)

get_h2 <-function(l){
  #h2=array(0, dim=c(99))
  #print(h[1900])
  t_temp=get_h(l)
  for(t in 1901 : 1999){
    t_temp[t] = alpha0[l]+(alpha1[l]*in_data[t-1]*in_data[t-1])+(beta1[l]*t_temp[t-1])
   # print(h[t])
  }
  return(t_temp)
}

# Calculate summation of h
for(j in 1:5){
  temp_membership = membership2(j)
  #print(temp_membership)
  temp_h=get_h2(j)
  #print(temp_h)
  for(i in 1901:1999){
   # print(sum_membership2[i-1900])
    gl[j,i]=temp_membership[i-1900]/sum_membership2[i-1900]
    #print (gl[j,i])
    sum_ht[j,i]=gl[j,i]*temp_h[i]
   # print(sum_ht[j,i])
  }
}

#Create white noise process
noise <- rnorm(1999)
scale <- function(noise){(noise-min(noise))/(max(noise)-min(noise))*(max(in_data)-min(in_data))+min(in_data)}
w.noise <- scale(noise)

# Predict stock market data using Fuzzy GARCH
predicted_value2=array(0.1,dim=c(99))
for(i in 1:99){
  predicted_value2[i] = sqrt(sum(sum_ht[,1900+i]))*w.noise[1900+i]
  #print(predicted_value2[i])
  if(is.nan(predicted_value2[i])){
    predicted_value2[i] = 0
  }
}

print(sum((in_data[1901:1999]-predicted_value2)^2))
plot(in_data[1901:1999],type='l')
plot(predicted_value2,type='l')