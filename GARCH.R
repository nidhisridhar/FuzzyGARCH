

x=garch(in_data,order=c(1,1))
logLik(x)
AIC(x)

al0=0.011289 
al1= 0.073818
be1= 0.916907

h=array(data=0,dim=c(1999))
h[1]=alpha0
for(t in 2 : 1999){
  h[t] = al0 + (al1*in_data[t-1]*in_data[t-1]) + (be1*h[t-1])
}
noise <- rnorm(1999)
scale <- function(noise){(noise-min(noise))/(max(noise)-min(noise))*(max(in_data)-min(in_data))+min(in_data)}
w.noise <- scale(noise)

predicted_value1=array(0.1,dim=c(1999))
for(i in 1:1999){
  predicted_value1[i] = sqrt(h[i])*w.noise[i]
  if(is.nan(predicted_value1[i])){
    predicted_value1[i] = 0
  }
}
plot(predicted_value1,type='l')

