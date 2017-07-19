
##hungvd8
#@since 23-05-2017
data <- read.csv("/home/hungdv/rdocker/bras_count_19-5.csv")
MX480 = data[data$bras_id == 'MX480-02',]
MX480_signIn = MX480$signin_total_count
MX480_logOff = MX480$logoff_total_count
MX480_rate = MX480_signIn/MX480_logOff
MX480_signIn = MX480_rate

hist(MX480_signIn, breaks = 30)
plot(as.ts(MX480_signIn),ylab='Signin Count')

days = as.numeric(MX480_signIn)




for (i in 1:45 ) {
 pos = floor(runif(1, 1, 50))
 days[i*15+pos] = days[i*15+pos]^1.2
}
days[510+pos] = 0
plot(as.ts(days))

#Moving Average
# install required library
# install.packages("FBN")
# library(FBN)
 
#decomposed_days = decompose(ts(days, frequency = 7), "multiplicative")
decomposed_days = decompose(ts(na.omit( days), frequency = 60), "multiplicative")

plot(decomposed_days)
#Normal distribution to find min max 
random = decomposed_days$random
min = mean(random, na.rm = T) - 4*sd(random, na.rm = T)
max = mean(random, na.rm = T) + 4*sd(random, na.rm = T)
 
plot(as.ts(as.vector(random)), ylim = c(-0.5,2.5))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)

#find anomaly
position = data.frame(id=seq(1, length(random)), value=random)
#position = data.frame(id=seq(1, length(random)), value=random)

anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
 value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]
 
plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
#Moving Average With Quantite
decomposed_days = decompose(ts(days, frequency = 60), "multiplicative")
plot(decomposed_days)
random = decomposed_days$random

random = days


quantiles = quantile(rm_random,c(.25,.50,.75))
q25Th = quantiles[1]
q50Th = quantiles[2]
q75Th = quantiles[3]   
IQR = q75Th - q25Th
min = q25Th - 1.5*IQR
max = q75Th + 1.5*IQR
position = data.frame(id=seq(1, length(random)), value=random)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")



###############3 Moving Median - Moving Median

#install.packages("forecast")
library(forecast)
library(stats)

trend = runmed(days, 100)
trend = rollmedian(days,1,fill = NA)
plot(as.ts(trend))
detrend = days / as.vector(trend)
m = t(matrix(data = detrend, nrow = 7))
seasonal = colMeans(m, na.rm = T)
random = days / (trend * seasonal)
rm_random = runmed(random[!is.na(random)], 3)
plot(as.ts(rm_random))


#movingMedianAgain()

min = mean(rm_random, na.rm = T) - 4*sd(rm_random, na.rm = T)
max = mean(rm_random, na.rm = T) + 4*sd(rm_random, na.rm = T)
plot(as.ts(random))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)


#find anomaly
position = data.frame(id=seq(1, length(random)), value=random)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value)]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
points(x = anomaly$id, y =anomaly$value, col="#e15f3f")

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
################################################################################################
################################################################################################



data <- read.csv("/home/hungdv/rdocker/bras_count_19-5.csv")
MX480 = data[data$bras_id == 'MX480-02',]
MX480_signIn = MX480$signin_total_count
#hist(MX480_signIn, breaks = 30)
#plot(as.ts(MX480_signIn))

days = as.numeric(MX480_signIn)

### Using  Signma 

mean = mean(days,na.rm = T)
sd = sd(days,na.rm = T)
min = mean - 4*sd
max = mean + 4*sd
#print(mean)
#print(sd)


#find anomaly
position = data.frame(id=seq(1, length(days)), value=days)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value)]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
points(x = anomaly$id, y =anomaly$value, col="#e15f3f")

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")

### Using  Quantite    

quantiles = quantile(days, na.rm = T)
#quantiles = quantile(days,c(.25,.50,.75))

q25Th = quantiles[1]
q75Th = quantiles[3]   
IQR = q75Th - q25Th
min = q25Th - 3.0*IQR
max = q75Th + 3.0*IQR

#find anomaly
position = data.frame(id=seq(1, length(days)), value=days)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value)]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
points(x = anomaly$id, y =anomaly$value, col="#e15f3f")

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
#//////////////////////////////////////////////////////////////////////
#Moving Averaged

#Tren Detach
#Moving Average
# install required library
# install.packages("FBN")
# library(FBN)

#decomposed_days = decompose(ts(days, frequency = 7), "multiplicative")
decomposed_days = decompose(ts(days, frequency = 10), "multiplicative")
decomposed_days = decompose(ts(days, frequency = 60), "multiplicative")
decomposed_days = decompose(ts(days, frequency = 100), "multiplicative")
decomposed_days = decompose(ts(days, frequency = 200), "multiplicative")
decomposed_days = decompose(ts(days), "multiplicative")

plot(decomposed_days)
#Normal distribution to find min max 
random = decomposed_days$random
min = mean(random, na.rm = T) - 4*sd(random, na.rm = T)
max = mean(random, na.rm = T) + 4*sd(random, na.rm = T)

plot(as.ts(as.vector(random)), ylim = c(-0.5,2.5))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)

#find anomaly
position = data.frame(id=seq(1, length(random)), value=random)
#position = data.frame(id=seq(1, length(random)), value=random)

anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
#Moving Average With Quantite
decomposed_days = decompose(ts(days, frequency = 60), "multiplicative")
plot(decomposed_days)
random = decomposed_days$random
quantiles = quantile(random, na.rm = T)
#quantiles = quantile(days,c(.25,.50,.75))

q25Th = quantiles[1]
q75Th = quantiles[3]   
IQR = q75Th - q25Th
min = q25Th - 3.0*IQR
max = q75Th + 3.0*IQR
position = data.frame(id=seq(1, length(random)), value=random)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")



###############3 MOVING MEDIAN
#not good.
#install.packages("forecast")
library(forecast)
library(stats)

trend = runmed(days, 10)
   
#movingMedianAgain()

min = mean(rm_random, na.rm = T) - 4*sd(rm_random, na.rm = T)
max = mean(rm_random, na.rm = T) + 4*sd(rm_random, na.rm = T)
plot(as.ts(random))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)


#find anomaly
position = data.frame(id=seq(1, length(random)), value=random)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value)]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
points(x = anomaly$id, y =anomaly$value, col="#e15f3f")

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")


#############################################
#good
trend = rollmedian(days,60,fill = NA)
trend = rollmedian(days,7,fill = NA)
trend = runmed(days,61)
trend = rollmedian(days,5,align = "right",fill = NA)
plot(as.ts(trend))
detrend = days / as.vector(trend)
m = t(matrix(data = detrend, nrow = 7))
#m = t(matrix(data = detrend, nrow = length(detrend)))

seasonal = colMeans(m, na.rm = T)
random = days / (trend * seasonal)
# thu khong tinh moving meadian tren moving meadian.
#movingMedianAgain()
#rm_random = runmed(random[!is.na(random)], 3)
#rm_random = runmed(random[!is.na(random)], 5) #not good.
# higher mean more sensitive
#rm_random = runmed(random[!is.na(random)], 5)
#rm_random = runmed(random[!is.na(random)], 10)
#plot(as.ts(rm_random))




#min = mean(rm_random, na.rm = T) - 4*sd(rm_random, na.rm = T)
#max = mean(rm_random, na.rm = T) + 4*sd(rm_random, na.rm = T)
quantiles = quantile(random, na.rm = T)
#quantiles = quantile(rm_random, na.rm = T)
q25Th = quantiles[1]
q75Th = quantiles[3]   
IQR = q75Th - q25Th
min = q25Th - 3.0*IQR
max = q75Th + 3.0*IQR

plot(as.ts(random))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)


#find anomaly
position = data.frame(id=seq(1, length(random)), value=random)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value)]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
points(x = anomaly$id, y =anomaly$value, col="#e15f3f")

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")






library("zoo")
x  = 0:100

ts = as.ts(x)

trend  = rollmedian(ts,5,fill = NA)
detrend = ts- trend
trend = runmed(ts, 5)
plot(detrend)










