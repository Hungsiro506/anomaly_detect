
set.seed(4)
data <- read.csv("webTraffic.csv", sep = ",", header = T)
days = as.numeric(data$Visite)
for (i in 1:45 ) {
 pos = floor(runif(1, 1, 50))
 days[i*15+pos] = days[i*15+pos]^1.2
}
days[510+pos] = 0
plot(as.ts(days))

#Moving Average
# install required library
install.packages("FBN")
library(FBN)
 
decomposed_days = decompose(ts(days, frequency = 7), "multiplicative")
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
#####################################################################

#Quantite
#find anomaly
quantiles = quantile(data$Visite,c(.25,.50,.75))
v25Th = quantiles[1]
v50Th = quantiles[2]
v75Th = quantiles[3]   
IQR = v75Th - v25Th
lowerThreasHold = v25Th - 1.5*IQR
upperThreasHold = v75Th + 1.5*IQR



visite = data$Visite
position = data.frame(id=seq(1, length(visite)), value=visite)
anomalyH = position[position$value > upperThreasHold, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < lowerThreasHold, ]
anomalyL = anomalyL[!is.na(anomalyL$value)]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
 value=c(anomalyH$value, anomalyL$value))
points(x = anomaly$id, y =anomaly$value, col="#e15f3f")
 
plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")


###############################################################

movingMedian <- rollmedian(data$Visite,11,fill = NA)
plot(as.ts(movingMedian))
movingMedian <- rollmedian(data$Visite,21,fill = NA)
plot(as.ts(movingMedian))
movingMedian <- rollmedian(data$Visite,31,fill = NA)
plot(as.ts(movingMedian))
movingMedian <- rollmedian(data$Visite,3,fill = NA)
plot(as.ts(movingMedian))
movingMedian <- rollmedian(data$Visite,5,fill = NA)
plot(as.ts(movingMedian))
#### Seem to be the best efford
movingMedian <- rollmedian(data$Visite,7,fill = NA)
plot(as.ts(movingMedian))
####
movingMedian <- rollmedian(data$Visite,7,fill = NA)
plot(as.ts(movingMedian))

#install library
install.packages("forecast")
library(forecast)
library(stats)

trend = runmed(days, 7)
plot(as.ts(trend))

#trend = movingMedian
#detrend = days/as.vector(trend)

detrend = days / as.vector(trend)
m = t(matrix(data = detrend, nrow = 7))
seasonal = colMeans(m, na.rm = T)
random = days / (trend * seasonal)
rm_random = runmed(random[!is.na(random)], 3)
 
# min = mean(rm_random, na.rm = T) - 4*sd(rm_random, na.rm = T)
# max = mean(rm_random, na.rm = T) + 4*sd(rm_random, na.rm = T)


quantiles = quantile(rm_random,c(.25,.50,.75))
q25Th = quantiles[1]
q50Th = quantiles[2]
q75Th = quantiles[3]   
IQR = q75Th - q25Th
min = q25Th - 1.5*IQR
max = q75Th + 1.5*IQR

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





### Load data.


data <- read.csv("/home/hungdv/rdocker/bras_count_19-5.csv")
MX480 = data[data$bras_id == 'MX480-02',]
MX480_signIn = MX480$signin_total_count
#hist(MX480_signIn, breaks = 30)
#plot(as.ts(MX480_signIn))

days = as.numeric(MX480_signIn)

### Using  Signma 

mean = mean(dsays)
sd = sd(days)







