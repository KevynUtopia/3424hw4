
# Q2
Data = read.table("./Presidential Election Data.txt", header=TRUE)
Data$G_I <- Data$G * Data$I
model <- lm(V ~ I + D + W + G_I + P + N, data=Data)

summary(model)

model_alt <- lm(V ~ D + W + G_I + P + N, data=Data)
summary(model_alt)

model_alt <- lm(V ~ I + D + W + P + N, data=Data)
summary(model_alt)

Data$G_I <- Data$G * Data$I
Data$P_I <- Data$P * Data$I
Data$N_I <- Data$N * Data$I
model_1 <- lm(V ~ I + D + W + G_I + P_I + N_I, data=Data)
summary(model_1)

Data$P_D <- Data$P * Data$D
Data$N_D <- Data$N * Data$D
model_2 <- lm(V ~ I + D + W + G_I + P_D + N_D, data=Data)
summary(model_2)

Data$logP_D <- log(Data$P) * Data$D
Data$logP <- log(Data$P)
Data$logN_I <- log(Data$N) * Data$I
model_3 <- lm(V ~ I + D +  G_I + logP_D  + logN_I + logP, data=Data)
summary(model_3)


# Q3
Data = read.table("./Presidential Election Data.txt", header=TRUE)
Data$D1 <- (Data$D==1)*1
Data$D2 <- (Data$D==-1)*1
Data$G_I <- Data$G * Data$I
Data$D1_D2 <- Data$D1 - Data$D2
model_2 <- lm(V ~  I + D1_D2 + D2 + W + G_I + P + N, data=Data)
summary(model_2)
model_2_alt <- lm(V ~  I + D1_D2 + W + G_I + P + N, data=Data)
summary(model_2_alt)

# Q5
# a
Data = read.table("./Presidential Election Data.txt", header=TRUE)
Data$Y <- log(Data$V/(1-Data$V))
Data$G_I <- Data$G * Data$I


model1 = lm(V ~ I + D + W + G_I + P + N, Data)
model2 = lm(Y ~ I + D + W + G_I + P + N, Data)
plot(model1)
plot(model2)
summary(model1)
summary(model2)

# b
Data = read.table("./Presidential Election Data.txt", header=TRUE)
Data$t <- exp(Data$V/(1-Data$V))
Data$G_I <- Data$G * Data$I
model = lm(V ~ I + D + W + G_I + P + N, Data)
model3 = lm(t ~ I + D + W + G_I + P + N, Data)
plot(model)
plot(model3)
summary(model)
summary(model3)

# Q6
# a
Data = read.table("./Presidential Election Data.txt", header=TRUE)
Data$Y <- log(Data$V/(1-Data$V))
Data$G_I <- Data$G * Data$I
Data$D1 <- (Data$D==1)*1
Data$D2 <- (Data$D==-1)*1

model1 = lm(V ~ I + D1 + D2 + W + G_I + P + N, Data)
model2 = lm(Y ~ I + D1 + D2 + W + G_I + P + N, Data)

plot(model1)
plot(model2)
summary(model1)
summary(model2)

# b
Data = read.table("./Presidential Election Data.txt", header=TRUE)
Data$t <- exp(Data$V/(1-Data$V))
Data$G_I <- Data$G * Data$I
Data$D1 <- (Data$D==1)*1
Data$D2 <- (Data$D==-1)*1
model = lm(V ~ I + D1 + D2 + W + G_I + P + N, Data)
model3 = lm(t ~ I + D1 + D2 + W + G_I + P + N, Data)
plot(model)
plot(model3)
summary(model)
summary(model3)

