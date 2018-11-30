library(ncdf4)
library(leaps)
library(pracma)

In <- nc_open("./project 3/matchups_NPP_2018-01.nc")
In.Variance.names()

sst_reg <- ncvar_get(In, "sst_reg")
sst_insitu <- ncvar_get(In, "sst_insitu")
sst_ref <- ncvar_get(In, "sst_ref")
sza <- ncvar_get(In, 'sza')
vza <- ncvar_get(In, 'vza')

lat <- ncvar_get(In, 'latitude')
lon <- ncvar_get(In, 'longitude')
figure
plot(lon, lat, '.')

#to see all the variable names in ncdf file:
names(In$var)


BT_M12 <- ncvar_get(In, "BT_M12")
BT_M13 <- ncvar_get(In, "BT_M13")
BT_M14 <- ncvar_get(In, "BT_M14")
BT_M15 <- ncvar_get(In, "BT_M15")
BT_M16 <- ncvar_get(In, "BT_M16")


dataframe1 = data.frame(sst_insitu, BT_M12, BT_M13, BT_M14, BT_M15, BT_M16)
head(dataframe1)

dataframe2 = data.frame(sza,vza,sst_insitu, BT_M12, BT_M13, BT_M14, BT_M15, BT_M16, sst_ref)
head(dataframe2,10)

day_time = dataframe2[(dataframe2$sza >= 0) & (dataframe2$sza <= 85), ]
head(day_time)

night_time = dataframe2[(dataframe2$sza >= 95) & (dataframe2$sza <= 180), ]
head(night_time)

# Use subset selection methods to find the best two-variable model.
regfit.full = regsubsets(sst_insitu ~ ., data = dataframe1, nvmax=2)
reg.summary = summary(regfit.full)

reg.summary$rsq
reg.summary$which
# The output indicates that the best two-variable model contains only BT_M15 and BT_M16.


# Question 1
b1 = BT_M15
b2 = BT_M16 - BT_M15
model1 = lm(sst_insitu ~ b1 + b2, data = dataframe1)

mean((sst_reg - sst_insitu)^2)

b1 = BT_M16
b2 = BT_M15 - BT_M16
model2 = lm(sst_insitu ~ b1 + b2, data = dataframe1)

# To evaluate the performance of model1, the result is 0.9817122
m = summary(model1)
model1_r_squared = summary(model1)$r.squared
model1_r_squared

model2_r_squared = summary(model2)$r.squared
model2_r_squared

cor(sst_insitu, sst_reg) ^ 2
mean((sst_insitu - sst_reg) ^ 2)
mean(model1$residuals ^ 2)
mean(model2$residuals ^ 2)
mean(model3$residuals ^ 2)
mean(model4$residuals ^ 2)




# Question 2
b1 = BT_M15
b2 = sst_ref * (BT_M15 - BT_M16)
b3 =  (BT_M15 - BT_M16) * (sec(vza) - 1)
model3 = lm(sst_insitu ~ b1 + b2 + b3, data = dataframe1)
model3_r_squared = summary(model3)$r.squared
model3_r_squared





# Question 3
model4 = lm(sst_insitu ~ b1 + b2 + b3 + sst_ref, data = dataframe1)
model4_r_squared = summary(model4)$r.squared
model4_r_squared





# Question 4
T3.7 <- BT_M12   
T4 <- BT_M13 
T8.6 <- BT_M14 
T11 <- BT_M15 
T12 <- BT_M16 

b1 = BT_M15
b2 = BT_M15 - BT_M12
b3 = BT_M15 - BT_M14
b4 = BT_M15 - BT_M16
sec = sec(vza) - 1
b5 = sec
b6 = BT_M15 * sec
b7 = (BT_M15 - BT_M12) * sec
b8 = (BT_M15 - BT_M14) * sec
b9 = (BT_M15 - BT_M16 ) * sec
b10 = BT_M15 * sst_ref 
b11 = (BT_M15 - BT_M12) * sst_ref 
b12 = (BT_M15 - BT_M14) * sst_ref 
b13 = (BT_M15 - BT_M16) * sst_ref 
head(night_time)
dataframe_night = night_time[-c(1)]
head(dataframe_night)
model_night = lm(sst_insitu ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + b11 + b12 + b13, data = dataframe_night)
model_night_r_squared = summary(model_night)$adj.r.squared
model_night_r_squared


b1 = BT_M15
b2 = BT_M15 - BT_M14
b3 = BT_M15 - BT_M16
sec = sec(vza) - 1
b4 = sec
b5 = BT_M15 * sec
b6 = (BT_M15 - BT_M14) * sec
b7 = (BT_M15 - BT_M16) * sec
b8 = BT_M15 * sst_ref 
b9 = (BT_M15 - BT_M14) * sst_ref 
b10 = (BT_M15 - BT_M16) * sst_ref 

model_day = lm(sst_insitu ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10, data = dataframe1)
model_day_r_squared = summary(model_day)$adj.r.squared
model_day_r_squared




library(glmnet)

x=model.matrix(sst_insitu ~  b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + b11 + b12 + b13,dataframe1)[,-1]
y=as.vector(dataframe1$sst_insitu)

grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

set.seed (1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

#set.seed (1)
#cv.out=cv.glmnet(x[train ,],y[train],alpha=0, nfolds=3)
#bestlam=cv.out$lambda.min
#bestlam

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=4)[1:14,]

ridge.pred=predict(ridge.mod,s=4 ,newx=x[test,])
mean((ridge.pred-y.test)^2)


#Question 5
regfit.bwd = regsubsets(sst_insitu ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + b11 + b12 + b13, data = dataframe1, method='backward')
regfit.summary = summary(regfit.bwd)
regfit.summary$which
regfit.summary$adjr2

# The result tells us that only keeping b1,b10 and remove other variables is without significant loss of performance

coef(regfit.bwd, 2)

model6 = lm(sst_insitu ~ b1 + b10, data = dataframe1)
model6_r_squared = summary(model6)$adj.r.squared
model6_r_squared

pred = predict(model6, dataframe1[2:6], interval = "prediction")
mean((pred-y.test)^2)

regfit.fwd = regsubsets(sst_insitu ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 + b11 + b12 + b13, data = dataframe1, method='forward', nvmax = 13)
regfit.summary = summary(regfit.fwd)
regfit.summary$which
which.max(regfit.summary$adjr2)
regfit.summary$adjr2

coef(regfit.fwd, 2)
