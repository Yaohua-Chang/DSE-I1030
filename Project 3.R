
library(ncdf4)
library(leaps)
library(pracma)

In <- nc_open("/Users/changyaohua/Desktop/project\ 3/matchups_NPP_2018-01.nc")
In2 <- nc_open("/Users/changyaohua/Desktop/project\ 3/matchups_NPP_2018-02.nc")

#to see all the variable names in ncdf file:
names(In$var)

sst_reg <- ncvar_get(In, "sst_reg")
sst_insitu <- ncvar_get(In, "sst_insitu")
sst_ref <- ncvar_get(In, "sst_ref")
sza <- ncvar_get(In, 'sza')
vza <- ncvar_get(In, 'vza')

BT_M12 <- ncvar_get(In, "BT_M12")
BT_M13 <- ncvar_get(In, "BT_M13")
BT_M14 <- ncvar_get(In, "BT_M14")
BT_M15 <- ncvar_get(In, "BT_M15")
BT_M16 <- ncvar_get(In, "BT_M16")


dataframe1 = data.frame(sst_insitu, BT_M12, BT_M13, BT_M14, BT_M15, BT_M16)
head(dataframe1)


# Question 1

# Use subset selection methods to find the best two-variable model.
regfit.full = regsubsets(sst_insitu ~ ., data = dataframe1, nvmax=2)
reg.summary = summary(regfit.full)

reg.summary$rsq
reg.summary$which
# The output indicates that the best two-variable model contains only BT_M15 and BT_M16.

model1 = lm(sst_insitu ~ BT_M15 + I(BT_M16 - BT_M15), data = dataframe1)

model2 = lm(sst_insitu ~ BT_M16 + I(BT_M15 - BT_M16), data = dataframe1)

# To evaluate the performance of model1, the adjusted R squared is 0.979362
summary1 = summary(model1)
model1_adj_r_squared = summary1$adj.r.squared
model1_adj_r_squared
summary1$coefficients
sqrt(mean(model1$residuals ^ 2))

# To evaluate the performance of model2, the adjusted R squared is 0.979362 as well
summary2 = summary(model2)
model2_adj_r_squared = summary2$adj.r.squared
model2_adj_r_squared
summary2$coefficients
sqrt(mean(model2$residuals ^ 2))


#MSE: mean squared error
#cor(sst_insitu, sst_reg) ^ 2
sqrt(mean((sst_insitu - sst_reg) ^ 2))







# Question 2
model3 = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M16):sst_ref + I(BT_M15 - BT_M16):sec, data = dataframe1)
summary3 = summary(model3)
model3_adj_r_squared = summary3$adj.r.squared
model3_adj_r_squared
summary3$coefficients

sqrt(mean(model3$residuals ^ 2))




# Question 3
model4 = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M16):sst_ref + I(BT_M15 - BT_M16):sec + sst_ref, data = dataframe1)
summary4 = summary(model4)
model4_adj_r_squared = summary4$adj.r.squared
model4_adj_r_squared
summary4$coefficients

sqrt(mean(model4$residuals ^ 2))



# Question 4

#T3.7 <- BT_M12   
#T4 <- BT_M13 
#T8.6 <- BT_M14 
#T11 <- BT_M15 
#T12 <- BT_M16 

sec = sec(vza) - 1
dataframe2 = data.frame(sza,sec,sst_insitu, BT_M12, BT_M13, BT_M14, BT_M15, BT_M16, sst_ref)
head(dataframe2,10)

day_time = dataframe2[(dataframe2$sza >= 0) & (dataframe2$sza <= 85), ]
head(day_time)

night_time = dataframe2[(dataframe2$sza >= 95) & (dataframe2$sza <= 180), ]
head(night_time)

model_night = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M12) + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) 
                 + sec + BT_M15:sec + I(BT_M15 - BT_M12):sec + I(BT_M15 - BT_M14):sec 
                 + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref + I(BT_M15 - BT_M12):sst_ref 
                 + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, data = night_time)
summary_night = summary(model_night)
model_night_r_squared = summary_night$adj.r.squared
model_night_r_squared
summary_night$coefficients

sqrt(mean(model_night$residuals ^ 2))


model_day = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) + sec + BT_M15:sec 
               + I(BT_M15 - BT_M14):sec + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref 
               + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, data = day_time)
summary_day = summary(model_day)
model_day_r_squared = summary_day$adj.r.squared
model_day_r_squared
summary_day$coefficients

sqrt(mean(model_day$residuals ^ 2))



library(glmnet)

x_night=model.matrix(sst_insitu ~  BT_M15 + I(BT_M15 - BT_M12) + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) 
               + sec + BT_M15:sec + I(BT_M15 - BT_M12):sec + I(BT_M15 - BT_M14):sec 
               + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref + I(BT_M15 - BT_M12):sst_ref 
               + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, night_time)[,-1]
y_night=as.vector(night_time$sst_insitu)

set.seed (1)
train=sample(1:nrow(x_night), nrow(x_night)/2)
test=(-train)
y_night.test=y_night[test]

#set.seed (1)
#cv.out=cv.glmnet(x[train ,],y[train],alpha=0, nfolds=3)
#bestlam=cv.out$lambda.min
#bestlam

grid=10^seq(10,-2,length=100)

out=glmnet(x_night,y_night,alpha=0)
ridge.mod=glmnet(x_night,y_night,alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=min(out$lambda) ,newx=x_night[test,])
predict(out,type="coefficients",s=min(out$lambda))[1:14,]
mean((ridge.pred-y_night.test)^2)

out=glmnet(x,y,alpha=1)
lasso.mod=glmnet(x_night[train ,],y_night[train],alpha=1,lambda=grid)
lasso.pred=predict(lasso.mod,s=min(out$lambda) ,newx=x_night[test,])
predict(out,type="coefficients",s=min(out$lambda))[1:14,]
mean((lasso.pred-y_night.test)^2)


# for daytime
x_day=model.matrix(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) + sec + BT_M15:sec 
                   + I(BT_M15 - BT_M14):sec + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref 
                   + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, day_time)[,-1]
y_day=as.vector(day_time$sst_insitu)

set.seed (1)
train=sample(1:nrow(x_day), nrow(x_day)/2)
test=(-train)
y_day.test=y_day[test]

out=glmnet(x_day,y_day,alpha=0)
ridge.mod=glmnet(x_day,y_day,alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=min(out$lambda) ,newx=x_day[test,])
predict(out_day,type="coefficients",s=min(out$lambda))[1:11,]
mean((ridge.pred-y_day.test)^2)

out_day=glmnet(x_day,y_day,alpha=1)
lasso.mod=glmnet(x_day[train ,],y_day[train],alpha=1,lambda=grid)
lasso.pred=predict(lasso.mod,s=min(out$lambda) ,newx=x_day[test,])
predict(out,type="coefficients",s=min(out$lambda))[1:11,]
mean((lasso.pred-y_day.test)^2)


#Question 5
regfit.bwd = regsubsets(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M12) + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) 
                        + sec + BT_M15:sec + I(BT_M15 - BT_M12):sec + I(BT_M15 - BT_M14):sec 
                        + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref + I(BT_M15 - BT_M12):sst_ref 
                        + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, 
                        data = night_time, method='backward', nvmax=13)
regfit.summary = summary(regfit.bwd)
regfit.summary$which
regfit.summary$adjr2
regfit.summary$bic


# The result tells us that only keeping b1,b10 and remove other variables is without significant loss of performance

coef(regfit.bwd, 2)

model6 = lm(sst_insitu ~ BT_M15 + BT_M15:sst_ref, data = night_time)
model6_r_squared = summary(model6)$adj.r.squared
model6_r_squared
sqrt(mean(model6$residuals^2))

AIC(model6)
BIC(model6)

regfit.bwd = regsubsets(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) + sec + BT_M15:sec 
                        + I(BT_M15 - BT_M14):sec + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref 
                        + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, 
                        data = day_time, method='backward', nvmax=10)
regfit.summary = summary(regfit.bwd)
regfit.summary$which
regfit.summary$adjr2
regfit.summary$bic
regfit.summary$cp


# The result tells us that only keeping b1,b10 and remove other variables is without significant loss of performance
coef(regfit.bwd, 2)

model7 = lm(sst_insitu ~ BT_M15 + BT_M15:sst_ref, data = night_time)
model7_r_squared = summary(model6)$adj.r.squared
model7_r_squared
sqrt(mean(model7$residuals^2))



regfit.summary$adjr2
regfit.summary$bic
regfit.summary$cp
aic = regfit.summary$bic + (2-log(4819216))*(9+1)

plot(regfit.bwd, scale = "Cp")
plot(regfit.bwd, scale = "adjr2")
plot(regfit.bwd, scale = "bic")
# Link other datas
# Can I get R^2 of origin?


#Question 6
regfit.full = regsubsets(sst_insitu ~ ., data = dataframe2, nvmax=8)
reg.summary = summary(regfit.full)

reg.summary$rsq
reg.summary$which

sqrt(mean(reg.summary$rss^2))

sst_insitu_2 <- ncvar_get(In2, "sst_insitu")
sst_ref_2 <- ncvar_get(In2, "sst_ref")
sza_2 <- ncvar_get(In2, 'sza')
vza_2 <- ncvar_get(In2, 'vza')

BT_M12_2 <- ncvar_get(In2, "BT_M12")
BT_M13_2 <- ncvar_get(In2, "BT_M13")
BT_M14_2 <- ncvar_get(In2, "BT_M14")
BT_M15_2 <- ncvar_get(In2, "BT_M15")
BT_M16_2 <- ncvar_get(In2, "BT_M16")


dataframe3 = data.frame(sst_insitu_2, BT_M12_2, BT_M13_2, BT_M14_2, BT_M15_2, BT_M16_2, sst_ref_2, sza_2, vza_2)
head(dataframe3)

model_varibale_1 = lm(sst_insitu_2~sst_ref_2, data=dataframe3)
rmse_varibale_1 = sqrt(mean(model_varibale_1$residuals^2))

model_varibale_2 = lm(sst_insitu_2~sst_ref_2 + sza_2, data=dataframe3)
rmse_varibale_2 = sqrt(mean(model_varibale_2$residuals^2))

model_varibale_3 = lm(sst_insitu_2~sst_ref_2 + sza_2 + BT_M15_2, data=dataframe3)
rmse_varibale_3 = sqrt(mean(model_varibale_3$residuals^2))

model_varibale_4 = lm(sst_insitu_2~sst_ref_2 + sza_2 + BT_M15_2 + BT_M16_2, data=dataframe3)
rmse_varibale_4 = sqrt(mean(model_varibale_4$residuals^2))

model_varibale_5 = lm(sst_insitu_2~sst_ref_2 + sza_2 + BT_M15_2 + BT_M16_2 + BT_M13_2, data=dataframe3)
rmse_varibale_5 = sqrt(mean(model_varibale_5$residuals^2))

model_varibale_6 = lm(sst_insitu_2~sst_ref_2 + sza_2 + BT_M15_2 + BT_M16_2 + BT_M13_2 + BT_M12_2, data=dataframe3)
rmse_varibale_6 = sqrt(mean(model_varibale_6$residuals^2))

model_varibale_7 = lm(sst_insitu_2~sst_ref_2 + sza_2 + BT_M15_2 + BT_M16_2 + BT_M13_2 + BT_M12_2 + BT_M14_2, data=dataframe3)
rmse_varibale_7 = sqrt(mean(model_varibale_7$residuals^2))

model_varibale_8 = lm(sst_insitu_2~sst_ref_2 + sza_2 + BT_M15_2 + BT_M16_2 + BT_M13_2 + BT_M12_2 + BT_M14_2 + sec, data=dataframe3)
rmse_varibale_8 = sqrt(mean(model_varibale_8$residuals^2))


plot(c(1,2,3,4), c(rmse_varibale_1, rmse_varibale_2, rmse_varibale_3, rmse_varibale_4), type='b', xlab = 'Size of best model', ylab = 'RMSE')
