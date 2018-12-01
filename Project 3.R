library(ncdf4)
library(leaps)
library(pracma)

In <- nc_open("./project 3/matchups_NPP_2018-01.nc")
sst_reg <- ncvar_get(In, "sst_reg")
sst_insitu <- ncvar_get(In, "sst_insitu")
sst_ref <- ncvar_get(In, "sst_ref")
sza <- ncvar_get(In, 'sza')
vza <- ncvar_get(In, 'vza')

#to see all the variable names in ncdf file:
names(In$var)


BT_M12 <- ncvar_get(In, "BT_M12")
BT_M13 <- ncvar_get(In, "BT_M13")
BT_M14 <- ncvar_get(In, "BT_M14")
BT_M15 <- ncvar_get(In, "BT_M15")
BT_M16 <- ncvar_get(In, "BT_M16")


dataframe1 = data.frame(sst_insitu, BT_M12, BT_M13, BT_M14, BT_M15, BT_M16)
head(dataframe1)

sec = sec(vza) - 1
dataframe2 = data.frame(sza,sec,sst_insitu, BT_M12, BT_M13, BT_M14, BT_M15, BT_M16, sst_ref)
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
model1 = lm(sst_insitu ~ BT_M15 + I(BT_M16 - BT_M15), data = dataframe1)

model2 = lm(sst_insitu ~ BT_M16 + I(BT_M15 - BT_M16), data = dataframe1)

# To evaluate the performance of model1, the adjusted R squared is 0.979362
summary1 = summary(model1)
model1_adj_r_squared = summary1$adj.r.squared
model1_adj_r_squared
summary1$coefficients

# To evaluate the performance of model2, the adjusted R squared is 0.979362 as well
summary2 = summary(model2)
model2_adj_r_squared = summary2$adj.r.squared
model2_adj_r_squared
summary2$coefficients

cor(sst_insitu, sst_reg) ^ 2
mean((sst_insitu - sst_reg) ^ 2)
mean(model1$residuals ^ 2)
mean(model2$residuals ^ 2)
mean(model3$residuals ^ 2)
mean(model4$residuals ^ 2)




# Question 2
model3 = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M16):sst_ref + I(BT_M15 - BT_M16):sec, data = dataframe1)
summary3 = summary(model3)
model3_adj_r_squared = summary3$adj.r.squared
model3_adj_r_squared
summary3$coefficients




# Question 3
model4 = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M16):sst_ref + I(BT_M15 - BT_M16):sec + sst_ref, data = dataframe1)
summary4 = summary(model4)
model4_adj_r_squared = summary4$adj.r.squared
model4_adj_r_squared
summary4$coefficients



# Question 4
T3.7 <- BT_M12   
T4 <- BT_M13 
T8.6 <- BT_M14 
T11 <- BT_M15 
T12 <- BT_M16 

model_night = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M12) + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) 
                 + sec + BT_M15:sec + I(BT_M15 - BT_M12):sec + I(BT_M15 - BT_M14):sec 
                 + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref + I(BT_M15 - BT_M12):sst_ref 
                 + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, data = night_time)
summary_night = summary(model_night)
model_night_r_squared = summary_night$adj.r.squared
model_night_r_squared
summary_night$coefficients



model_day = lm(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) + sec + BT_M15:sec 
               + I(BT_M15 - BT_M14):sec + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref 
               + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, data = day_time)
summary_day = summary(model_day)
model_day_r_squared = summary_day$adj.r.squared
model_day_r_squared
summary_day$coefficients




library(glmnet)

x=model.matrix(sst_insitu ~  BT_M15 + I(BT_M15 - BT_M12) + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) 
               + sec + BT_M15:sec + I(BT_M15 - BT_M12):sec + I(BT_M15 - BT_M14):sec 
               + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref + I(BT_M15 - BT_M12):sst_ref 
               + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, dataframe1)[,-1]
y=as.vector(dataframe1$sst_insitu)

set.seed (1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

#set.seed (1)
#cv.out=cv.glmnet(x[train ,],y[train],alpha=0, nfolds=3)
#bestlam=cv.out$lambda.min
#bestlam

grid=10^seq(10,-2,length=100)

out=glmnet(x,y,alpha=0)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
ridge.pred=predict(ridge.mod,s=min(out$lambda) ,newx=x[test,])
predict(out,type="coefficients",s=min(out$lambda))[1:14,]
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=1)
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
lasso.pred=predict(lasso.mod,s=min(out$lambda) ,newx=x[test,])
predict(out,type="coefficients",s=min(out$lambda))[1:14,]
mean((lasso.pred-y.test)^2)


#Question 5
regfit.bwd = regsubsets(sst_insitu ~ BT_M15 + I(BT_M15 - BT_M12) + I(BT_M15 - BT_M14) + I(BT_M15 - BT_M16) 
                        + sec + BT_M15:sec + I(BT_M15 - BT_M12):sec + I(BT_M15 - BT_M14):sec 
                        + I(BT_M15 - BT_M16):sec + BT_M15:sst_ref + I(BT_M15 - BT_M12):sst_ref 
                        + I(BT_M15 - BT_M14):sst_ref + I(BT_M15 - BT_M16):sst_ref, 
                        data = dataframe1, method='backward')
regfit.summary = summary(regfit.bwd)
regfit.summary$which
regfit.summary$adjr2

# The result tells us that only keeping b1,b10 and remove other variables is without significant loss of performance

coef(regfit.bwd, 2)

model6 = lm(sst_insitu ~ BT_M15 + BT_M15:sst_ref, data = dataframe1)
model6_r_squared = summary(model6)$adj.r.squared
model6_r_squared

pred = predict(model6, dataframe1[2:6], interval = "prediction")
mean((pred-y.test)^2)

