
# Lab #5 
#Hugo Pinto 



# Running a regresion where i like to get a result for  African American people, what are the wages  associeted with business 

#independent variable = x = AFAm, bach deg    
#dependent variable = y = INCWAGE 
attach(acs2017_ny)
use_varb <- (AGE >= 20) & (AGE <= 65) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (AfAm == 1) & (DEGFIELD == "Business") 

dat_use <- subset(acs2017_ny,use_varb)
detach()
attach(dat_use)

# now we apply diffent polinomila funtions to get diffent results 

lm1 <- lm((INCWAGE ~ AGE + I(AGE^2)+I(AGE^3)+I(AGE^4)+ + AfAm + female + educ_college))


Reg <- lm(INCWAGE ~ AGE + I(AGE^2) + educ_college) 
summary(Reg) 

Reg <- lm(INCWAGE ~ AGE + I(AGE^3) + educ_college) 
summary(Reg) 

Reg <- lm(INCWAGE ~ AGE + I(AGE^4) + educ_college) 
summary(Reg) 

Reg <- lm(INCWAGE ~ AGE + I(AGE^5) + educ_college) 
summary(Reg) 




# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) #  diffent ways tu set the the data 
graph_obs <- (runif(NNobs) < 0.9) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  


plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)


 # now I am changing the line  to  fit  my your regression
to_be_predicted2 <- data.frame(AGE = 20:65, educ_college = 0)
to_be_predicted2$yhat <- predict(Reg, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
to_be_predicted2 <- data.frame(AGE = 20:65, educ_college = 1)


to_be_predicted2$yhat <- predict(Reg, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)




