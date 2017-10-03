library(zoo)
bmw.raw = read.csv("C:/Users/Kevin/Dropbox/Uni Homework/4B/ECON 321/Assignments/a3/2014 BMW 3 Series Sedan Used.csv", header = TRUE)

bmw = data.frame(data=bmw.raw)

for(t in unique(bmw.raw$MODEL)){
  bmw[paste("model_",t,sep="")] <- ifelse(bmw.raw$MODEL==t,1,0)
}

for(t in unique(bmw.raw$LINE)){
  bmw[paste("line_",t,sep="")] <- ifelse(bmw.raw$LINE==t,1,0)
}

for(t in unique(bmw.raw$IN_COLOUR)){
  bmw[paste("in_colour_",t,sep="")] <- ifelse(bmw.raw$IN_COLOUR==t,1,0)
}

for(t in unique(bmw.raw$EX_COLOUR)){
  bmw[paste("ex_colour_",t,sep="")] <- ifelse(bmw.raw$EX_COLOUR==t,1,0)
}
head(bmw)


## a) Price ~ KM + The SINK

lm.bmw.sink = lm(data.PRICE ~ data.KM
                 + data.XDRIVE
                 + model_328I + model_335I 
                 + line_Sport + line_Modern + line_Luxury
                 + in_colour_Red + in_colour_Beige
                 + ex_colour_Black + ex_colour_Grey + ex_colour_Red + ex_colour_Silver
                 ,data=bmw)                                  
summary(lm.bmw.sink)


par(mfrow=c(1,1))
plot(fitted(lm.bmw.sink),residuals(lm.bmw.sink), main="Fitted Residual Plot", ylab ="Residuals", xlab= "Fitted Values")

qqnorm(residuals(lm.bmw.sink), main="Residual Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Residual Quantiles")

lm.ur = lm.bmw.sink

#******************
#*Homoskedasticity*
#******************
#Check for heteroskedasticity with breusche pagan test

ei = residuals(lm.bmw.sink)
bpt = lm(ei ~ data.KM
         + data.XDRIVE
         + model_328I + model_335I 
         + line_Sport + line_Modern + line_Luxury
         + in_colour_Red + in_colour_Beige
         + ex_colour_Black + ex_colour_Grey + ex_colour_Red + ex_colour_Silver
         ,data=bmw)
#since the fstat for breushe pagan test (testing all coeficients is 0) is:
summary(bpt)$fstatistic[1]
#and the F-rejection to reject homoskedasticity is 
qf(0.95,summary(bpt)$fstatistic[2], summary(bpt)$fstatistic[3])
#Not enough ground to reject Null. Therefore the data is homoskedastic


## ************************
## *Remove Interior Colour*
## ************************
## b) Price ~ KM + The SINK - in_colour

lm.bmw2 = lm(data.PRICE ~ data.KM
                 + data.XDRIVE
                 + model_328I + model_335I 
                 + line_Sport + line_Modern + line_Luxury
                 + ex_colour_Black + ex_colour_Grey + ex_colour_Red + ex_colour_Silver
                 ,data=bmw)                                  
summary(lm.bmw2)
lm.r = lm.bmw2

#Using F-Test to check if all interior colour variables are 0, the F stat is:
((summary(lm.ur)$r.squared - summary(lm.r)$r.squared) / (summary(lm.ur)$df[1] - summary(lm.r)$df[1]))/(
  (1-summary(lm.ur)$r.squared)/(summary(lm.ur)$df[2]))
#F rejection for the null hypothesis is:
qf(0.95,(summary(lm.ur)$df[1] - summary(lm.r)$df[1]), summary(lm.ur)$df[2])
#Since the F-stat did not exceed the rejection, we cannot reject that interior colour has significant 
#impact on used BMW 2014 3 series car prices when exterior xDrive, model, line and exterior colour are
#already taken to into account.

## **********************
## *Test Exterior Colour*
##***********************
lm.ur = lm.bmw2

lm.bmw3 = lm(data.PRICE ~ data.KM
             + data.XDRIVE
             + model_328I + model_335I 
             + line_Sport + line_Modern + line_Luxury             
             ,data=bmw)                                  
summary(lm.bmw3)
lm.r = lm.bmw3

#Using F-Test to check if all exterior colour variables are 0, the F stat is:
((summary(lm.ur)$r.squared - summary(lm.r)$r.squared) / (summary(lm.ur)$df[1] - summary(lm.r)$df[1]))/(
  (1-summary(lm.ur)$r.squared)/(summary(lm.ur)$df[2]))
#F rejection for the null hypothesis is at 95% confidence:
qf(0.95,(summary(lm.ur)$df[1] - summary(lm.r)$df[1]), summary(lm.ur)$df[2])
#Since the F-stat exceeded. We can reject the null that exterior colours has no impact to car prices with
#5% level of significance



## *****************
## *Test Car Model *
##******************
lm.ur = lm.bmw2
lm.bmw5 = lm(data.PRICE ~ data.KM
             + data.XDRIVE            
             + line_Sport + line_Modern + line_Luxury             
             + ex_colour_Black + ex_colour_Grey + ex_colour_Red + ex_colour_Silver
             ,data=bmw)                                  
summary(lm.bmw5)
lm.r = lm.bmw5

#Using F-Test to check if all interior colour variables are 0, the F stat is:
((summary(lm.ur)$r.squared - summary(lm.r)$r.squared) / (summary(lm.ur)$df[1] - summary(lm.r)$df[1]))/(
  (1-summary(lm.ur)$r.squared)/(summary(lm.ur)$df[2]))
#F rejection for the null hypothesis is:
qf(0.95,(summary(lm.ur)$df[1] - summary(lm.r)$df[1]), summary(lm.ur)$df[2])
#Since the F-stat number greatly surpasses the F-rejection.We reject that car model has no significant
#impact on the pricing of the car.


## *********************
## *Test Car Body Line *
##**********************
lm.ur = lm.bmw2
lm.bmw4 = lm(data.PRICE ~ data.KM
             + data.XDRIVE
             + model_328I + model_335I              
             + ex_colour_Black + ex_colour_Grey + ex_colour_Red + ex_colour_Silver
             ,data=bmw)                                  
summary(lm.bmw4)
lm.r = lm.bmw4

#Using F-Test to check if all interior colour variables are 0, the F stat is:
((summary(lm.ur)$r.squared - summary(lm.r)$r.squared) / (summary(lm.ur)$df[1] - summary(lm.r)$df[1]))/(
  (1-summary(lm.ur)$r.squared)/(summary(lm.ur)$df[2]))
#F rejection for the null hypothesis is:
qf(0.95,(summary(lm.ur)$df[1] - summary(lm.r)$df[1]), summary(lm.ur)$df[2])
#Since the F-stat did not exceed the F-Rejection, the line of the car has no affect on the pricing 
#the used car. 
