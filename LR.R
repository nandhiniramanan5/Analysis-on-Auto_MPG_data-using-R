#to fit a model with a quantitative dependent variable (automobile miles per gallon or MPG) and multiple independent variables (attributes of the automobile and its engine)

Auto = read.csv("C:/Users/Nandini/Documents/Textbooks/Big data/Auto_MPG_data_2.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
dim(Auto)
str(Auto)
summary(Auto)
sapply(Auto[, 1:7], range)
sapply(Auto[, 1:7], mean)
sapply(Auto[, 1:7], sd)
pairs(Auto)
plot(Auto$mpg, Auto$weight)
# Heavier weight correlates with lower mpg.
plot(Auto$mpg, Auto$cylinders)
# More cylinders, less mpg.
plot(Auto$mpg, Auto$year)
# Cars become more efficient over time.

#Weight, displacement and horsepower seem to have an inverse effect with mpg
#correlation matrix
cor(Auto[,1:7])


#throw all the predicates into LM
lm1<-lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin,data = Auto)
plot(lm1)
plot(predict(lm1), rstudent(lm1))
summary(lm1)

#seems like some interaction between cylinders, displacement and weight, which isnt very useful 
lm2 <- lm(mpg~ horsepower + acceleration + origin+ cylinders*displacement*weight, data = Auto)
summary(lm2)

#do step evaluation to obtain the best model
base<- lm(mpg ~ 1 ,data = Auto)
summary(base)
base.forward <- step(base, scope =  ~cylinders + displacement + horsepower + weight + acceleration + year + origin, direction = "forward" ) 

# we take model with low AIC , Step:  AIC=951.24, mpg ~ weight + year + origin
model=lm(mpg ~ weight+year+origin, data=Auto)
summary(model)
confint(model)
plot(model)
plot(predict(model), rstudent(model))

#  transformations; The residuals plot has less of a curve than the first regression with all the terms.

lm3 <- lm(mpg~log(weight)+sqrt(year)+acceleration+I(origin^2), data = Auto)
summary(lm3)
plot(lm3)
plot(predict(lm3), rstudent(lm3))

