# Importing data set
data <- read.csv("C:/Users/91736/Downloads/medical insurance.csv")

head(data , n = 5)

str(data)
summary(data)

# checking for missing values
sum(is.na(data))

# Encoding categorical variables
new_data <- data
new_data$sex <- factor(new_data$sex, exclude = NULL)
new_data$smoker <- factor(new_data$smoker, exclude = NULL)
new_data$region <- factor(new_data$region, exclude = NULL)
new_data <- model.matrix(~.-1, data = new_data[, c("sex", "smoker","region")],
                                                  contrasts.arg = list(
                                                       sex = contrasts(new_data$sex, contrasts = FALSE),
                                                       smoker = contrasts(new_data$smoker, contrasts = FALSE),
                                                       region = contrasts(new_data$region, contrasts = FALSE)
                                                    ))
head(new_data , n=5 )

data2 <- subset(new_data, select = -c(1, 3,5))
head(data2 , n=5 )

age = data$age
bmi = data$bmi
children = data$children
charges = data$charges
data3 = cbind(data2,age)
data4 = cbind(data3,bmi)
data5 = cbind(data4,children)
data6 = cbind(data5,charges)

df = data6
head(df , n =5)


# Splitting the features and target

x = subset(df , select = -c(9))
head(x , n =5)
y = data$charges
y


#Splitting the data into Training and Testing data
PackageUrl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_0.9.1.tar.gz" 
install.packages(PackageUrl, repos=NULL, type="source")

install.packages("caTools")

library(caTools)

sample = sample.split(y, SplitRatio = .80)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)
View(train)
View(test)

x_train = subset(train , select = -c(9))
y_train =  subset(train , select = c(9))
x_test = subset(test , select = -c(9))
y_test = subset(test , select = c(9))

#Fitting the linear regression model
x_train1 = as.data.frame(train)
x_test1 = as.data.frame(test)

model = lm(formula = charges ~ age+bmi+children+sexmale+
                 smokeryes+regionnorthwest+regionsoutheast+regionsouthwest, data = x_train1)
summary(model) 

#prediction on testing set
y_pred_train = predict(model, newdata = x_train1)
y_pred_test = predict(model, newdata = x_test1)
head(y_pred,n=5)

#Model Assumptions

#Linearity

install.packages("ggplot2")
library(ggplot2)
ggplot(x_train1, aes(x=age, y=charges)) + geom_point()
ggplot(x_train1, aes(x=bmi, y=charges)) + geom_point()
ggplot(x_train1, aes(x=children, y=charges)) + geom_point()
ggplot(x_train1, aes(x=sexmale, y=charges)) + geom_point()
ggplot(x_train1, aes(x=smokeryes, y=charges)) + geom_point()
ggplot(x_train1, aes(x=regionnorthwest, y=charges)) + geom_point()
ggplot(x_train1, aes(x=regionsoutheast, y=charges)) + geom_point()
ggplot(x_train1, aes(x=regionsouthwest, y=charges)) + geom_point()

# No Multicollinearity

install.packages("caret")
library(caret)

install.packages("car")
library(car)

vif(model)


#Normality of Residuals

residuals = y_train - y_pred_train
d <- density(residuals)
plot(d)


# Homoscedasticity

par(mfrow=c(2,2))

install.packages("olsrr")
library(olsrr)
ols_plot_resid_fit(model)

# No Autocorrelation

durbinWatsonTest(model)

##Forward Selection

FWDfit.p <- ols_step_forward_p(model,penter= .05)

FWDfit.p

# Final Model

final_model = lm(formula = charges ~ age+children+
             smokeryes+bmi, data = x_train1)
summary(final_model) 






