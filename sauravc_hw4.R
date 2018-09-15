#Question 01

Wine = read.csv(file.choose())
wine1 <- Wine[1:160,]
wine2 <- Wine[161:178,]

wine1 = data.frame(wine1)
wine1$Class = as.factor(wine1$Class)

wine2 = data.frame(wine2)
wine2$Class = as.factor(wine2$Class)

NB=naiveBayes(Class~., data = wine1)
summary(NB)

NB_pred = predict(NB, newdata = wine2)
table(NB_pred, true = wine2$Class)
mean(NB_pred == wine2$Class)



#Question 02

OptDigit = read.csv(file.choose())

head(OptDigit)
j=1
out <- NULL
temp <- NULL

#taking out the columns with 0 values
for(i in 1:ncol(OptDigit))
{
  temp <- unique(OptDigit[,i])
  if(length(temp)==1 & (temp)==0)
  {
    out[j] <- i;
    j = j+1
  }
}


temp2 <- OptDigit[,-out]

#Building training and testing data
smp_size = floor(0.80*nrow(temp2))
set.seed(123)
train_temp = sample(seq_len(nrow(temp2)), size = smp_size)
train = temp2[train_temp, ]
test = temp2[-train_temp, ]
nrow(train)
nrow(test)

train = data.frame(train)
train$OptDigit = as.factor(train$OptDigit)

test = data.frame(test)
test$OptDigit = as.factor(test$OptDigit)

NB=naiveBayes(OptDigit~., data = train)
summary(NB)

NB_pred = predict(NB, newdata = test)
table(NB_pred, true = test$OptDigit)
mean(NB_pred == test$OptDigit)NB_pred = predict(NB, newdata = test)
table(NB_pred, true = test$OptDigit)
mean(NB_pred == test$OptDigit)

#Question 3:

olympic = read.csv(file.choose())
head(olympic)
pc = princomp(~., data = olympic, cor = TRUE)
summary(pc)
pc$loadings
pc$scores
