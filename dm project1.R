winequality.red <- read.csv("D:/sem 2/wine quality/winequality-red.csv")
View(winequality.red)
attach(winequality.red)
head(winequality.red, n=10)
str(winequality.red)
summary(winequality.red)
cor(winequality.red)
dim(winequality.red)


library(corrplot)
library(RColorBrewer)
M <-cor(winequality.red)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))




winequality.red$quality<-as.factor(winequality.red$quality)


library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

library(dplyr)
df <- Data%>%
  group_by(quality) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = quality, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

ggplot(Data,aes(x=quality,y=fixed.acidity))+
  geom_step(direction = "hv")

ggplot(Data,aes(x=quality,y=volatile.acidity))+
  geom_line()

ggplot(Data,aes(x=quality,y=citric.acid))+
  geom_line()

ggplot(Data,aes(x=quality,y=residual.sugar))+
  geom_line()

ggplot(Data,aes(x=quality,y=chlorides))+
  geom_line()

ggplot(Data,aes(x=quality,y=free.sulfur.dioxide))+
  geom_line()

ggplot(Data,aes(x=quality,y=total.sulfur.dioxide))+
  geom_line()

ggplot(Data,aes(x=quality,y=sulphates))+
  geom_line()
ggplot(winequality.red,aes(x=quality,y=alcohol))+
  geom_step(direction = "hv")


ggplot(Data,aes(x=quality,y=alcohol))+
  geom_line() 

Data=winequality.red
Data$quality[Data$quality == 3 |Data$quality == 4 | Data$quality == 5] = 0
Data$quality[Data$quality == 6 |Data$quality == 7 | Data$quality == 8 ] = 1
Data

ggplot(df, aes(x = quality, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

library(caTools)
set.seed(123)
split = sample.split(Data$quality, SplitRatio = 0.7)
training_set = subset(Data, split == TRUE)
test_set = subset(Data, split == FALSE)

library(randomForest)
set.seed(123)
training_set$quality <- as.factor(training_set$quality)
classifier = randomForest(x = training_set[1:11],
                          y = training_set$quality,
                          ntree = 500)
y_pred = predict(classifier, newdata = test_set[1:11])
table(test_set$quality, y_pred)

table(test_set$quality, y_pred) %>% 
  prop.table() %>% round(digits = 3)
library(caret)
confusionMatrix( y_pred, test_set$quality,
                positive = "pos")

head(Data, n=5)

