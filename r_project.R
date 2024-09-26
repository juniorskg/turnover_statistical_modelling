
#import the dataset
df<-read.csv("C:/Users/couli/Documents/R_project/HR_comma_sep.csv")
df

# check null values
sum(is.na(df))

# data exploration
library(psych)
describe(df)

#box plot for satisfaction level distribution
boxplot(df$satisfaction_level, main = "satisfaction level box plot", ylab = "satisfaction_level", col = "lightblue")
barplot(table(df$salary)[c("low","medium","high")],
        col = c("red","yellow","green"  ), 
        main = "salary distribution bar chart",
        xlab = "salary",
        ylab = "Frequency")
# bar char for employees turnover distribution

barplot(table(df$left)[c("1","0")],
        col = c("red","green"  ), 
        main = "employees turnover distribution bar chart",
        xlab = "employees turn over",
        ylab = "Frequency")


# scatter plot to check relationship  between turnover and both salary and satisfaction
library(ggplot2)
a<-df[, c("salary", "satisfaction_level","left")]
a$left<-as.character(a$left)
ggplot(data =a, aes(x =salary , y = satisfaction_level, color = left)) +
  geom_point(size = 2) +  # Set the size of points
  labs(title = "Scatter Plot of turnover over (salary and satisfaction level)",
       x = "salary",
       y = "satisfaction_level",
       color = "turnover") +  
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) 

#average satisfaction of employees by turnover 
left_sac<-mean(subset(df, left ==1)$satisfaction_level)

stay_sac<-mean(subset(df, left ==0)$satisfaction_level)
a<-data.frame(turnover= c("stay","leave"),
              average_satisfaction= c(stay_sac,left_sac))
print(a)

#convert salary to numerical
df$salary[df$salary == "low"] <- 1
df$salary[df$salary == "medium"] <- 2
df$salary[df$salary == "high"] <- 3
df$salary<- as.numeric(df$salary)
# encode department using on hotencoder
df$Department<- model.matrix(~ Department - 1, data = df)
print(df)

#feature engeeener for chi square test

low_leave=length(subset(df, left == 1 & satisfaction_level < 0.5)$salary)
low_noleave=length(subset(df, left == 0 & satisfaction_level < 0.5)$salary)
high_leave=length(subset(df, left == 1 & satisfaction_level > 0.5)$salary)
high_noleave=length(subset(df, left == 0 & satisfaction_level > 0.5)$salary)
b<-matrix(c(low_leave,low_noleave,high_leave,high_noleave),nrow = 2,byrow = TRUE)
rownames(b) <- c("low satisfaction level","high satisfaction level")
colnames(b) <- c("leave", "Not leave")
b


low_leave=length(subset(df, left == 1 & salary == 1)$satisfaction_level)
low_noleave=length(subset(df, left == 0 & salary == 1)$satisfaction_level)
mid_leave=length(subset(df, left == 1 & salary == 2)$satisfaction_level)
mid_noleave=length(subset(df, left == 0 & salary == 2)$satisfaction_level)
high_leave=length(subset(df, left == 1 & salary == 3)$satisfaction_level)
high_noleave=length(subset(df, left == 0 & salary == 3)$satisfaction_level)
a<-matrix(c(low_leave,low_noleave,mid_leave,mid_noleave,high_leave,high_noleave),nrow = 3,byrow = TRUE)
rownames(a) <- c("low salary", "medium salary","high salary")
colnames(a) <- c("leave", "Not leave")
a

chisq.test(b)
chisq.test(a)

# split data into train and test
library(caTools)
split=sample.split(df$left,SplitRatio=0.8)
train=subset(df,split==TRUE)
test= subset(df,split==FALSE)

#apply logistic regression
logmodel<-glm(left~.,data = train,family = "binomial")
summary(logmodel)
#predict test data
y_prob=predict(logmodel,newdata=x_test , type="response")
y_predict <- ifelse(y_prob>0.5, 1, 0)
#calculate evalution metric make evaluation
confu=table(y_predict,test$left)
accuracy<-(confu[1,1]+confu[2,2])/(confu[1,1]+confu[2,2]+confu[1,2]+confu[2,1])
precision <- confu[1,1] / (confu[1,1] + confu[1,2])
recall<-confu[1,1]/(confu[1,1]+confu[2,1])
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("accuracy =",accuracy))
print(paste("precision  = ",precision ))
print(paste("Recall = ",recall))
print(paste("f1_score = ",f1_score))
confu

  


