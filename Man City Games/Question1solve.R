#Project begins
library(readr)
sampled_data <- read_csv("C:/Users/gauur/OneDrive/Desktop/CLASSES/STA 305/After Exam1/Project/sampled_data.csv")
#the dataset we will work with is the sampled_data dataset
#Question1:Is the outcome of the game (W/L/D) related to the playing formation chosen?  
#Outcome of the games is WIN.LOSE.DRAW COLUMN
#Formation of the games is in Formation Column

# We have two columns and we want to see if they are related. Thus we want to see if there is a relationship between outcome of the game
#and the formation that they played in. Hence we do,Test for Independence.
RQ1.table_old <- xtabs(formula = ~Formation + WIN.LOSE.DRAW, data = sampled_data)
RQ1.table_old
#drop the values with max 2 entry:(insufficient data)
formations_to_drop <- c("3.2.4.2", "4.1.3.2", "4.4.1.1" , "3.4.2.1", "3.5.2" , "4.4.2", "4.1.4.1")
new_data <- subset(sampled_data, !Formation %in% formations_to_drop)
new_data
new_data$WIN.LOSE.DRAW[new_data$WIN.LOSE.DRAW == "Draw"] <- "Not Win"
new_data$WIN.LOSE.DRAW[new_data$WIN.LOSE.DRAW == "Lose"] <- "Not Win"
new_data
man_city_wins <- subset(new_data, WIN.LOSE.DRAW == "Win")
man_city_wins
man_city_lost <- subset(new_data, WIN.LOSE.DRAW == "Not Win")
man_city_lost

prop_test_result <- prop.test(141, 182, conf.level = 0.95)
prop_test_result

prop_test_result2 <- prop.test(41, 182, conf.level = 0.95)
prop_test_result2
RQ1.table <- xtabs(formula = ~Formation + WIN.LOSE.DRAW, data = new_data)
RQ1.table
#we see we have two columns with three options.
transRQ1 <- t(RQ1.table)
pcnts6 <- scale(transRQ1, FALSE, colSums(transRQ1)) * 100
barplot(pcnts6, beside = TRUE, col = c("red", "green"), ylim = c(0, 100), ylab = "percent", main = "Outcome by Formation")
legend(x = "topright", y = 100, legend = c("Not Win", "Win"), fill = c("red", "green"))


chisq.test(RQ1.table,simulate.p.value = TRUE)
chisq.test(RQ1.table)
#test stat == 1.4122
#we have the test
#ind.test <- chisq.test(RQ1.table) #may or may not use this
#ind.test$expected #may or may not use this
#ind.test$residuals #may or may not use this
# WE CONCLUDE THAT FORAMTION ALONE IS NOT RELATED WITH THE WINNING OUTCOME . SO WE LOOK FURTHER.


#RQ2: checking if goal scored is related.
#the dataset to use initially is new_Data
#taking out essential columns:
#column_names <- names(new_data)
#print(column_names)
selected_columns <- c("Goals.SCORED", "Shots", "Shot.on.Target","Possession","Passes","Fouls","Yellow.Cards","Corners")
question2_data <- new_data[selected_columns]
question2_data <- as.data.frame(lapply(question2_data, as.numeric))
question2_data
cor(question2_data)
pairs(question2_data, pch=1, col="steelblue")

par(mfrow= c(2,2))
plot(question2_data$Shots, question2_data$Goals.SCORED, xlab = "Shots", ylab ="Outcome",
     main = "Outcome based on Shots", col="darkgreen")
plot(question2_data$Shot.on.Target, question2_data$Goals.SCORED, xlab = "Shots on Target", ylab ="Outcome",
     main = "Outcome based on Shots on target", col="purple")
plot(question2_data$Possession, question2_data$Goals.SCORED, xlab = "Possession", ylab ="Outcome",
     main = "Outcome based on Possession", col="darkorange")
plot(question2_data$Yellow.Cards, question2_data$Goals.SCORED, xlab = "Yellow Cards", ylab ="Outcome",
     main = "Outcome based on Yellow.Cards", col="purple")

#plot(question2_data$Passes, question2_data$Goals.SCORED, xlab = "Passes", ylab ="Outcome",
     #main = "Outcome based on Passes", col="blue")

plot(question2_data$Fouls, question2_data$Goals.SCORED, xlab = "Fouls", ylab ="Outcome",
     main = "Outcome based on Fouls", col="brown")
plot(question2_data$Corners, question2_data$Goals.SCORED, xlab = "Corners", ylab ="Outcome",
     main = "Outcome based on Corners", col="darkorange")



question2_datamodel<-lm(Goals.SCORED~ Shots + Shot.on.Target + Possession + Passes +Fouls +Yellow.Cards+Corners, data=question2_data)
summary(question2_datamodel) #adf.r^2 = 0.4554


#Check Normality
qqnorm(question2_datamodel$residuals)
hist(question2_datamodel$residuals)
shapiro.test(question2_datamodel$residuals)

#Check Constant Variance/Std.Dev.
plot(question2_datamodel$fitted.values, question2_datamodel$residuals)
abline(h=0, lty = 2)


#removing fouls:
question2_datamodel2<-lm(Goals.SCORED~ Shots + Shot.on.Target + Possession + Passes +Yellow.Cards+Corners, data=question2_data)
summary(question2_datamodel2) #adf.r^2 = 0.4585

#removing possession:
question2_datamodel3<-lm(Goals.SCORED~ Shots + Shot.on.Target+ Passes +Yellow.Cards+Corners, data=question2_data)
summary(question2_datamodel3) #adf.r^2 = 0.4609
#removing passes:
question2_datamodel4<-lm(Goals.SCORED~ Shots + Shot.on.Target +Yellow.Cards+Corners, data=question2_data)
summary(question2_datamodel4) #adf.r^2 = 0.4615
#removing shots:
question2_datamodel5<-lm(Goals.SCORED~Shot.on.Target +Yellow.Cards+Corners, data=question2_data)
summary(question2_datamodel5) #adf.r^2 = 0.461

# 7 shots on target, 7 corners and 1 yellow cards
new <- data.frame(Shot.on.Target=c(7), Yellow.Cards=c(1), Corners=c(7))
predict(question2_datamodel5, newdata=new, level = 0.95, interval = "predict")


par(mfrow= c(1,2))
plot(question2_data$Shot.on.Target, question2_data$Goals.SCORED, xlab = "Possession", ylab ="Outcome",
     main = "Outcome based on Shot.on.Target", col="darkorange")

plot(question2_data$Shots, question2_data$Goals.SCORED, 
     main = "Scatter Plot of Goals Scored vs Number of Shots",
     xlab = "Number of Shots",
     ylab = "Goals Scored",
     col = "blue",  # Color of the points
     pch = 16       # Type of point (you can change it)
)
question2_data_forplott  <- question2_data[order(question2_data$Shots), ]
barplot(height = question2_data_forplott $Goals.SCORED,
        names.arg = question2_data_forplott $Shots,
        col = "skyblue",
        main = "Bar Plot",
        xlab = "Number of Shots",
        ylab = "Goals Scored"
)



