#Drugs Project

#------------------------------------1989---------------------------------
tmp <- file.choose()
poll1989 <- read.csv(file = tmp, header = TRUE, stringsAsFactors = FALSE)
str(poll1989)
head(poll1989)
poll1989["count"] <- 1
poll1989$count


colors.gender <- c("#009ACD","#FF1493")

#1 males, 2 females
sex <- table(poll1989$sex)
#age
age <- table(poll1989$age)
barplot(age, col = "#7FFF00")
barplot(sex, col = colors.gender, ylab = "count", xlab = "Gender")
#piecharts 
lbls <- c("Males","Females")
pie(sex, labels = lbls, main="1989", col = colors.gender)

#sex and perception of Consumption of drugs in Mexico against other countries
a1 <- tapply(poll1989$count, list(poll1989$sex,poll1989$q23),sum)
barplot(a1, beside = T, col = colors.gender, main = "Do you consider that drug use is higher in other countries than in Mexico?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Drugs are used more for other countries than Mexico, 2=The use of Drugs is common in Mexico 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Drugs consumption in Mexico is growing or not? Opinion by sex
a2 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q23a), sum)
barplot(a2, beside = T, col = colors.gender, main = "Do you consider that drug use is increasing in Mexico?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Yes, drugs use is growing in Mexico, 2=No, it is not growing 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Amount of drugs passing through Mexico
a3 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q24), sum)
barplot(a3, beside = T, col = colors.gender, main = "Do you consider that Amount of drugs passing through Mexico is increasing or not increasing?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=it is increasing, 2=it is decreasing, 3= it remains almost the same 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Drug control should be a priority in Mexico?
a4 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q25), sum)
barplot(a4, beside = T, col = colors.gender, main = "Drug control should be a priority in Mexico?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=high priority, 2=other problems are more important, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Mexico and international cooperation to fight against drug traffic
#Many people say that druf traffic should be controlled by the producers countries 
a5 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q26), sum)
barplot(a5, beside = T, col = colors.gender, main = "Drug traffic should be fighted by the countries that produce drugs?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=producers countries, 2=consumers countries, 3 = both, consumers and producers, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Are you in favor of the extradition of drug traffickers?
#SpiderPlot
a6 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q28), sum)
a6 <- as.data.frame(a6)
colnames(a6)=c("Very in favor" , "In favor" , "Against" , "Very Against", "Don´t know")
a6=rbind(rep(405,15) , rep(0,15) , a6)
radarchart(a6)

colors_border = c("#009ACD","#FF1493")
colors_in = c("#009ACD","#FF1493")
radarchart( a6  , axistype=1 , 
            
            #custom polygon
            pcol="#009ACD" , pfcol="#FF1493" , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = c("Female" , "Male"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


#barplot
a6 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q28), sum)
barplot(a6, beside = T, col = colors.gender, main = "Are you in favor of the extradition of drug traffickers?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1 = Very in favor, 2 = In favor, 3 = Against, 4 = very against,  9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Are you in favor of the extradition of drug traffickers? And if they are mexicans, you will have the same opinion?
a7 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q28a), sum)
barplot(a7, beside = T, col = colors.gender, main = "And if they are mexicans, you will have the same opinion?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1 = same opinion, 2 = I will against extradition, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#What is your opinion about the effort that make the Mexican Government against the drug traffic?
#Do you think is working more than other countries?
a8 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q29), sum)
barplot(a8, beside = T, col = colors.gender, main = "Do you think is working more than other countries?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=It is working more than other countries, 2=It is working less than other countries, 3= I don´t have enough info to make an opinion, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Do you think Mexico should work closely with other countries to fight against the drug traffic?
a9 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q30), sum)
barplot(a9, beside = T, col = colors.gender, main = "Do you think is working more than other countries?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Yes, it should , 2=No, it should not, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Do you consider that there are countries with which Mexico should not work to combat drug trafficking?
a10 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q31), sum)
barplot(a10, beside = T, col = colors.gender, main = "Do you think is working more than other countries?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Yes, it should , 2=No, it should not, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Cuanta intromision extranjera existe? Mucha, algo, no mucha, no sabe...
a11 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q31a), sum)
barplot(a11, beside = T, col = colors.gender, main = "How many international intromission are?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Many intromission , 2=Some intromission, 3=Not intromission ,9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#Would you that foreign intromission is a seriuos problem, a little serious problem, not a really serious problem, it is not a problem? 
#radarplot
a12 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q31b), sum)
a12 <- as.data.frame(a12)
colnames(a12)=c("1=Serious problem" , "2=A little serious problem", "3=Not very serious", "4= not a problem" ,"9 = Don´t know")
a12=rbind(rep(188,4) , rep(0,4) , a12)
radarchart(a12)

colors_border = c("#009ACD","#FF1493")
colors_in = c("#009ACD","#FF1493")
radarchart( a12  , axistype=1 , 
            
            #custom polygon
            pcol="#009ACD" , pfcol="#FF1493" , plwd=2 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = c("Female" , "Male"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


#barplot
a12 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q31b), sum)
barplot(a12, beside = T, col = colors.gender, main = "Would you that foreign intromission is a ...?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Serious problem , 2=A little serious problem, 3=Not very serious, 4= not a problem ,9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#What is the main challenge to resolve the drugs problems? First Place
a13 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q32_1), sum)
barplot(a13, beside = T, col = colors.gender, main = "What is the main challenge to resolve the drugs problems?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Lack of efficiency to reduce consumption, 2 = Lack of efficiency to reduce production, 3=Power of drug traffickers, 4= Lack of cooperation, 5 = None of them, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#What is the main challenge to resolve the drugs problems? Second place 
a14 <- tapply(poll1989$count, list(poll1989$sex, poll1989$q32_2), sum)
barplot(a14, beside = T, col = colors.gender, main = "Would you that foreign intromission is a ...?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Lack of efficiency to reduce consumption, 2 = Lack of efficiency to reduce production, 3=Power of drug traffickers, 4= Lack of cooperation, 5 = None of them, 9 = Don´t know", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#------------------------------------2000-------------------------------
tmp <- file.choose()
poll2000 <- read.csv(file = tmp, header = TRUE, stringsAsFactors = FALSE)
str(poll2000)
head(poll2000)
poll2000["count"] <- 1
poll2000$count

poll2000$p22i <- cut(poll2000$p22,
                     breaks = c(-Inf, 3, 6, 9, 12, Inf),
                     labels = c(".49-3", "3-6","6-9","9-12","12-15"),
                     right = FALSE)
#p22= How much have you heard or read about the assistance the US has given Mexico to combat narcotrafficking here?
b1 <- table(poll2000$p22i)
barplot(b1, beside = T, col = "#FF4500", main = "How much have you heard or read about the assistance the US has given Mexico to combat narcotrafficking here?", bty="l", xlab = "Answers", ylab = "Frequency")
mtext(text = "1=Great Deal, 2=Fair amount, 3=Not very much, 4=None at all, 8=Don´t know, 9= No answer", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#p23= OVerall, how helpful has US assistance to Mexico been in combating narcotrafficking?
b2 <- table(poll2000$p23)
barplot(b2, beside = T, col = "#FF4500", main = "OVerall, how helpful has US assistance to Mexico been in combating narcotrafficking?", bty="l", xlab = "Answers", ylab = "Frequency")
mtext(text = "1=Very helpful, 2=Somewhat helpful, 3=Not very helpful, 4=Not helpful at all, 7=Not asked, 8=Don´t know, 9= No answer", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#p24= In your opinion, should Mexico cooperate more, less, or about the same as it is now with the US narcotrafficking?
b3 <- table(poll2000$p24)
barplot(b3, beside = T, col = "#FF4500", main = "In your opinion, should Mexico cooperate more, less, or about the same as it is now with the US narcotrafficking?", bty="l", xlab = "Answers", ylab = "Frequency")
mtext(text = "1=Cooperate more, 2=Cooperate about the same as now, 3=Cooperate less, 8=Don´t know, 9= No answer", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#p25=How would you rate the US governments efforts to deal with their drugs problems?
b4 <- table(poll2000$p25)
barplot(b4, beside = T, col = "#FF4500", main = "How would you rate the US governments efforts to deal with their drugs problems?", bty="l", xlab = "Answers", ylab = "Frequency")
mtext(text = "1=Very good, 2=Fairly good, 3=Fairly poor, 4=Very poor, 8=Don´t know, 9= No answer", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#p26=Is illegal drug user in the U.S.?
b5 <- table(poll2000$p26)
barplot(b5, beside = T, col = "#FF4500", main = "Is illegal drug use in the U.S.?", bty="l", xlab = "Answers", ylab = "Frequency")
mtext(text = "1=Increasing, 2=Remaining about the same, 3=Decreasing, 4=Haven´t heard enough, 8=Don´t know, 9= No answer", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#p27=Thinking about the posibility of legalizing drugs such as cocaine and heroin, would you be?
b6 <- table(poll2000$p27_a)
barplot(b6, beside = T, col = "#FF4500", main = "Thinking about the posibility of legalizing drugs such as cocaine and heroin, would you be?", bty="l", xlab = "Answers", ylab = "Frequency")
mtext(text = "1=Strongly favor, 2=Somewhat favor, 3=Somewhat oppose, 4=Strongly oppose, 8=Don´t know, 9= No answer", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#p27b= And how about marijuana?
b7 <- table(poll2000$p27_b)
barplot(b7, beside = T, col = "#FF4500", main = "And how about marijuana?", bty="l", xlab = "Answers", ylab = "Frequency")
mtext(text = "1=Strongly favor, 2=Somewhat favor, 3=Somewhat oppose, 4=Strongly oppose, 8=Don´t know, 9= No answer", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#-----------------------------2014----------------------------------
tmp <- file.choose()
poll2014 <- read.csv(file = tmp, header = TRUE, stringsAsFactors = FALSE)
str(poll2014)
head(poll2014)
poll2014["count"] <- 1
poll2014$count

barplot(table(poll2014$q1),  ylab = "count", xlab = "Gender", col = colors.gender, main = "Gender")
barplot(table(poll2014$q2), col = "#7FFF00", ylab = "count", xlab = "Gender")

#DISO10: Tell me please if you consider the problem: Illegal drug sales in your neighborhood, is ?
c1 <- tapply(poll2014$count, list(poll2014$q1,poll2014$diso10),sum)
barplot(c1, beside = T, col = colors.gender, main = "Do you consider that Illegal drug sales is ?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Very serious, 2=some serious, 3=little serious, 4=not serious, 5=It is not a problem", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#DISO14: Tell me please if you consider the problem: people consuming drugs in your neighborhood, is  
c2 <- tapply(poll2014$count, list(poll2014$q1,poll2014$diso14),sum)
barplot(c2, beside = T, col = colors.gender, main = "Do you consider that people consuming drugs in your neighborhood, is?", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Very serious, 2=some serious, 3=little serious, 4=not serious, 5=It is not a problem", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)

#VICBAR3: Have occurred Illegal drug sales In the past 12 months in your neighborhood?
c3 <- tapply(poll2014$count, list(poll2014$q1,poll2014$vicbar3),sum)
barplot(c3, beside = T, col = colors.gender, main = "Have occurred Illegal drug sales In the past 12 months in your neighborhood", bty="l", xlab = "Answers", ylab = "Frequency")
legend("topright", "(x,y)", pch = 14, legend = c("Males","Females"), ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = colors.gender)
mtext(text = "1=Yes, 2=No", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = .8)