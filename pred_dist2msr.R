install.packages("ggplot2")
install.packages("readtext")
install.packages("psych")
#install.packages("car")
#install.packages("ggpubr")
#install.packages("dgof")
#install.packages("kSamples")

library(readtext)
library(ggplot2)
library(psych)
#library(car)
#library(ggpubr)
#library(dgof)
#library(kSamples)

setwd("Documents/BookClub/BC2Clean")

#grep -v NM predict_ires2_6b.txt > predict_ires2_6b_m.txt
fname <- "predict_in_6bmsr1_m.txt"
m_test <- read.delim(fname, sep = "", header = T, na.strings = " ", fill = T)
mr <- nrow(m_test)
#for (i in 1:mr){
#  m_test[i,28:49] <- sort(m_test[i,7:27], decreasing = T)
#}
t2 <- m_test[,1:7]
#t2[,4] <- m_test[,6]
t2[,8] <- FALSE
#t2[,6:8] <- m_test[,28:30]
#t2[,9] <- "Y"
#t2[,10] <- m_test[,4]
names(t2) <- c("Correct_class", "Score", "Guess_class", "TrustedVote", "VoteScore", "TrustedScore","File", "Flag")
for (i in 1:mr){
  if( t2$Correct_class[i] == t2$Guess_class[i] ){
    t2$Flag[i] <- TRUE
  }
}

t2r <- t2[t2$Flag == TRUE,]
t2w <- t2[t2$Flag != TRUE,]


TVt = 0.5
VSt = 0.95

#Untrusted Accuracy
length(t2r$Score)/(length(t2r$Score) + length(t2w$Score))

#Trusted Accuracy Meta
(sum(t2r$TrustedVote >= TVt)+sum(t2w$TrustedVote < TVt))/(length(t2r$Score) + length(t2w$Score))
(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)+sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt))/(length(t2r$Score) + length(t2w$Score))
#Precision Meta
sum(t2r$TrustedVote >= TVt)/(sum(t2r$TrustedVote >= TVt) + sum(t2w$TrustedVote >= TVt))
sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)/(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt) + sum(t2w$TrustedVote >= TVt & t2w$VoteScore >= VSt))
#Recall Meta
sum(t2r$TrustedVote >= TVt)/(sum(t2r$TrustedVote >= TVt) + sum(t2r$TrustedVote < TVt)) #length(t2r$Score)
sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)/(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt) + sum(t2r$TrustedVote < TVt & t2r$VoteScore >= VSt))
#Specificity Meta
sum(t2w$TrustedVote < TVt)/(sum(t2w$TrustedVote < TVt) + sum(t2w$TrustedVote >= TVt)) #length(t2w$Score)
sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt)/(sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt) + sum(t2w$TrustedVote >= TVt & t2w$VoteScore >= VSt))
