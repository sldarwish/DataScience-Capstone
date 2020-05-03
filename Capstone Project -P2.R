if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")



library(dplyr)
library(stringr)
library(caret)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(ggthemes)
library(readxl)
library(randomForest)
library(rpart)
library(GGally)

letter_rated <- function(v){
  v <- v %>% as.character %>% as.factor
  levels(v) <- list(Outstanding="5", VeryGood="4",
                    Good="3", Acceptable="2",Weak="1")
  return(v)
}

dsib<- read.csv("dsib.csv")
dsib <- dsib %>% as.data.frame
dsib <- dsib %>% mutate(School.Name= as.character(School.Name))
schooldata <- read.csv("schooldata.csv")
schooldata <- schooldata %>% select (-(grades)) %>% select(-School.Name)

schools <- left_join(dsib, schooldata, by=c("Schoolid"= "schoolid"))
schools <- schools %>% filter(!is.na(curriculum) )

curris <- schools$curriculum %>% str_split("-", simplify=TRUE)
schools <- schools %>% mutate (curriculum= curris[,1], dual= curris[,2])

schools <- schools %>% mutate (rating= as.factor(r1819*-1) )
levels(schools$rating) <- c("Outstanding", "Very Good","Good","Acceptable","Weak")
schools$rating <- ordered(schools$rating, levels=c("Outstanding", "Very Good","Good","Acceptable","Weak"))

#school curriculum vs. school rating distribution
rcolors <- c("green4", "chartreuse", "yellow","orange", "red")

schools %>% group_by(curriculum, rating) %>% summarize(total= n()) %>%
  ggplot(aes(curriculum,total, fill= rating)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual("School Rating", values= rcolors)+
  xlab("School Curriculum")+
  ylab("Total number of Schools")+
  ggtitle("School Curriculum vs. School Rating")+
  theme(axis.text.x=element_text(angle=90,hjust=1))


#Year School was Established vs. Rating
schools %>% group_by(established, rating) %>% summarize(total= n()) %>% 
  ggplot(aes(established, total, fill= rating)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual("School Rating",values= rcolors)+
  scale_x_continuous(name= "Year School was Established", breaks=seq(1957,2017,3)) +
  ylab("Total Number of Schools")+
  ggtitle("Year School was Established vs. School Rating")+
  theme(axis.text.x=element_text(angle=90,hjust=1))
 
 #School Leadership Effectiveness and School Self Evaluation, vs. rating
 schools %>% ggplot(aes(P6.1, P6.2, color= rating)) +
   geom_jitter(width=0.1, height = 0.1)+
   scale_color_manual("School Rating",values= rcolors)+
   scale_x_continuous(name= "6.1: Effectiveness of School Leadership", breaks= c(1,2,3,4,5),
                      labels=c( "Weak", "Acceptable","Good","Very Good","Outstanding" ) ) +
   scale_y_continuous(name= "6.2: School Self-Evaluation", breaks= c(1,2,3,4,5),
                      labels=c( "Weak", "Acceptable","Good","Very Good","Outstanding" ) )+
   ggtitle("School Leadership vs. School Rating")

 #Student Enrollment vs. School Rating
 library(scales)
 schools %>% group_by(rating, enroll1819) %>% summarize(totalen= sum(enroll1819)) %>%
   ggplot(aes(rating, enroll1819, color= rating)) +
   geom_boxplot() + 
   scale_y_log10(name= "Number of students enrolled - log10", 
                 breaks= trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
   xlab("School Rating")+
   scale_color_manual(values= rcolors)+
   theme(legend.title = element_blank(), legend.position = "none") 
 
 
#Converting curriculum categorical data into numeric
# schools <- schools %>% mutate(curriculum = unclass(curriculum),
#                               dual= unclass(dual))

dps <- schools %>% select(Schoolid, School.Name,location, curriculum, dual,
                          established, enroll1819, KG3.1, E3.1, M3.1, H3.1,
                          KG5.1, E5.1, M5.1, H5.1, KG5.2, E5.2, M5.2, H5.2,
                          P6.1, P6.2, P6.3, P6.4, P6.5, r1617, r1718, r1819, rating) %>% 
                          filter(!is.na(P6.1))

#Grouping schools with "rare" curriculums
Other <- c("Japanese", "Russian", "Pakistani","Philippine", "SABIS", "German")
saved_cur <-  dps$curriculum
dps <- dps %>% mutate (curriculum= ifelse(curriculum %in% Other, "Other", saved_cur)) %>%
  mutate( curriculum= as.factor(curriculum))

#regraphing school curriculum vs. school rating 
dps %>% group_by(curriculum, rating) %>% summarize(total= n()) %>%
  ggplot(aes(curriculum,total, fill=  rating)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual("School Rating", values= rcolors)+
  xlab("School Curriculum")+
  ylab("Total number of Schools")+
  ggtitle("School Curriculum vs. School Rating")+
  theme(axis.text.x=element_text(angle=90,hjust=1))



#RMSE function
RMSE <- function(trueratings, predictedratings){
  sqrt(mean((trueratings-predictedratings)^2))
}

dps[is.na(dps)] <- 0
dps <- dps %>% mutate(phases=4)
dps$phases<- 4-rowSums(dps[,8:11]==0)

dps <- dps %>% mutate(enroll= round(enroll1819/150),
                          E= round(established/3)*3,
                          phase_size = ceiling((enroll1819/phases)/100)*100,
                          te_avg= ((KG3.1+ E3.1+ M3.1+ H3.1)/phases),
                          safe_avg=(KG5.1+ E5.1+ M5.1+ H5.1)/phases,
                          avg= ((P6.1+ P6.2)/2))
dps$dual[dps$dual==""] <- c("None")

#Student Enrollment per phase
dps %>% 
  ggplot(aes(y=phases, x=enroll1819, color= rating)) +
  geom_point() + 
  scale_x_log10(name= "Number of students enrolled - log10", 
                breaks= trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x) )) +
  ylab("Number of educational phases") +
  scale_color_manual("School Rating", values= rcolors)

#The teaching effect vs. school rating
teachingmat <- dps %>% select(KG3.1, E3.1, M3.1, H3.1, r1819, rating)
zeroindex <- which(teachingmat==0)
zindex <- arrayInd(zeroindex,dim(teachingmat))[,1]
teachingmat[teachingmat==0] <- teachingmat$r1819[zindex]
teachingmat %>% select(KG3.1, E3.1, M3.1, H3.1, r1819, rating)  %>% 
  ggparcoord(columns= 1:4,
             groupColumn= 6,
             showPoints=TRUE,
             scale= "globalminmax",
             alphaLines= 0.3, splineFactor= TRUE) +
  scale_color_manual(values= rcolors,"School Rating" ) +
  scale_y_continuous(name= "3.1: Teaching for Effective Learning",
                     breaks= c(1,2,3,4,5),
                     labels= c( "Weak", "Acceptable","Good","Very Good","Outstanding" ) ) +
  scale_x_continuous(name= "School Phases",
                     breaks= c(1,2,3,4),
                     labels= c( "Kindergarten", "Elementary", "Middle", "High" ) )+
  ggtitle("Teaching for Effective Learning\n in Each School Phase")

#health and safety vs. School rating

safetymat <- dps %>% select(KG5.1, E5.1, M5.1, H5.1, r1819, rating)
zeroindex <- which(safetymat==0)
zindex <- arrayInd(zeroindex,dim(safetymat))[,1]
safetymat[safetymat==0] <- safetymat$r1819[zindex]
safetymat  %>%
  ggparcoord(columns= 1:4,
             groupColumn= 6,
             showPoints=TRUE,
             scale= "globalminmax",
             alphaLines= 1, splineFactor= TRUE) +
  scale_color_manual(values= rcolors,"School Rating" ) +
  scale_y_continuous(name= "5.1: Health and Safety",
                     breaks= c(1,2,3,4,5),
                     labels= c( "Weak", "Acceptable","Good","Very Good","Outstanding" ) ) +
  scale_x_continuous(name= "School Phases",
                     breaks= c(1,2,3,4),
                     labels= c( "Kindergarten", "Elementary", "Middle", "High" ) )+
  ggtitle("Health and Safety in Each School Phase")

#Previous school rating vs. School rating 18-19
dps %>% ggplot(aes(y= r1617, yend= r1819, x= School.Name, xend=School.Name) )+ 
  geom_segment( color= "grey") +
  geom_point(aes(color= '16-17')) + 
  geom_point(aes(dps$School.Name, dps$r1718,
                 color= '17-18'), shape= 17) + 
  geom_point(aes(dps$School.Name, dps$r1819,
                 color= '18-19'), shape= 15)+
  scale_y_continuous(name="School Inspection Results",
                     breaks= c(1,2,3,4,5),
                     labels= c( "Weak", "Acceptable","Good","Very Good","Outstanding" )) +
  xlab("School Name") +
  scale_color_manual("School Rating", values=c("red", "blue", "darkgreen") ) +
  theme(axis.text.x=element_text(angle=90,hjust=1, vjust=1, size=2))

#partitioning the data into training, testing, and validation sets

set.seed(311)
testindex <- createDataPartition(y = dps$r1819, times = 1, p = 0.3, list = FALSE)
train <- dps[-testindex,]
testing <- dps[testindex,]

set.seed(311)
t_index <- createDataPartition(y = testing$r1819, times = 1, p = 0.5, list = FALSE)
test <- testing[-t_index,]
validation  <- testing[t_index,]

#Average school rating
mu <- mean(train$r1819)
test <- test %>% mutate(predicted= mu)
method<- c("")
rmserror<- 0

errors <- data.frame(method, rmserror, stringsAsFactors = FALSE)
errors <- errors %>% mutate(method= c("RMSE from using the mean rating only:"),
                             rmserror= RMSE(test$r1819, test$predicted))
#The curriculum effect

#calculating mean of curriculum
curriculumbias <- train  %>% group_by(curriculum) %>% summarize(b_curr = mean(r1819- mu))

train <- train %>% left_join(curriculumbias)

test<- test %>% left_join(curriculumbias)%>%
  mutate(predicted= mu+b_curr)

dualbias <- train %>% group_by(dual) %>% summarize (b_dual= mean(r1819- mu - b_curr))
train <- train %>% left_join(dualbias)
test <- test %>% left_join(dualbias) %>% mutate(predicted= mu + b_curr + b_dual)

errors <- errors %>% add_row(method= c("RMSE from using the mean and curriculum effect"),
                             rmserror= RMSE(test$r1819, test$predicted))


#calculating Leadership effects
#SEF effect
SEFbias <- train  %>%
   group_by (avg) %>%
  summarize(b_l = mean(r1819 - mu - b_curr - b_dual))

train <- train %>% left_join(SEFbias)

test <- test %>% left_join(SEFbias)%>%
  mutate(predicted= mu + b_curr + b_dual + b_l)

errors <- errors %>% add_row(method= c("RMSE from using the mean, curriculum,and leadership effects"),
                             rmserror= RMSE(test$r1819, test$predicted))

#Year school was established effect

establishedbias <- train  %>% group_by(E) %>% 
  summarize(b_est = mean(r1819- mu- b_curr- b_dual - b_l))
train <- train %>% left_join (establishedbias)
test <- test %>% left_join(establishedbias) %>% 
  mutate(predicted= mu + b_curr + b_dual +b_l+ b_est) 

#fail safe against data partition
test$b_est[is.na(test$predicted)] <- 0
test <- test %>% mutate (predicted= mu + b_curr + b_dual +b_l+ b_est )
test$predicted[which(test$predicted>5)] <- 5

errors <- errors %>% add_row(
  method= c(
  "RMSE from using the mean, curriculum, leadership,\n and year established effects"),
                             rmserror= RMSE(test$r1819, test$predicted))


#Enrollment effect
enrollbias <- train %>%
  group_by(enroll) %>% summarize(b_en = mean(r1819- mu - b_curr - b_dual- b_l - b_est))
train <- train %>% left_join(enrollbias)
test <- test %>% left_join(enrollbias) %>%
  mutate(predicted= mu+b_curr+ b_dual + b_l + b_est+ b_en) 
test$b_en[is.na(test$predicted)] <- 0
test <- test %>% mutate(predicted= mu+b_curr+ b_dual + b_l + b_est+ b_en)
test$predicted[which(test$predicted>5)] <- 5

errors <- errors %>% add_row(method= c(
  "RMSE from using the mean, curriculum, leadership,\n and year established, enrollment effects"),
                             rmserror= RMSE(test$r1819, test$predicted))

#Teaching effect

teachingbias <- train %>% group_by(te_avg)%>%
 summarize(b_te= mean(r1819- mu - b_curr- b_dual -b_l-b_est-b_en))
train <- train %>% left_join(teachingbias)

test <- test %>% left_join(teachingbias)%>%
  mutate(predicted= mu + b_curr + b_dual + b_l + b_est + b_en + b_te) 
test$b_te[is.na(test$predicted)] <- 0
test <- test %>% mutate(predicted= mu + b_curr + b_dual + b_l + b_est + b_en + b_te) 
test$predicted[which(test$predicted>5)] <- 5
errors <- errors %>% add_row(method= c(
  "RMSE from using the mean, curriculum, leadership,\n and year established, enrollment, teaching effects"),
                             rmserror= RMSE(test$r1819, test$predicted))

#School size effect
sizebias <- train %>% group_by(phases,phase_size) %>% 
  summarize(b_ss=mean(r1819 - mu - b_curr - b_dual - b_l - b_est - b_en - b_te))
train <- train %>% left_join(sizebias)

test <- test %>% left_join(sizebias) %>%
  mutate(predicted= mu + b_curr + b_dual + b_l + b_est + b_en + b_te + b_ss) 
test$b_ss[is.na(test$predicted)] <- 0
test <- test %>% 
  mutate(predicted= mu + b_curr + b_dual + b_l + b_est + b_en + b_te + b_ss) 

test$predicted[which(test$predicted>5)] <- 5

errors <- errors %>% add_row(method= c(
  "RMSE from using the mean, curriculum, leadership,\n and year established, enrollment, teaching,\n school size effects"),
                             rmserror= RMSE(test$r1819, test$predicted))



#Safety bias

safetybias <- train %>% group_by(safe_avg) %>% 
 summarize(b_safe= mean(r1819- mu -b_curr -b_dual -b_l -b_est -b_en -b_te -b_ss))
train <- train %>% left_join(safetybias)

test <- test %>% left_join(safetybias) %>%
  mutate(predicted= mu +b_curr +b_dual +b_l +b_est +b_en +b_te +b_ss +b_safe) 

#failsafe against data partitioning
test$b_safe[is.na(test$predicted)] <- 0
test <- test %>% 
  mutate(predicted= mu + b_curr + b_dual + b_l + b_est + b_en + b_te + b_ss + b_safe) 
test$predicted[which(test$predicted>5)] <- 5

errors <- errors %>%
  add_row(method= c(
    "RMSE from using the mean, curriculum, leadership,\n and year established, enrollment, teaching,\n school size, safety effects"),
                             rmserror= RMSE(test$r1819, test$predicted))

#Previous rating bias 
prevbias <- train %>% group_by(r1617, r1718) %>% 
  summarize(b_prev= mean(r1819 -mu -b_curr -b_dual -b_l -b_est -b_en -b_te -b_ss -b_safe))

train <- train %>% left_join(prevbias)

test <- test %>% left_join(prevbias)%>%
  mutate(predicted= mu +b_curr +b_dual +b_l +b_est +b_en +b_te +b_ss +b_safe +b_prev) 

test$b_prev[is.na(test$predicted)] <- 0

test <- test %>% 
  mutate(predicted= mu +b_curr +b_dual +b_l +b_est +b_en +b_te +b_ss +b_safe +b_prev)
test$predicted[which(test$predicted>5)] <- 5

errors <- errors %>% add_row(method= c(
    "RMSE from using the mean, curriculum, leadership,\n and year established, enrollment, teaching,\n school size, safety, previous rating effects"),
    rmserror= RMSE(test$r1819, test$predicted))
errors


 test %>% ggplot(aes(y=r1819, x=predicted, color= as.factor(r1819) )) +
   geom_point() + scale_color_manual(values= ratingscolor) + xlab("test$predicted")


 #resetting trainset 
 train <- dps[-testindex,] 
 
 regularized <- function(lambda)
 {
   mu <- mean( train$r1819)
   
   curriculumbias <- train  %>% group_by(curriculum) %>% 
     summarize(b_curr= sum(r1819- mu)/(n()+lambda))
   train <- train %>% left_join(curriculumbias)
   
   dualbias <- train %>% group_by(dual) %>%
     summarize(b_dual= sum(r1819- mu -b_curr)/(n()+lambda))
   train <- train %>% left_join(dualbias)
   
   SEFbias <- train   %>% group_by (avg) %>% 
     summarize(b_l= sum(r1819- mu- b_curr -b_dual)/(n()+lambda))
   train <- train %>% left_join(SEFbias)
   
   establishedbias <- train %>% group_by(E) %>%
     summarize(b_est= sum(r1819- mu -b_curr -b_dual -b_l)/(n()+lambda))
   train <- train %>% left_join(establishedbias)
   
   enrollbias <- train %>% group_by(enroll) %>% 
     summarize(b_en= sum(r1819- mu -b_curr -b_dual -b_l -b_est)/(n()+lambda))
   train <- train %>% left_join(enrollbias)
 
   teachingbias <- train %>% group_by(te_avg) %>% 
     summarize(b_te= sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en)/(n()+lambda))
   train <- train %>% left_join(teachingbias)
   
   sizebias <- train %>% group_by(phases,phase_size) %>% 
     summarize(b_ss= sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en -b_te)/(n()+lambda))
   train <- train %>% left_join(sizebias)
   
   safetybias <- train %>% group_by(safe_avg) %>% 
     summarize(b_safe= sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en -b_te -b_ss)/(n()+lambda))
   train <- train %>% left_join(safetybias)
   
   prevbias <- train %>% group_by(r1617, r1718) %>% 
     summarize(b_prev=sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en -b_te -b_ss -b_safe)/(n()+lambda))
   train <- train %>% left_join(prevbias)
   
   train <- train %>% mutate(predicted= mu+b_curr +b_dual +b_l+b_est+b_en+b_te+b_ss+b_safe+b_prev)
   train$predicted[which(train$predicted>5)] <- 5
   
   RMSE(train$r1819, train$predicted)
   }
 
 lambdas <- seq(0,5,0.01)
 rmses <- sapply(lambdas, regularized)
 
 qplot(lambdas, rmses)
 lambda <- lambdas[which.min(rmses)]
 print("Minimum RMSE: ")
 min(rmses)

#resetting training again
train <- dps[-testindex,] 

#Using best lambda
curriculumbias <- train  %>% group_by(curriculum) %>% 
  summarize(b_curr= sum(r1819- mu)/(n()+lambda))
train <- train %>% left_join(curriculumbias)

dualbias <- train %>% group_by(dual) %>%
  summarize(b_dual= sum(r1819- mu -b_curr)/(n()+lambda))
train <- train %>% left_join(dualbias)

SEFbias <- train   %>% group_by (avg) %>% 
  summarize(b_l= sum(r1819- mu- b_curr -b_dual)/(n()+lambda))
train <- train %>% left_join(SEFbias)

establishedbias <- train %>% group_by(E) %>%
  summarize(b_est= sum(r1819- mu -b_curr -b_dual -b_l)/(n()+lambda))
train <- train %>% left_join(establishedbias)

enrollbias <- train %>% group_by(enroll) %>% 
  summarize(b_en= sum(r1819- mu -b_curr -b_dual -b_l -b_est)/(n()+lambda))
train <- train %>% left_join(enrollbias)

teachingbias <- train %>% group_by(te_avg) %>% 
  summarize(b_te= sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en)/(n()+lambda))
train <- train %>% left_join(teachingbias)

sizebias <- train %>% group_by(phases,phase_size) %>% 
  summarize(b_ss= sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en -b_te)/(n()+lambda))
train <- train %>% left_join(sizebias)

safetybias <- train %>% group_by(safe_avg) %>% 
  summarize(b_safe= sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en -b_te -b_ss)/(n()+lambda))
train <- train %>% left_join(safetybias)

prevbias <- train %>% group_by(r1617, r1718) %>% 
  summarize(b_prev=sum(r1819- mu -b_curr -b_dual -b_l -b_est -b_en -b_te -b_ss -b_safe)/(n()+lambda))
train <- train %>% left_join(prevbias)

train <- train %>% mutate(predicted= mu+b_curr +b_dual +b_l+b_est+b_en+b_te+b_ss+b_safe+b_prev)
train$predicted[which(train$predicted>5)] <- 5
RMSE(train$r1819, train$predicted)

test <- testing[-t_index,]

test <- test %>%
     left_join(curriculumbias) %>% left_join(dualbias) %>%
     left_join(SEFbias) %>% left_join(establishedbias) %>%
     left_join(enrollbias) %>% left_join(teachingbias)%>%
    left_join(sizebias) %>% left_join(safetybias) %>% left_join(prevbias) %>%
  mutate(predicted= mu +b_curr +b_dual +b_l +b_est +b_en +b_te +b_ss +b_safe +b_prev)

test[is.na(test)] <- 0
test <- test %>% mutate(predicted= mu +b_curr +b_dual +b_l +b_est +b_en +b_te +b_ss +b_safe +b_prev)
test$predicted[which(test$predicted>5)] <- 5

errors <- errors %>% add_row(method= c(
  "RMSE from using the mean, curriculum, leadership,\n and year established, enrollment, teaching,\n school size, safety, previous rating\n effects with regularization"),
  rmserror= RMSE(test$r1819, test$predicted))
errors

final <- validation%>% 
  left_join(curriculumbias) %>% left_join (dualbias) %>%
  left_join(SEFbias) %>% left_join(establishedbias) %>%
  left_join(enrollbias) %>% left_join(teachingbias) %>%
  left_join(sizebias) %>% left_join(safetybias) %>% left_join(prevbias)

final <- final %>% mutate(predicted= mu+b_curr +b_dual +b_l+b_est+b_en+b_te+b_ss+b_safe+b_prev) 
final$predicted[which(final$predicted>5)] <- 5


final$b_est[is.na(final$predicted)] <- 0
final$b_en[is.na(final$predicted)] <- 0
final$b_te[is.na(final$predicted)] <- 0
final$b_ss[is.na(final$predicted)] <- 0
final$b_safe[is.na(final$predicted)] <- 0
final$b_prev[is.na(final$predicted)] <- 0
  
final <- final %>% mutate(predicted= mu + b_curr + b_l + b_est +
                            b_en + b_te + b_ss + b_safe + b_prev) 
final$predicted[which(final$predicted>5)] <- 5

print(paste0("RMSE of validation dataset only: ", round(RMSE(final$r1819, final$predicted), digits=7)))



