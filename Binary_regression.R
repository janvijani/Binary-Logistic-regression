summary(bank_marketing)
summary(bank_marketing$job)
# blue-collar , housemaid , technician = blue-collar
# admin. , management  = desk_jobs
# retired , student, unemployed,unknown  = no_income 

bank_marketing$job<-as.character(bank_marketing$job)
bank_marketing$job[bank_marketing$job=="blue-collar"|
                     bank_marketing$job=="housemaid"|
                     bank_marketing$job=="technician"]<-"blue-collar"

bank_marketing$job[bank_marketing$job=="admin."|
                     bank_marketing$job=="management"]<-"desk_job"

bank_marketing$job[bank_marketing$job=="retired"|
                     bank_marketing$job=="student"|
                     bank_marketing$job=="unemployed"|
                     bank_marketing$job=="unknown"]<-"no_income"

bank_marketing$job[bank_marketing$job=="entrepreneur"|
                     bank_marketing$job=="self-employed"]<-"self-employed"

bank_marketing$job<-as.factor(bank_marketing$job)
summary(bank_marketing$job)

bank_marketing$default<-as.character(bank_marketing$default)
bank_marketing$default[bank_marketing$default=="unknown"]<-"no"
bank_marketing$default<-as.factor(bank_marketing$default)

bank_marketing$housing<-as.character(bank_marketing$housing)
bank_marketing$housing[bank_marketing$housing=="unknown"]<-"no"
bank_marketing$housing<-as.factor(bank_marketing$housing)

bank_marketing$loan<-as.character(bank_marketing$loan)
bank_marketing$loan[bank_marketing$loan=="unknown"]<-"no"
bank_marketing$loan<-as.factor(bank_marketing$loan)

summary(bank_marketing)
quantile(bank_marketing$campaign,probs = seq(0.9,1.0,0.01))

# as 91% of customers have value for campaign [no of times customer is contacted]
# either 5 or less than that ; so any value > 5 will be converted as 5

bank_marketing$campaign[bank_marketing$campaign>5]<-5
summary(bank_marketing$campaign)
bank_marketing$campaign<-as.factor(bank_marketing$campaign)

summary(bank_marketing$campaign)
head(bank_marketing$campaign,20)

bank_marketing$marital<-as.character(bank_marketing$marital)
bank_marketing$marital[bank_marketing$marital=="unknown"]<-"married"
bank_marketing$marital<-as.factor(bank_marketing$marital)
summary(bank_marketing)

quantile(bank_marketing$duration,probs = seq(0.9,1.0,0.01))
bank_marketing$duration[bank_marketing$duration>600]<-600
summary(bank_marketing$duration)
head(bank_marketing$duration,20)

head(bank_marketing$pdays,50)
bank_marketing$pdays<-NULL
# as most of the observations have value 999 ; the column has became almost
# constant ; so removing the column 

# Hold -Out Cross Validation 
# we convert our data as 75 - 25 % 
# we use 75 % of the data for training model 
# we build model on 75% of the data = training data 
# we evaluate model performance using confusion matrix / evaluation matrix
# we implement model on test data 
# we check performance on test data using confusion matrix
# we compare performance of training data with test data 
# if train and test data generates similar acc , sen , spe = model is validated
# if train and test data generates very diff acc, sen , spe = model is invalid 
# we cannot use invalid model for further predictive purpose 

#labeled training data [IP + expected OP ]- model - learned model 
# training data : 75 % data 
# we will create logistic regression in such a way that the regression op
# should match with expected op 

# we will use remaining 25 % data and we will implement model created in past 
# comp the predicted op on 25 % of the data with its Expected Op 


# 200 observation : apple / banana / orange 
# 150 - 50 
# 150 - train model 

# create training and testing data 
# no : 36000 ; yes : 4000


#downsampling
library(caret)
bank_marketing_ds<-downSample(bank_marketing,bank_marketing$y)
dim(bank_marketing_ds)
table(bank_marketing_ds$Class)

#using sample function 
set.seed(200)
#you can use any number here
#set.seed() uses permutation patterns and gives you the same set for indexes each time 
#it is random in nature but for each number there is a fixed permutation pattern
#Hence it creates the same sets of random values in the future
#we can work without it too
index<-sample(nrow(bank_marketing_ds), 0.75*nrow(bank_marketing_ds))
train_bank<-bank_marketing_ds[index,]
test_bank<-bank_marketing_ds[-index,]
head(train_bank)

null_bank<-glm(Class~1, data = train_bank, family= "binomial")
full_bank<-glm(Class~., data = train_bank, family= "binomial")

#if the error occurs about two or more levels,it means there is one variable in your dataset that only has one level  
#it means that when the programme seperated the main d/b into training and testing data, in any of the table, 
#only one value for that variable is present in the entire table 
#in that case, check summary of the each training and testing d/b
#Whichever column only havs one level(only one value), just convert it to null 

step(null_bank,direction="forward",
     scope=list(lower=null_bank, upper=full_bank))

bank_marketing_model<-glm(formula = Class ~ duration + nr.employed + month + poutcome + 
                                emp.var.rate + euribor3m + cons.price.idx + education + contact + 
                                job + loan, family = "binomial", data = train_bank)


#now generating probabilites using predict function 
pred_prob_train<-predict(bank_marketing_model,train_bank,type="response")
# use 0.5 as a cutoff to classify probabilities as yes and no 
# if prob >0.5 : Yes else No 
pred_class_train<-ifelse(pred_prob_train>=0.5,"yes","no")

table(actual=train_bank$Class,predicted=pred_class_train)
acc_train<-(2942+3172)/nrow(train_bank);acc_train #0.87
sen_train<- 3172 / (301+3172);sen_train # 0.91
spe_train<- 2942/(2942+549);spe_train # 0.84

#now we can say that our training data is working well 
#hence, we check the same model we created on the testing data 

pred_prob_test<-predict(bank_marketing_model,test_bank,type="response")
pred_class_test<-ifelse(pred_prob_test>=0.5,"yes","no")
table(actual=test_bank$Class,predicted=pred_class_test)

acc_test<-(972+1054)/nrow(test_bank);acc_test # 0.87
sen_test<- 1054 /(117+1054);sen_test # 0.90
spe_test<-972 /(972+177);spe_test # 0.84

#now lets check the summary
summary(bank_marketing_model)
