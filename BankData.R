library(tidyverse)
library(flextable)
library(e1071)
library(flextable)
library(dlookr)
library(mice)
library(VIM)
library(gtExtras)
library(gtsummary)
library(visdat)
library(ggstatsplot)
library(ggcorrplot)
library(randomForest)
library(caret)
library(PerformanceAnalytics)
library(performance)
library(class)
library(caTools)
library(equatiomatic)
library(sjPlot)
library(emmeans)
library(rpart)
library(rpart.plot)
library(report)
library(ggeffects)
library(effectsize)



bank = read.csv("E:/data sets/Bank data/bank.csv", sep = ";")
head(bank)
tail(bank)

# preparation for nice plots
theme_set(theme_bw()+
            theme(title = element_text(color = "#4CBB17",
                                       size = 18,
                                       face = "bold"),
                  legend.position = "bottom",
                  axis.text = 
                    element_text(size = 10,
                                 color = "#6495ED",
                                 face = "bold"),
                  axis.title = element_text(size = 12,
                                            face = "bold",
                                            colour = "#FF5733")))
                                            
                                            
## the first 5 characters                                                                                       colour = "#FF5733")))
bank %>% 
  head(5) %>% 
  gt()
## the last 5 characters
bank %>% 
  tail(5) %>% 
  regulartable() %>% 
  theme_box()
##Checking the data types
str(bank)
dim(bank)

## Checking for duplicates
bank %>% 
  filter(duplicated(.)) %>% 
  view()

## EDA
summary(bank)

## Checking the number of Outliers
diagnose_outlier(bank) %>% 
  flextable()

unique(bank$marital)
unique(bank$education)
unique(bank$job)
unique(bank$default)
unique(bank$housing)
unique(bank$loan)
unique(bank$month)
unique(bank$contact) # no missing values
unique(bank$day_of_week) # no missing values
unique(bank$poutcome)  # no missng values
unique(bank$y) # no missng values


#1. Age

summary(bank$age)

quantile(bank$age, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

##Plotting a Histogram of Age
bank %>% 
  ggplot(aes(age))+
  geom_histogram(binwidth = 7, fill = "steelblue")+
  geom_vline(aes(xintercept = mean(age)), size = 1.0, colour = "red",linetype = "dashed")+
  annotate("text",x = 40.0, y = 11000,
          label = paste("Mean \n ", round(mean(bank$age),2)),
           color = "darkred",
           size = 5)+
  annotate("text",x = 80.0, y = 12000,
           label = paste("Skewness \n ", round(skewness(bank$age),3)),
           color = "navyblue",
           size = 3)+
  annotate("text",x = 80.0, y = 10000,
           label = paste("Kurtosis \n ", round(kurtosis(bank$age),3)),
           color = "red",
           size = 3)+
  scale_size(range = c(12,12))+
  scale_x_continuous(breaks = c(20,40,60,80))+
  labs(title = "Histogram Of Age",
       x = "Age",
       y = NULL)
# checking for normality across the data
bank %>% 
  select(1,11:13,16:20) %>% 
  normality() %>% 
  mutate(across(is.numeric, ~round(., 3))) %>% 
  regulartable()

Normality<- bank %>% 
  select(1,11:13,16:20) 
normality(Normality) %>% 
  flextable()


# 2. Job
unique(bank$job)
## Frequencies of job type
bank %>% 
  plot_frq(job)+
  labs(title = "Bar plot of Job Type",
       x = "Job Type")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

bank %>% 
  ggplot(aes(job))+
  geom_bar(aes(fill = y), position = "dodge", stat = "count",show.legend = T,alpha = .5)+
  labs(title = "Bar plot of Job Type",
       x = "Job Type",
       fill = "Subscription \nof Service")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

#3. Marital status
bank %>% 
  ggplot(aes(marital))+
  geom_bar(aes(fill = y), position = "dodge", stat = "count",show.legend = T)+
  labs(title = "Bar plot of Marital status",
       x = "Marital Status",
       fill = "Purchase \nof Service")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

#4.Education
bank %>% 
  ggplot(aes(education))+
  geom_bar(aes(fill = y), position = "dodge", stat = "count",show.legend = T,alpha = .7)+
  labs(title = "Bar plot of Education",
       x = "Education",
       fill = "Purchase \nof Service")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

#6. Housing
bank %>% 
  ggplot(aes(housing))+
  geom_bar(aes(fill = y), position = "dodge", stat = "count",show.legend = T,alpha = .5)+
  labs(title = "Bar plot of Housing",
       x = "Housing",
       fill = "Purchase \nof Service")

#7. Loan
bank %>% 
  ggplot(aes(loan))+
  geom_bar(aes(fill = y), position = "dodge", stat = "count",show.legend = T,alpha = .5)+
  labs(title = "Bar plot of Loan",
       x = "Loan",
       fill = "Purchase \nof Service")

#8. Conntact
bank %>% 
  ggplot(aes(contact))+
  geom_bar(aes(fill = y), position = "dodge", stat = "count",show.legend = T)+
  labs(title = "Bar plot of Contact",
       x = "Contact",
       fill = "Purchase \nof Service")

#9. Month
bank %>% 
  ggplot(aes(month))+
  geom_bar(aes(fill = y),position = "dodge",stat = "count",show.legend = T)+
  labs(title = "Bar plot of Months",
       x = "month",
       fill = "Purchase \nof Service")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

#10. Day of the week
bank %>% 
  ggplot(aes(day_of_week))+
  geom_bar(aes(fill = y),position = "dodge",stat = "count",show.legend = T)+
  labs(title = "Bar plot of Days of the week",
       x = "day of the week",
       fill = "Purchase \nof Service")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

#11. Duration
summary(bank$duration)
quantile(bank$duration, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

bank %>% 
  ggplot(aes(duration))+
  geom_histogram(bins = 20,fill = "steelblue")+
  geom_vline(aes(xintercept = mean(duration)), size = 1.0, colour = "red",linetype = "dashed")+
  annotate("text",x = 258.0, y = 18000,
          label = paste("Mean \n ", round(mean(bank$duration),2)),
         color = "darkred",
        size = 5)+
    annotate("text",x = 4500.0, y = 19000,
           label = paste("Skewness \n ", round(skewness(bank$duration),3)),
           color = "navyblue",
          size = 3)+
  annotate("text",x = 4500.0, y = 17000,
          label = paste("Kurtosis \n ", round(kurtosis(bank$duration),3)),
         color = "red",
      size = 3)+
labs(title = "Histogram Of Duration",
     x = "Duration in (seconds)",
     y = NULL)

bank %>%
  group_by(y) %>% 
  summarise(LowerAge = min(age),
            LowerDuration = min(duration),
            Lowercampaign = min(campaign),
            Lowerpdays = min(pdays),
            LowerPrevContacts = min(previous),
            LowerEmployVAR = min(emp.var.rate),
            LowerConsumerprice = min(cons.price.idx),
            LowerConsumerconf = min(cons.conf.idx),
            LowerEuribor = min(euribor3m),
            LowerNo_Emp = min(nr.employed)) %>%
  flextable()

#11.campaign
summary(bank$campaign)
quantile(bank$campaign, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))
bank %>% 
  ggplot(aes(campaign))+
  geom_histogram(bins = 20,fill = "steelblue")+
  geom_vline(aes(xintercept = mean(campaign)), size = 1.0, colour = "red",linetype = "dashed")+
  annotate("text",x = 3.0, y = 15500,
          label = paste("Mean \n ", round(mean(bank$campaign),2)),
         color = "darkred",
        size = 5)+
labs(title = "Number of contacts during \nthe campaign",
     x = "No of contacts in campaign",
     y = NULL)

# 12.Number of times passed after the client was previously contacted(pdays)
summary(bank$pdays)

quantile(bank$pdays, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

bank %>% 
  ggplot(aes(pdays))+
  geom_histogram(bins = 20,fill = "steelblue")+
labs(title = "No of Contacts perfomed\n before campaign",
     x = "No of Contacts perfomed before campaign",
     y = NULL)
# 15. Outcome pf previous marketing campaign(poutcome)
bank %>% 
  ggplot(aes(poutcome))+
  geom_bar(aes(fill = y),position = "dodge",stat = "count",show.legend = T)+
  labs(title = "Bar plot of POutcome",
       x = "Previous marketing outcome",
       fill = "Purchase \nof Service")
##### SOCIAL AND ECONOMIC ATTRIBUTES

# 16. Employment variation rate - quartely indicator
summary(bank$emp.var.rate)
quantile(bank$emp.var.rate, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

bank %>% 
  ggplot(aes(emp.var.rate))+
  geom_histogram(bins = 20,fill = "steelblue")+
labs(title = "Variation rate\n of Employment",
     x = "Variation rate of Employment",
     y = NULL)

# 17. Consumer price index- monthly indicator
summary(bank$cons.price.idx)
quantile(bank$cons.price.idx, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

bank %>% 
  ggplot(aes(cons.price.idx))+
  geom_histogram(bins = 15,fill = "steelblue")+
  geom_vline(aes(xintercept = mean(cons.price.idx)), size = 1.0, colour = "red",linetype = "dashed")+
  annotate("text",x = 94.0, y = 93,
          label = paste("Mean \n ", round(mean(bank$cons.price.idx),2)),
         color = "darkred",
        size = 5)+
labs(title = "Monthly consumer\n price index",
     x = "Monthly consumer price index",
     y = NULL)

## 18. Consumer confidex index- monthly indicator
summary(bank$cons.price.idx)
quantile(bank$cons.price.idx, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

bank %>% 
  ggplot(aes(cons.conf.idx))+
  geom_histogram(bins = 15,fill = "steelblue")+
labs(title = "Monthly consumer conf index",
     x = "Monthly consumer confidence index",
     y = NULL)

## 19. Euribor 3 month rate- daily indicator
summary(bank$euribor3m)
quantile(bank$euribor3m, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

bank %>% 
  ggplot(aes(euribor3m))+
  geom_histogram(binwidth = 1,fill = "steelblue")+
labs(title = "Euribor 3 month rate",
     x = "Euribor 3 month rate",
     y = NULL)

## 20. Number of employees - quartely indicator
summary(bank$nr.employed)
quantile(bank$nr.employed, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))


bank %>% 
  ggplot(aes(nr.employed))+
  geom_histogram(binwidth = 100,fill = "steelblue")+
  geom_vline(aes(xintercept = mean(nr.employed)), size = 1.0, colour = "red",linetype = "dashed")+
  annotate("text",x = 5167.0, y = 5225,
          label = paste("Mean \n ", round(mean(bank$nr.employed),2)),
         color = "darkred",
        size = 5)+
labs(title = "Number of Employees",
     x = "Employee numbers",
     y = NULL)

###### OUTPUT VARIABLE
## 21. Will the customer subscribe to a term of deposit?
bank %>% 
  ggplot(aes(y))+
  geom_bar(aes(fill = y), position = "dodge", stat = "count",show.legend = F)+
  labs(title = "Bar plot of Customer\n Subscription",
       x = "Customer Subscription",
       fill = "Purchase \nof Service")


#### DATA CLEANING
## Handling missing data

##checking for the percentage of missing data
p<- function(bank) {sum(is.na(bank))/length(bank)*100}
apply(bank,2,p)

# there are no missing values as per the above function in our data yet 
# the missing values have been coded as "unknown"

# dealing with missing values and coding them to NA
bank$job[bank$job == "unknown"]<-NA
bank$marital[bank$marital == "unknown"]<-NA
bank$education[bank$education == "unknown"]<-NA
bank$housing[bank$housing == "unknown"]<-NA
bank$loan[bank$loan == "unknown"]<-NA

## converting to factors
bank$job<- factor(bank$job)
bank$marital<-factor(bank$marital)
bank$education<-factor(bank$education)
bank$housing<-factor(bank$housing)
bank$loan<-factor(bank$loan)





## Imputation of the categorical variables

plot_na_pareto(bank, only_na = T) ## Finding the visual format of missing data


bank_data<- mice(bank, method = "polyreg", m =1)
bankk<- complete(bank_data,1)

## Visualizing the data (Imputed data)
ggplot()+
  geom_bar(data = bank, aes(job), width = 0.3)+
  geom_bar(data = bankk, aes(job), fill = "red",
          position = position_nudge(x = 0.25), width = 0.3)

## Chi-square tests of independence (Between 2 categorical variables)


ggbarstats(
  data = bank,
  x = marital,
  y = y,
  label = "both"
)

ggbarstats(
  data = bank,
  x = education,
  y = y,
  label = "both"
)


ggbarstats(
  data = bank,
  x = loan,
  y = y,
  label = "both"
)



bankk <-bankk %>% 
  select(-default) %>% 
  mutate(job= case_when(job == "admin" ~ "1",
                        job ==  "blue-collar" ~ "2",
                        job == "entrepreneur" ~ "3",
                        job == "housemaid" ~ "4",
                        job == "management"~ "5",
                        job == "retired" ~ "6",
                        job == "self-employed"~ "7",
                        job == "services"~ "8",
                        job == "student"~ "9",
                        job == "technician"~ "10",
                         TRUE ~ "11")) %>% 
  mutate(marital = case_when(marital == "divorced" ~ "0",
                             marital == "married" ~ "1",
                         TRUE ~ "2")) %>%
  mutate(education = case_when(education == "basic.4y"~ "0",
                               education == "basic.6y"~ "1",
                               education == "basic.9y"~ "2",
                               education == "high.school"~ "3",
                               education == "illiterate"~ "4",
                               education == "professional.course"~ "5",
                                 TRUE ~ "6")) %>%
  mutate(housing = case_when(housing == "no"~ "0",
                             TRUE ~ "1")) %>% 
  mutate(loan = case_when(loan == "no"~ "0",
                          TRUE ~ "1")) %>% 
  mutate(contact = case_when(contact == "telephone"~ "0",
                             TRUE ~ "1")) %>%
  mutate(month = case_when(month == "mar"~ "0",
                           month == "apr"~ "1",
                           month == "may"~ "2",
                           month == "jun"~ "3",
                           month == "jul"~ "4",
                           month == "aug"~ "5",
                           month == "sep"~ "6",
                           month == "oct"~ "7",
                           month == "nov"~ "8",
                               TRUE ~ "9")) %>%
  mutate(day_of_week = case_when(day_of_week == "mon"~ "0",
                                 day_of_week == "tue"~ "1",
                                 day_of_week == "wed"~ "2",
                                 day_of_week == "thu"~ "3",
                           TRUE ~ "4")) %>%
  mutate(poutcome = case_when(poutcome == "nonexistent"~ "0",
                              poutcome == "failure"~ "1",
                                 TRUE ~ "3")) %>%
  mutate(y = case_when(y == "no"~ "0",
                          TRUE ~ "1")) %>% 
  view()
## changing the factor variables to numeric so as to perfom correlation
bankk$job<- as.numeric(bankk$job)
bankk$marital<-as.numeric(bankk$marital)
bankk$education<-as.numeric(bankk$education)
bankk$housing<-as.numeric(bankk$housing)
bankk$loan<-as.numeric(bankk$loan)
bankk$contact<-as.numeric(bankk$contact)
bankk$month<-as.numeric(bankk$month)
bankk$day_of_week<-as.numeric(bankk$day_of_week)
bankk$poutcome<-as.numeric(bankk$poutcome)
bankk$y<-as.numeric(bankk$y)

## CORRELATION ANALYSIS
## Checking for the normality of the data before conducting correlation
bankk %>% 
  group_by(y) %>% 
  normality() %>% 
  mutate(across(is.numeric, ~round(., 3))) %>% 
  regulartable()

## Normalizing the data
normalize<- function(x) {
  return((x - min(x)) / (max(x) - min(x))) }
Bank<- as.data.frame(lapply(bankk[,1:20], normalize))
head(Bank)



#. 5 Correlation Analysis
round(cor(bankk),1)


ggcorrplot(cor(Bank), title = "CORRELOGRAM", 
           legend.title = "Pearson Correlation" , lab =  TRUE,
           lab_col = "black",
           lab_size = 3, ggtheme = theme_bw,
           outline.color = "black",
           colors = c("white", "blue", "darkred"))



# Creating models

## Random Forest

set.seed(3)
id<- sample(2,nrow(Bank),prob = c(0.7,0.3), replace = TRUE)
Bank_train<- Bank[id == 1,]
Bank_test<- Bank[id==2,]

Bank$y<- as.factor(Bank$y)

bestmtry<- tuneRF(Bank_train, Bank_train$y,stepFactor = 1.2, improve = 0.01, trace = T, plot = T)

Bank_train$y<- as.factor(Bank_train$y)
Bank_forest<- randomForest(y ~ ., data= Bank_train)
Bank_forest

importance(Bank_forest) ## to find no of significant values in the decision trees
varImpPlot(Bank_forest) ## visualizing of the above

## model validation
preBank<- predict(Bank_forest, newdata = Bank_test, type = "class")


confusionMatrix(table(preBank, Bank_test$y))


## LOGISTIC REGRESSION
set.seed(5)
split<- sample.split(bankk,SplitRatio = 0.8)
training<- subset(bankk,split == "TRUE")
testing<- subset(bankk,split== "FALSE")
model<- glm(y ~ ., training, family = "binomial")
summary(model)


### Checking for the non linearity by the generalized additive model
m1 <- mgcv:: gam(y ~ job + s(age) + marital + education + housing+loan+month+day_of_week+poutcome,training, family = binomial)
plot(m1)  ## The data seems to be non -linear

plot_model(model, type = "pred", terms = "age")  ## Plot predictions

## Checking for non-linearity
m2<- glm(y ~ poly(age, 2), data = training, family = binomial)
m3<- glm(y ~ poly(age, 3), data = training, family = binomial)
m4<- glm(y ~ poly(age, 4), data = training, family = binomial)

## Compare models with the lowwest AIC
AIC(model, m2,m3,m4)

## Another method of choosing the best model
tab_model(m4)


### Checking the models assumptions
check_model(m4)

## Plot predictions
plot_model(m4, type = "eff", terms = "age[all]")


## Plot particular predictions
b <- emmip(m4, ~ age, CIs = T, type = "response", at = list(age = c(1,25,53,73)))+
  scale_y_continuous(labels = scales::percent)
b ## Age therefore negatively impacts customer subscription of service(y)

## Getting the probabilities and the odds ratios
emmeans(m4,pairwise ~ age, type = "response",
        at = list(age = c(1,25,53,73)), infer = T)

## Extracting predictions
ggeffect(model)

## Displaty pairwise comparisons
tables<- tbl_regression(
  model,
  exponentiate = TRUE,
  add_pairwise_contrasts = TRUE,
  contrasts_adjust = "bonferroni",
  pairwise_reverse = FALSE,
  pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% 
  add_significance_stars(hide_p = F, hide_se = T,
                         hide_ci = F) %>% 
  bold_p()

tables

## Checking the perfomance of the model
performance(model)

interpret_r2(0.378) ## An R2 of 0.378 mean that our predictors explain 37.8% of the
# loan subscription of service and that's substantial

predictt<- predict(model, testing,y = "response")
table(ActualValue = testing$y, PredictedValue = predictt>0.5)


           
##
#### SUPPORT VECTOR MACHINES
set.seed(300)
Intrain <-createDataPartition(y = bankk$y, p = 0.8, list = F)
traini<- bankk[Intrain,]
testi<- bankk[-Intrain,]

traini[["y"]] = factor(traini[["y"]])
trctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear<- train(y ~., data = traini, method = "svmLinear",
                   trControl = trctrl,
                   preProcess = c("center","scale"), 
                   tuneLength = 10)

svm_Linear
test_pred<- predict(svm_Linear, newdata = testi)
confusionMatrix(table(test_pred, testi$y))

#### DECISION TREES
set.seed(147)
id<- sample(2,nrow(bankk),prob = c(0.8,0.2), replace = TRUE)
bankk_train<- bankk[id == 1,]
bankk_test<- bankk[id==2,]

## Building a classification tree
bank_tree<- rpart(y~., data = bankk_train, method = "class")
bank_tree
rpart.plot(bank_tree)
pred_bank<- predict(bank_tree, newdata = bankk_test, type = "class")
confusionMatrix(table(pred_bank, bankk_test$y))
