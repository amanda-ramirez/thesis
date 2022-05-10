library("readxl")
library(lubridate)
library(ggplot)

set.seed(100)
data <- read_excel("true_pairs.xlsx", col_types='guess')
data$Difference = data$`Yield ask, at issue_SLB` - data$`Yield ask, yas_CB`

stay <- c("Issuer Name_SLB","Issue Date_SLB","Maturity_SLB","Amt Issued_SLB","Cntry of Incorp_SLB",
          "Cpn_SLB","Mty Type_SLB","Payment Rank_SLB", "ESG Rating_SLB","BBG Composite_SLB",
          "Coupon Type_SLB","BICS Level 1_SLB",
          "Step up desc_SLB","Step up_SLB","Step down_SLB","Step-up&down?_SLB","SPT _SLB",
          "Yield ask, at issue_SLB","Issue Date_CB", "Maturity_CB",
          "Amt Issued_CB","Cpn_CB",
          "Yield ask, yas_CB","Difference")

data = data[,(names(data) %in% stay)]
data$`BBG Composite_SLB`<-ifelse(is.na(data$`BBG Composite_SLB`),"NR",data$`BBG Composite_SLB`)

slb=c(data$'Yield ask, at issue_SLB')
cb=c(data$'Yield ask, yas_CB')
d=c(data$Difference)

summary(slb)
summary(cb)
summary(d)
boxplot(slb,cb,xlab='Bond type',ylab='Yield at issue',names=c('SLB','CB'))

hist(slb,freq=FALSE, main='Histogram of SLB Yield at Issue',xlab='Yield at Issue')
hist(cb, freq=FALSE, main='Histogram of CB Yield at Issue',xlab='Yield at Issue')
hist(d, freq=FALSE, main='Histogram of Yield Difference',xlab='Yield at Issue')

qqnorm(slb, main='QQ Plot for SLB Yield at Issue')
qqline(slb)

qqnorm(cb, main='QQ Plot for CB Yield at Issue')
qqline(cb)

qqnorm(d, main='QQ Plot for Yield Difference')
qqline(d)

shapiro.test(slb)
shapiro.test(cb)
shapiro.test(d) # normally distributed

# Signed rank
wilcox.test(slb,cb,paired=TRUE)
wilcox.test(slb,cb,paired=TRUE,alternative='less')

##--------------------

#--- One-hot encoding
new_df <- data
new_df$"Cntry of Incorp_SLB" <- factor(new_df$"Cntry of Incorp_SLB", exclude=NULL)
new_df$"BICS Level 1_SLB"<- factor(new_df$"BICS Level 1_SLB", exclude=NULL)
new_df$`Mty Type_SLB` <- factor(new_df$`Mty Type_SLB`, exclude=NULL)
new_df$`Payment Rank_SLB` <- factor(new_df$`Payment Rank_SLB`, exclude=NULL)
new_df$`Coupon Type_SLB` <- factor(new_df$`Coupon Type_SLB`, exclude=NULL)
new_df$`BBG Composite_SLB` <- factor(new_df$`BBG Composite_SLB`, exclude=NULL)
new_df$iss_diff <- time_length(difftime(new_df$`Issue Date_SLB`, new_df$`Issue Date_CB`),"years")
new_df$mty_diff <- time_length(difftime(new_df$`Maturity_SLB`, new_df$`Maturity_CB`),"years")
new_df$size_diff <- new_df$`Amt Issued_SLB` / new_df$`Amt Issued_CB`


#------- lm

model <- lm(Difference ~ `Cntry of Incorp_SLB` + `BICS Level 1_SLB` +
               `Mty Type_SLB` + `Coupon Type_SLB` + `BBG Composite_SLB`+ iss_diff  + mty_diff +
               size_diff, data=new_df)
anova(model)
summary(model)
plot(residuals(model)) # plot each variable against residuals
hist(residuals(model),freq=FALSE) # no systematic trend
qqnorm(residuals(model))
qqline(residuals(model)) # different types of data?

#--- explanatory 


table(new_df$`Cntry of Incorp_SLB`)
table(new_df$`BICS Level 1_SLB`)
table(new_df$`Mty Type_SLB`)
table(new_df$`Coupon Type_SLB`)
table(new_df$`BBG Composite_SLB`)
plot(new_df$iss_diff)
plot(new_df$mty_diff)
plot(new_df$size_diff)
plot(table(new_df$`Issuer Name_SLB`))



