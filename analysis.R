library("readxl")
set.seed(100)
data <- read_excel("true_pairs.xlsx", col_types='guess')
data$Difference = data$`Yield ask, at issue_SLB` - data$`Yield ask, yas_CB`

stay <- c("Issue Date_SLB","Maturity_SLB","Amt Issued_SLB","Cntry of Incorp_SLB",
          "Cpn_SLB","Mty Type_SLB","Payment Rank_SLB",
          "Coupon Type_SLB","BICS Level 1_SLB",
          "Step up desc_SLB","Step up_SLB","Step down_SLB","Step-up&down?_SLB","SPT _SLB",
          "Yield ask, at issue_SLB","Issue Date_CB", "Maturity_CB",
          "Amt Issued_CB","Cpn_CB",
          "Yield ask, yas_CB","Difference")

data = data[,(names(data) %in% stay)]

slb=c(data$'Yield ask, at issue_SLB')
cb=c(data$'Yield ask, yas_CB')
d=c(data$Difference)

summary(slb)
summary(cb)
summary(d)
boxplot(slb,cb,xlab='Bond type',ylab='Yield at issue',names=c('SLB','CB'))

hist(slb,freq=FALSE, main='Histogram of SLB Yield at issue',xlab='Yield at issue')
hist(cb, freq=FALSE, main='Histogram of CB Yield at issue',xlab='Yield at issue')

qqnorm(slb, main='SLB')
qqline(slb)

qqnorm(cb, main='CB')
qqline(cb)

qqnorm(d, main='Yield difference')
qqline(d)

shapiro.test(slb)
shapiro.test(cb)
shapiro.test(d) # normally distributed

ks.test(slb,'pnorm')
ks.test(cb,'pnorm')
ks.test(d,'pnorm') # not normally distributed

# Rank sum
wilcox.test(slb,cb,paired=FALSE)
wilcox.test(slb,cb,paired=FALSE,alternative='less')

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

#----
library(lubridate)
library(rethinking)

new_df$cntry_id <- as.integer(new_df$`Cntry of Incorp_SLB`)
new_df$sector_id <- as.integer(new_df$`BICS Level 1_SLB`)
new_df$mty_type_id <- as.integer(new_df$`Mty Type_SLB`)
new_df$seniority_id <- as.integer(new_df$`Payment Rank_SLB`)
new_df$cpn_type_id <- as.integer(new_df$`Coupon Type_SLB`)
new_df$iss_diff <- time_length(difftime(new_df$`Issue Date_SLB`, new_df$`Issue Date_CB`),"years")
new_df$mty_diff <- time_length(difftime(new_df$`Maturity_SLB`, new_df$`Maturity_CB`),"years")
new_df$size_diff <- new_df$`Amt Issued_SLB` / new_df$`Amt Issued_CB`


model <- quap(
  flist = alist(Difference ~ dnorm(mu, sigma),
                mu <- c[cntry_id] + s[sector_id] + m[mty_type_id] +
                  sn[seniority_id] + cp[cpn_type_id] + iss_diff * t1 +
                  mty_diff * t2 + size_diff * t3,
                c[cntry_id] ~ dnorm(0,1),
                s[sector_id] ~ dnorm(0,1),
                m[mty_type_id] ~ dnorm(0,1),
                sn[seniority_id] ~ dnorm(0,1),
                cp[cpn_type_id] ~ dnorm(0,1),
                t1 ~ dnorm(0,1),
                t2 ~ dnorm(0,1),
                t3 ~ dnorm(0,1),
                sigma ~dunif(0,50)
  ),
  data = new_df)

# in table form
precis(model, prob=0.95, depth=2) # also gives values at edges of 95%

# calculate correlation between variables
cov2cor(vcov(model))

#------- lm
conts <- model.matrix(~.-1, data = new_df[, c("Cntry of Incorp_SLB", "BICS Level 1_SLB","Mty Type_SLB","Coupon Type_SLB")],
                       contrasts.arg = list(
                         `Cntry of Incorp_SLB` = contrasts(new_df$"Cntry of Incorp_SLB", contrasts = FALSE),
                         `BICS Level 1_SLB` = contrasts(new_df$"BICS Level 1_SLB", contrasts = FALSE),
                         `Mty Type_SLB` = contrasts(new_df$"Mty Type_SLB", contrasts = FALSE),
                         `Coupon Type_SLB` = contrasts(new_df$"Coupon Type_SLB", contrasts = FALSE)
                       ))

model2 <- lm(Difference ~ `Cntry of Incorp_SLB` + `BICS Level 1_SLB` +
               `Mty Type_SLB` + `Coupon Type_SLB` + iss_diff  + mty_diff +
               size_diff, data=new_df)
anova(model2)
summary(model2)
