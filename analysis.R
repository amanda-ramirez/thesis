library("readxl")
library(lubridate)
library(ggplot2)
library(grid)
library(gridBase)
library(dplyr)
library(car)

set.seed(100)
data <- read_excel("true_pairs.xlsx", col_types='guess')
data$Difference = data$`Yield ask, at issue_SLB` - data$`Yield ask, yas_CB`

stay <- c("Issuer Name_SLB","Issue Date_SLB","Maturity_SLB","Amt Issued_SLB","Cntry of Incorp_SLB",
          "Cpn_SLB","Mty Type_SLB","Payment Rank_SLB", "ESG Rating_SLB","BBG Composite_SLB",
          "Coupon Type_SLB","BICS Level 1_SLB", "SPT _SLB","Step up_SLB",
          "Step up desc_SLB","Step up_SLB","SPT _SLB",
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

png('boxplot.png')
boxplot(slb,cb,xlab='Bond type',ylab='Yield at issue',names=c('SLB','CB'),main="Boxplot of Yields")
dev.off()


png('hist_slb.png')
hist(slb,freq=FALSE, main='Histogram of SLB Yield at Issue',xlab='Yield at Issue')
dev.off()


png('hist_slb.png')
hist(cb, freq=FALSE, main='Histogram of CB Yield at Issue',xlab='Yield at Issue')
dev.off()

png('hist_slb.png')
hist(d, freq=FALSE, main='Histogram of Yield Difference',xlab='Yield at Issue')
dev.off()

png('qq_slb.png')
qqnorm(slb, main='QQ Plot for SLB Yield at Issue')
qqline(slb)
dev.off()

png('qq_cb.png')
qqnorm(cb, main='QQ Plot for CB Yield at Issue')
qqline(cb)
dev.off()

png('qq_diff.png')
qqnorm(d, main='QQ Plot for Yield Difference')
qqline(d)
dev.off()


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
new_df$`ESG Rating_SLB` <- factor(new_df$`ESG Rating_SLB`, exclude=NULL)
new_df$`SPT _SLB` <- factor(new_df$`SPT _SLB`, exclude=NULL)
new_df$iss_diff <- time_length(difftime(new_df$`Issue Date_SLB`, new_df$`Issue Date_CB`),"years")
new_df$mty_diff <- time_length(difftime(new_df$`Maturity_SLB`, new_df$`Maturity_CB`),"years")
new_df$size_diff <- new_df$`Amt Issued_SLB` / new_df$`Amt Issued_CB`
new_df$mty_SLB <- time_length(difftime(new_df$`Maturity_SLB`, new_df$`Issue Date_SLB`),"years")
new_df$mty_CB <- time_length(difftime(new_df$`Maturity_CB`, new_df$`Issue Date_CB`),"years")

full_df <- new_df %>% select('Cntry of Incorp_SLB','BICS Level 1_SLB','Mty Type_SLB','Payment Rank_SLB', 
             'Coupon Type_SLB', 'BBG Composite_SLB','SPT _SLB','Step up_SLB','iss_diff','mty_diff','size_diff',
             'Amt Issued_SLB','mty_SLB','mty_CB','Difference','Yield ask, at issue_SLB','Yield ask, yas_CB')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



num_df <- new_df %>% select('Step up_SLB','iss_diff','mty_diff','size_diff',
'Amt Issued_SLB','Amt Issued_CB', 'mty_SLB','mty_CB','Difference','Yield ask, at issue_SLB','Yield ask, yas_CB','Cpn_SLB','Cpn_CB')


vtable:: sumtable(round(num_df,3))
#------- lm

model <- lm(Difference ~ `Cntry of Incorp_SLB` + `BICS Level 1_SLB` +
               `Mty Type_SLB` + `Coupon Type_SLB` + `BBG Composite_SLB`+ +`ESG Rating_SLB`+`SPT _SLB`+
              `Step up_SLB` + iss_diff  + mty_diff +size_diff, data=new_df)
anova(model)
summary(model)
plot(residuals(model)) # plot each variable against residuals
hist(residuals(model),freq=FALSE) # no systematic trend
qqnorm(residuals(model))
qqline(residuals(model)) # different types of data?


plot(model)



#--- explanatory 


##--- country
cntry = data.frame(table(new_df$`Cntry of Incorp_SLB`))
cntry = cntry[order(cntry$Freq,decreasing=TRUE),]

png('cntrybar.png')
x <- barplot(cntry$Freq,names=cntry$Var1,main='Bond Pairs by Country of Incorporation', ylab='Count',xlab='Country',ylim=c(0,12))
y <-as.matrix(cntry$Freq)
text(x,y+0.5,labels=as.character(y))
dev.off()


##--- industry

png('indbar.png')
ind = data.frame(table(new_df$`BICS Level 1_SLB`))
ind = ind[order(ind$Freq,decreasing=TRUE),]

par(mar=c(8,4,4,4))
i <- barplot(ind$Freq,names.arg="",main='Bond Pairs by Sector', ylab='Count',ylim=c(0,12))
mtext(text = "Sector",
      side = 1,#side 1 = bottom
      line = 7)

yi <- as.matrix(ind$Freq)
text(i,yi+0.5,labels=as.character(yi))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(ind$Var1,
          x = unit(i, "native"), y=unit(-1, "lines"),
          just="right", rot=50, gp=gpar(fontsize=10))

popViewport(3)

dev.off()
##--- 

table(new_df$`BICS Level 1_SLB`)
table(new_df$`Mty Type_SLB`)
table(new_df$`Coupon Type_SLB`)
table(new_df$`BBG Composite_SLB`)
plot(new_df$iss_diff)
plot(new_df$mty_diff)
plot(new_df$size_diff)
plot(table(new_df$`Issuer Name_SLB`))


#---

summ <- new_df %>% summarise(count=n(),amt = sum(`Amt Issued_SLB`), slb=mean(`Yield ask, at issue_SLB`),
                             cb = mean(`Yield ask, yas_CB`), dif = mean(Difference), cpn = mean(Cpn_SLB),
                             step = mean(`Step up_SLB`), issdtdiff = mean(iss_diff), mtydiff = mean(mty_diff),
                             sizediff = mean(size_diff))

country <- new_df %>% group_by(`Cntry of Incorp_SLB`)
country <- country  %>% summarise(count = n(), amt = sum(`Amt Issued_SLB`), yield = mean(Difference))
country <- country[order(country$amt, decreasing=TRUE),]

bics <- new_df %>% group_by(`BICS Level 1_SLB`)
bics <- bics  %>% summarise(count = n(), amt = sum(`Amt Issued_SLB`),yield =mean(Difference))
bics <- bics[order(bics$amt, decreasing=TRUE),]

bbg <- new_df %>% group_by(`BBG Composite_SLB`)
bbg <- bbg  %>% summarise(count = n(), amt = sum(`Amt Issued_SLB`),yield =mean(Difference))
bbg <- bbg[order(bbg$amt, decreasing=TRUE),]

mty <- new_df %>% group_by(`Mty Type_SLB`)
mty <- mty  %>% summarise(count = n(), amt = sum(`Amt Issued_SLB`),yield =mean(Difference))
mty <- mty[order(mty$amt, decreasing=TRUE),]

cpn <- new_df %>% group_by(`Coupon Type_SLB`)
cpn <- cpn  %>% summarise(count = n(), amt = sum(`Amt Issued_SLB`),yield =mean(Difference))
cpn <- cpn[order(cpn$amt, decreasing=TRUE),]

spt <- new_df %>% group_by(`SPT _SLB`)
spt <- spt  %>% summarise(count = n(), amt = sum(`Amt Issued_SLB`),yield =mean(Difference))
spt <- spt[order(spt$amt, decreasing=TRUE),]

esg <- new_df %>% group_by(`ESG Rating_SLB`)
esg <- esg  %>% summarise(count = n(), amt = sum(`Amt Issued_SLB`),yield =mean(Difference))
esg <- esg[order(spt$amt, decreasing=TRUE),]

