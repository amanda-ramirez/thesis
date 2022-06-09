library("readxl")
library(lubridate)
library(ggplot2)
library(grid)
library(gridBase)
library(dplyr)
library(car)

set.seed(100)
data <- read_excel("Descriptive Statistics.xlsx", col_types='guess', sheet=2)
data$`Issue Date` <- as.Date(data$`Issue Date`)

growth = data[order(data$`Issue Date`, decreasing=FALSE),]
growth$count =1
growth$`Cumulative Count` = cumsum(growth$count)

plot(`Cumulative Count` ~ `Issue Date`, growth, xaxt = "n", type = "l" ,main="SLB Cumulative Issuances Over Time")
axis(1, growth$`Issue Date`, format(growth$`Issue Date`, "%b %y"), cex.axis = .7)


new_df = data[data$`Yield ask, at issue`!="#N/A N/A",]
new_df$`Yield ask, at issue` <- as.numeric(new_df$`Yield ask, at issue`)
new_df <- new_df[complete.cases(new_df$`Yield ask, at issue`),]


new_df$"BICS Level 1"<- factor(new_df$"BICS Level 1", exclude=NULL)
new_df$"ESG Rating"<- factor(new_df$"ESG Rating", exclude=NULL)

indu <- new_df %>% group_by(`BICS Level 1`)
indu <- indu  %>% summarise(count = n(), amt = sum(`Amt Issued`), yield = mean(`Yield ask, at issue`))
indu <- indu[order(indu$amt, decreasing=TRUE),]

esg <- new_df %>% group_by(`ESG Rating`)
esg <- esg  %>% summarise(count = n(), amt = sum(`Amt Issued`), yield = mean(`Yield ask, at issue`))
esg <- esg[order(indu$amt, decreasing=TRUE),]

