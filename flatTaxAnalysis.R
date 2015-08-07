library(XLConnect)
library(ggplot2)
library(reshape2)
library(scales)
library(foreach)
library(doParallel)

setwd("~/Research/Tax/data")

df <- readWorksheetFromFile("clean2012.xls", 
                            sheet=1, 
                            startRow = 2,
                            endCol = 6)

df <- data.frame(df)
df <- df[-1,]
#df$AGIid <- c(1:19)


barPlot <- function(dat, xvar, yvar, xlabel, ylabel,
                    xtic=df$ID,
                    title){
    p <- ggplot(data=dat, aes(x=xtic, y=yvar), environment=environment()) +
        geom_bar(stat="identity") +
        ylab(ylabel) +
        ggtitle(title) +
        scale_x_discrete(name=xlabel, labels=xvar, limits=xvar) +
        scale_y_continuous(labels=dollar) +
        theme(axis.text.x=element_text(angle=90, size=13),
              axis.title.x=element_text(size=15),
              axis.title.y=element_text(size=15),
              plot.title=element_text(size=18))
    
    return(p)
}

bP <- barPlot(dat=df, xvar=df$AGI, yvar=df$taxableIncome, xlabel="AGI Level", 
              ylabel="Taxable Income",
              title="Total Taxable Income of Individuals in 2012")

print(bP)





flatRate <- .13
income <- seq(1,200000,by=400)
flatTax <- flatRate*income
dfTFT <- data.frame(income, flatTax)

#iRate <- numeric(length(income))
single <- 5950
married <- 11900
hOfHouse <- 8700

# ssTax for 2012 was 4.2% with max earnings of 110100
# medCare tax had no max at 1.45%
ssRate <- .042
medRate <- .0145

#run 'foreach' in parallel
c1 <- makeCluster(6)
registerDoParallel(c1)
#ssMedTaxLow <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
#    when(i <= 110100) %dopar% i*(ssRate+medRate)
#ssMedTaxHi <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
#    when(i > 110100) %dopar% i*medRate
#ssMedTax <- c(ssMedTaxLow, ssMedTaxHi)

incomeRate <- function(income, deduction, rate, exemptions, ssRate, medRate){
    ((income - (exemptions*3800))*rate) - deduction + income*(ssRate+medRate)
    }



b1 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 0 & i <= 8700) %dopar% incomeRate(i, single, rate=.1, exemptions=1,
                                                ssRate=.042, medRate=.0145)
b2 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 8701 & i <= 35350) %dopar% incomeRate(i, single, rate=.15, exemptions=1,
                                                    ssRate=.042, medRate=.0145)
b3 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 35351 & i <= 71350) %dopar% incomeRate(i, single, rate=.25, exemptions=1,
                                                     ssRate=.042, medRate=.0145)
b4 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 71351 & i <= 108725) %dopar% incomeRate(i, single, rate=.28, exemptions=1,
                                                      ssRate=.042, medRate=.0145)
b5 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 108726 & i <= 110100) %dopar% incomeRate(i, single, rate=.33, exemptions=1,
                                                       ssRate=.042, medRate=.0145)
b5.1 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 110101 & i <= 194175) %dopar% incomeRate(i, single, rate=.33, exemptions=1,
                                                       ssRate=0, medRate=.0145)
b6 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 194176) %dopar% incomeRate(i, single, rate=.35, exemptions=1,
                                         ssRate=0, medRate=.0145)
iRate <- c(b1,b2,b3,b4,b5,b5.1,b6)


# pTax = progressive tax bracket structure
pTax <- ifelse(iRate <= 0, 0, iRate)
dfTFT$pTax <- pTax

p <- ggplot(dfTFT) +
    geom_line(aes(x=dfTFT$income, y=dfTFT$flatTax, color="13% Flat Tax")) +
    geom_line(aes(x=dfTFT$income, y=dfTFT$pTax, color="Tax Bracket Model")) +
    ylab("Tax (dollars)") +
    xlab("Income (dollars)") +
    ggtitle("13% Flat Tax vs Progressive Tax Bracket Model \n
            Model Assumes Year 2012 Single Person Claiming One
Exemption and Social Security/Medicare Tax") +
    theme(axis.text.x=element_text(size=13),
          axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          plot.title=element_text(size=18))


print(p)