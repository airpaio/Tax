library(XLConnect)
library(ggplot2)
library(reshape2)
library(scales)
library(foreach)
library(doParallel)
library(geiger)
library(sp)
library(rgeos)
library(Bolstad2)
library(knitr)

setwd("~/Research/Tax/data")

df <- readWorksheetFromFile("clean2012.xls", 
                            sheet=1, 
                            startRow = 2,
                            endCol = 40)

df <- data.frame(df)
df <- df[-1,]



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

#print(bP)




# flat tax and income info
flatRate <- .13
income <- seq(0,150000,by=100)
flatTax <- flatRate*income



# standard deductions
single <- 5950
married <- 11900
hOfHouse <- 8700

# ssTax for 2012 was 4.2% with max earnings of 110100
# medCare tax had no max at 1.45%
ssRate <- .042
medRate <- .0145


incomeRate <- function(income, deduction, rate, exemptions, ssRate, medRate){
    (income - (exemptions*3800) - deduction)*rate + income*(ssRate+medRate)
}

brackets <- c(0, 8700, 35350, 71350, 108725, 194175)
brkRates <- c(.1, .15, .25, .28, .33, .35)

# get bases for the progressive tax structure
progTaxes <- sapply(1:length(brackets), function(r) brkRates[r]*(brackets[r+1]-brackets[r]))
progTaxes <- progTaxes[-length(progTaxes)]


# in parallel, get values of tax for each do;;ar of gross income
#run 'foreach' in parallel
c1 <- makeCluster(4)
registerDoParallel(c1)
b1 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 0 & i <= 8700) %dopar% incomeRate(i, single, rate=.1, exemptions=1,
                                                ssRate=.042, medRate=.0145)
b2 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 8701 & i <= 35350) %dopar% (progTaxes[1] + incomeRate(i-8701, single, 
                                                                    rate=.15, exemptions=1,
                                                                    ssRate=.042, medRate=.0145))
b3 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 35351 & i <= 71350) %dopar% (sum(progTaxes[1:2]) + incomeRate(i-35351, single,
                                                                            rate=.25, exemptions=1,
                                                                            ssRate=.042, medRate=.0145))
b4 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 71351 & i <= 108725) %dopar% (sum(progTaxes[1:3]) + incomeRate(i-71351, single,
                                                                             rate=.28,
                                                                             exemptions=1,
                                                                             ssRate=.042,
                                                                             medRate=.0145))
b5 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 108726 & i <= 110100) %dopar% (sum(progTaxes[1:4]) + incomeRate(i-108726, single,
                                                                              rate=.33,
                                                                              exemptions=1,
                                                                              ssRate=.042,
                                                                              medRate=.0145))
b5.1 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 110101 & i <= 194175) %dopar% (sum(progTaxes[1:4]) + incomeRate(i-108726, single,
                                                                              rate=.33,
                                                                              exemptions=1,
                                                                              ssRate=0,
                                                                              medRate=.0145))
b6 <- foreach (i = iter(income), .combine=c, .multicombine=TRUE) %:%
    when(i >= 194176) %dopar% (sum(progTaxes[1:5]) + incomeRate(i-194176, single,
                                                                rate=.33,
                                                                exemptions=1,
                                                                ssRate=0,
                                                                medRate=.0145))
iRate <- c(b1,b2,b3,b4,b5,b5.1,b6)


# pTax = progressive tax bracket structure
pTax <- ifelse(iRate <= 0, 0, iRate)
dfTFT <- data.frame(income, flatTax, pTax)




# line plot of progressive tax structure vs flat tax
linePlot <- ggplot(dfTFT, aes(x=dfTFT$income)) +
    geom_line(aes(x=dfTFT$income, y=dfTFT$flatTax, color="13% Flat Tax")) +
    geom_line(aes(x=dfTFT$income, y=dfTFT$pTax, color="Tax Bracket Model")) +
    scale_color_discrete(name = "Tax Models",
                         labels = c("13% Flat Tax", "Tax Bracket Model")) +
    ylab("Tax (dollars)") +
    xlab("Income (dollars)") +
    ggtitle("13% Flat Tax vs Progressive Tax Bracket Model \n
            Model Assumes Year 2012 Single Person Claiming One
            Exemption and Social Security/Medicare Tax") +
    theme(axis.text.x=element_text(size=13),
          axis.text.y=element_text(size=13),
          axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          plot.title=element_text(size=18))

print(linePlot)



##### Get the Flat tax Revenue for 13%
df$flatTaxRev <- .13*df$AGIlessDeficit

# upper range of flat tax revenue for incomes < 50000
upperFlatTaxRev <- sum(df$flatTaxRev[2:9])

# lower range of flat tax revenue for incomes < 50000
lowerFlatTaxRev <- sum(df$flatTaxRev[2:8])

# average flat tax per person
avgFlatTax <- upperFlatTaxRev/(sum(df$grossNumberOfReturns[2:9]))
df$avgFlatTaxPerPerson <- df$flatTaxRev/df$grossNumberOfReturns


##### create 'dodged' barplot of tax revenues for each income range for
## bot the flat tax and the 2012 progressive tax revenues
dfRevs <- melt(data.frame("Progressive Tax"=df$tax[2:length(df$tax)],
                          "13 Percent Flat Tax"=df$flatTaxRev[2:length(df$flatTaxRev)],
                          Income=df$AGI[2:length(df$AGI)]),
               variable.name="Model")


taxRevPlot <- ggplot(dfRevs, aes(Income, value, fill=Model)) +
    geom_bar(position="dodge", stat="identity") +
    ylab("Tax Revenue") +
    ggtitle("Actual 2012 Progressive Tax vs. 13% True Flat Tax") +
    scale_x_discrete(name="AGI Level", labels=df$AGI[2:length(df$AGI)],
                     limits=df$AGI[2:length(df$AGI)]) +
    scale_y_continuous(labels=dollar) +
    theme(axis.text.x=element_text(angle=90, size=13),
          axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          plot.title=element_text(size=18))


print(taxRevPlot)