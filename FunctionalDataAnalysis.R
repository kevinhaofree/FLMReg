library(XLConnect)
library(fdapace)
wb = loadWorkbook('Flower2015-2016.xls')
flower = readWorksheet(wb, 1)

##Generate X and Y
# X is the swp value varying by time
# Y is the pf value varying by time
SWPAnalysis = flower[!is.na(flower$SWP1_bar),]
pfAnalysis = flower[!is.na(flower$pf),]

# classify pf by ID
pfByID = as.list( by(pfAnalysis$pf,pfAnalysis$TreeID,as.vector) )
# The missing ID and useful ID, maybe used later
CommonID = intersect(SWPAnalysis$TreeID,pfAnalysis$TreeID)
MissingID = setdiff(SWPAnalysis$TreeID,CommonID)

## Here is one controversial thing: since the pf value for missing ID is unavailable
## 1) Should we use whole data that are available to us for functional regression? 
## 2) Or should we just use those that pf-values that are exists?
##############
# following lines I choose 1) for functional regression because every points are valuable
# But we can do the other version if that is more convicible

# dealing with the date as timepoint
Time = as.Date(as.character.Date(SWPAnalysis$date))
Days = Time - Time[1]

# classify swp-value by tree ID
SWPByID = as.list( by(SWPAnalysis$SWP1_bar,SWPAnalysis$TreeID,as.vector) )

#classify the timepoint by tree ID
timePoint = as.list(by(Days,SWPAnalysis$TreeID,as.vector))

#functional regression
## there are many options we can try like AIC, BIC ; choosing some different kernel for smoothing ,etc
FReg = FPCA(SWPByID,timePoint,optns = list (dataType = 'Sparse',diagnosticsPlot = TRUE,methodSelectK = 'FVE'))
plot(FReg$workGrid,FReg$mu,main = 'Mean Function',xlab = 'Days',ylab = ' ',type = 'l')
Cumulate = c(0, cumsum(FReg$lambda) )
plot(0:5,Cumulate/Cumulate[5],xlab = 'Number of Components',ylab = 'Fraction of Variance Explained',main = 'Screeplot',type = 'b')


# Prepare for doing a regression
GetLast<-function(t) t[length(t)]
Y_final = as.vector( unlist (lapply(pfByID,GetLast)))
#  get the period time for another response
#############(need to be done)###########

## naive regression
## basically just take the scores as X and Y_final as Y and doing a linear regression
## same procedure used for Y_peorid
Y_final = Y_final- mean(Y_final)
NaiveRes<-lm(Y_final~FReg$xiEst[CommonID,1:2]+0)



# get the naive beta function, maybe we can say something about this?
beta_naive = FReg$phi[,1:2]  %*% NaiveRes$coefficients
plot(FReg$workGrid,beta_naive,type = 'l')







