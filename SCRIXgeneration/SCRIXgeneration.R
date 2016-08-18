## set up basics
# clear workspace
rm(list=ls(all=TRUE))

# set working directory
setwd("C:/Users/user/Desktop/Q Kolleg/smart beta/SmartIndexData/")

# load library
library("zoo") 
library("fGarch")
library("Rsolnp")
library("PerformanceAnalytics")

# load data
load("market.RData")
load("price.RData")
load("volume.RData")
load("crix.RData")
load("crix_numb_const.RData")
load("cryptos.RData")
load("tmi.RData")

# import function to calculate VaR and ES
source("VaR_ES2.r")

# identify the column position of the cryptos in the matrix of data
cryptos_pos = list()
for (i in 1:length( crix_numb_const ) ){
  for (j in 1:3){
    cryptos_pos[[3*(i-1) + j]] = which( 
      colnames( market ) %in% 
        cryptos[[ 3*(i - 1) + j ]][ 1:crix_numb_const[i] ] )
  }
}

# set rolling window of 100 days
roll = 100

# starting time point for first date of the crix data
date   = rownames( market )
tstart = which( date == names(crix[1]) )
tend   = length( date )
nt     = tend - tstart + 1

# clean and prepare data 
# trim data to have same data period as CRIX
marketTrim = market[ (tstart - roll):tend, ] 
priceTrim  = price[ (tstart - roll):tend, ]
volumeTrim = volume[ (tstart - roll):tend, ]
returnTrim  = price[ (tstart - roll - 1):tend, ]

# use last observation carried forward to deal with NA
# do not remove leading NA
marketTrim = na.locf( marketTrim, F )
priceTrim  = na.locf( priceTrim, F )
volumeTrim = na.locf( volumeTrim, F )
returnTrim = na.locf( returnTrim, F )
dateTrim   = rownames( volumeTrim )

# use next observation carried backward for leading NA
marketTrim = na.locf( marketTrim, T, T )
priceTrim  = na.locf( priceTrim, T, T )
volumeTrim = na.locf( volumeTrim, T, T )
returnTrim = na.locf( returnTrim, T, T )

# calculate log returns
returnTrim = diff( log(returnTrim) )

# index for date where constituents changes
# 01 of every month, every 3 months the number of constituents changes
dateShort   = substr(dateTrim, nchar(dateTrim)-1, nchar(dateTrim))
month1Index = which( dateShort == "01" )

# vector to store scrix
ttend      = length(crix)
scrix   = numeric( ttend )

# calculate quantities to assign weights 
# create a list to store
templist = list()

# num of index prior to starting point of crix
tempInd  = max( which( month1Index <= roll ))  

# for loop to calculate quantities used to assign weights
for (i in 1:( length(month1Index) - tempInd ) ){
  tempN         = length( cryptos_pos[[i]]  ) 
  templist[[i]] = numeric( tempN )
  k             = i + tempInd
  #################### returns for sharpe ratio #####################
  tempreturn    = numeric( tempN )
  tempreturn    = returnTrim[(month1Index[k] - 1),cryptos_pos[[i]]]
  ###################################################################
  for (j in 1:tempN){
    # using returns for VaR and ES #############################
    tempEst = VAR.ES.EVT2(returnTrim[ 
      (month1Index[k] - roll + 1):(month1Index[k] - 1),
      cryptos_pos[[i]][j] ],0.05 ) 
    
    # 2 for ES; 1 for VaR
    templist[[i]][j] = tempEst[2] 
    ############################################################
    
#     # sharpe ratio, tempEst 1 for VaR; 2 for ES ################
#     tempEst     = VAR.ES.EVT2(priceTrim[ 
#       (month1Index[k] - roll + 1):(month1Index[k] - 1),
#       cryptos_pos[[i]][j] ],0.05 ) 
#     templist[[i]][j] = tempreturn[j] / (tempEst[2]+ 0.000001) 
#     ############################################################
    
#     # equal weight ############################################
#     templist[[i]][j] = 1/tempN 
#     ###########################################################
    
#     # using CF for VaR and ES #####################################################
#     # using returns for VaR
#     # tempEst = VaR(returnTrim[ (month1Index[k] - roll + 1):(month1Index[k] - 1),
#                               cryptos_pos[[i]][j] ],0.95, method="modified" ) 
#     # using returns for ES
#     tempEst = ES(returnTrim[ (month1Index[k] - roll + 1):(month1Index[k] - 1),
#                              cryptos_pos[[i]][j] ],0.95, method="modified" ) 
#     if(is.na(tempEst)){tempEst=quantile(returnTrim[ 
#       (month1Index[k] - roll + 1):(month1Index[k] - 1),cryptos_pos[[i]][j] ],0.05)}
#     templist[[i]][j] = tempEst
#     ###############################################################################
    
#     # using CF for sharpe ratio #####################################################
#     # using VaR in SR
#     #tempEst = SharpeRatio.modified(returnTrim[ 
#     #  (month1Index[k] - roll + 1):(month1Index[k] - 1),cryptos_pos[[i]][j] ], 
#     #  Rf = 0, p = 0.95, FUN = "VaR")
#     # using ES in SR
#     tempEst = SharpeRatio.modified(returnTrim[ 
#       (month1Index[k] - roll + 1):(month1Index[k] - 1),cryptos_pos[[i]][j] ], 
#       Rf = 0, p = 0.95, FUN = "ES")
#     if(is.na(tempEst)){
#       tempDen = abs(quantile(returnTrim[ 
#         (month1Index[k] - roll + 1):(month1Index[k] - 1),cryptos_pos[[i]][j] ],
#         0.05))
#       if(tempDen==0){
#         tempEst=mean(returnTrim[ 
#           (month1Index[k] - roll + 1):(month1Index[k] - 1),cryptos_pos[[i]][j] ])
#       }else{
#         tempEst=mean(returnTrim[ 
#           (month1Index[k] - roll + 1):(month1Index[k] - 1),
#           cryptos_pos[[i]][j] ])/tempDen
#       }}
#     templist[[i]][j] = tempEst
#     ##############################################################################
  }
}

# weights of the cryptos using Markowitz portfolio (min variance portfolio)
# weight = list()
# for (i in 1:( length(month1Index) - tempInd ) ){
#   k   = i + tempInd
#   min_func = function(x){
#     last_index_period_time = priceTrim[ 
#       (month1Index[k] - roll + 1):(month1Index[k] - 1),
#       cryptos_pos[[i]] ]
#     a = apply(sweep(last_index_period_time,MARGIN=2,x,`*`), 1, sum)
#     b = volatility(garchFit(~garch(1,1), data = diff(log(a)), trace = F))
#     sum(b)
#   }
#   
#   min_equal = function(x){
#     sum(x)
#   }
#   start_weight = rep(1/(length(cryptos_pos[[i]])), length(cryptos_pos[[i]]))
#   tempweight  = solnp(pars = start_weight, fun = min_func, eqfun = min_equal, 
#                       eqB = 1, control=list(delta=1e-5), 
#                       LB = rep(0, length(cryptos_pos[[i]])), 
#                       UB = rep(1, length(cryptos_pos[[i]])))
#   weight[[i]] = tempweight$pars
# }


# # weights of the cryptos using equal weight
# weight = list()
# for (i in 1:length(templist)){
#   weight[[i]] = templist[[i]]
# }


# # weights of the cryptos using VaR and ES using price
# weight = list()
# for (i in 1:length(templist)){
#   # use inverse, then take proportion
#   tempW       = 1/(templist[[i]]+0.000001) 
#   test        = tempW / sum( tempW )
#   weight[[i]]    = test
# }

# weights of the cryptos using VaR and ES using returns
weight = list()
for (i in 1:length(templist)){
  tempW   = templist[[i]] + 2*abs(min(templist[[i]])) + 0.0000001
  test    = tempW / sum( tempW )
  weight[[i]]      = test
}

# # weights of the cryptos using SR with VaR or ES as volatility
# # measured by price
# weight = list()
# for (i in 1:length(templist)){
#   tempW          = templist[[i]]
#   tempW          = tempW + 2*abs(min(tempW)) +0.000001
#   test           = tempW / sum( tempW )
#   weight[[i]] = test
# }

# starting index for scrix
scrixtstart = month1Index[tempInd + 1]

# # calculate old amount for CRIX
# index_old_amount = list()
# for (i in 1:( length(month1Index) - tempInd )){
#   index_old_amount[[i]] = marketTrim[
#     (month1Index[tempInd + i]-1), cryptos_pos[[i]]] / 
#   priceTrim[(month1Index[tempInd + i]-1), cryptos_pos[[i]]]
# }

# calculate old amount for SCRIX
index_old_amount = list()
for (i in 1:( length(month1Index) - tempInd )){
  index_old_amount[[i]] = sum(marketTrim[
    (month1Index[tempInd + i]-1), cryptos_pos[[i]]])* weight[[i]]  / 
    priceTrim[(month1Index[tempInd + i]-1), cryptos_pos[[i]]]
}

# calculate divisor
Div      = numeric( length(month1Index) - tempInd )
Div[1]   = sum( marketTrim[
  (month1Index[tempInd + 1]-1), cryptos_pos[[1]] ] )/ 1000
for (i in 2:length(Div) ){
  Div[i] = sum( marketTrim[
    (month1Index[tempInd + i ]-1), cryptos_pos[[i]] ] ) / 
    (sum( priceTrim[ 
      (month1Index[tempInd + i ] - 1), cryptos_pos[[i - 1]] ] *
        index_old_amount[[i-1]] ) / Div[i-1] )
}

# repeat the divisors, cryptos_pos, and amount for days in the month
Divrep         = numeric( ttend )
cryptos_posrep = list( ttend )
old_amountrep  = list( ttend )
tempIndex      = month1Index[ (tempInd + 1):length(month1Index) ] - roll
tempIndex      = c(tempIndex, ttend + 1)
for (i in 1:(length(tempIndex) - 1)){
  for (j in 1:(tempIndex[i+1]-tempIndex[i]) ){
    Divrep[tempIndex[i] - 1 + j]           = Div[i]
    cryptos_posrep[[tempIndex[i] - 1 + j]] = cryptos_pos[[i]]
    old_amountrep[[tempIndex[i] - 1 + j]]  = index_old_amount[[i]]
  }
}

# calculate SCRIX 
for (i in 1:ttend){
  scrix[i] = sum( priceTrim[scrixtstart + i - 1, cryptos_posrep[[i]] ]*
                    old_amountrep[[i]] ) / Divrep[i]
}

# name the scrix with dates
names(scrix) = names(crix)

## save scrix
scrix_es_return=scrix
save(scrix_es_return, file = "scrix_es_return.RData")
