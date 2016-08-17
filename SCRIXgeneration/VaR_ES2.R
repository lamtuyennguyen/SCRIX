##### VaR and Expected Shortfall ####
library("evd")
VAR.ES.EVT2 = function(x, alpha) {
  L   = -x
  zq  = quantile(L, 1 - alpha)
  thr = quantile(L, 0.9)
  if( thr==max(L) ){
    evtES = -zq
  }
  else {
    fitty = fpot(L, thr, model = "gpd", std.err = F)
    scale = as.numeric(fitty$scale)
    shape = as.numeric(fitty$param[2])
    evtES = -(zq/(1 - shape) + (scale - shape * thr)/(1 - shape))
  }
  temp        = c(-zq, evtES)
  names(temp) = c("VaR", "ES")
  return(temp)
}

#### GARCH fit ####
#library("fGarch")