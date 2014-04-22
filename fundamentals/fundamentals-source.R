library(gdata)
library(lubridate)
library(nlme)
library(stringr)
library(plotrix)
library(plyr)

##############
### load stuff made by FundamentalsUpdateInputs.r
##############

# load("data/fundydata.rdata")
# killResults = 2014
# daysOut =  electionDay("2014")-Sys.Date()
# intervalType = "prediction"

##############
### restrict ourselves to the data that we know.
##############

fitModel <- function(daysOut, killResults = 2014, intervalType = "prediction", q1FECmissing = F) {
  
  dropQ1 <- F
  today <- (electionDay(2014) - Sys.Date()) 
  if(daysOut %in% 203:(today-1) & q1FECmissing) dropQ1 <- T 

  ## make data that is known as of a specific date
  fec2 <- fecAsOf(fec, daysOut, dropQ1)
  cookRatings2 <- cookAsOf(cookRatings, daysOut)
  incApproval2 <- thingAsOf(incApproval, daysOut, "net", "relevant.office")
  presApproval2 <- thingAsOf(presApproval, daysOut, "pctapp", "cycle")
  genBallot2 <- thingAsOf(genBallot, daysOut, "net", "cycle")
  partyFav2 <- thingAsOf(partyFav, daysOut, "net", "cycle")
  
  ## lots of messy reassembling with known data
  reduced$demMarginAll[reduced$year >= killResults] <- NA
  ready2model <- prepData(fec2, cookRatings2, incApproval2, presApproval2, genBallot2, partyFav2, reduced)

  ## no year-end FEC report for 2013 
  noFECdem <- c("chad-taylor", "travis-childers", "david-domina")
  noFECrep <- c("ed-gillespie", "scott-brown")
  noYearEndReport <- ready2model$year == 2014 & (ready2model$slug.Democrat1 %in% noFECdem | ready2model$slug.Republican1 %in% noFECrep) & ready2model$demMoney%in%c(50,-50)
  ready2model$demMoney[noYearEndReport] <- 0

  ## fit a model
  form <- "demMarginAll ~ offyear + pvi + genBallot + incApproval2 + demMoney + netExp + norep + nodem"
  form <- as.formula(form)
  
  fitdata <- subset(ready2model, sigThirdParty == 0)
  m0 <- lm(form, data = fitdata)
  
  ## inflated variance for races that have a history of sen/pres vote being different, particularly the open ones
  ready2model$xx <- interaction(ready2model$presSenMostlyOpposite, ready2model$open2)
  m2 <- gls(form, 
            data = ready2model, 
            na.action = "na.omit", 
            weights = varIdent(form = ~1 | xx))
  var.inflate <- coef(m2$model, unconstrained = F)
  
  ### predictions for 2014
  nd <- subset(ready2model, year == killResults)
  #write.csv(nd[,c("office", "slug.Democrat1", "offyear", "pvi", "genBallot", "incApproval2", "demMoney", "netExp", "norep", "nodem")], file = paste0("~/Desktop/", daysOut, ".csv"))

  temp <- data.frame(suppressWarnings(predict(m0, newdata = nd, interval = intervalType))) ## don't care about rank deficiency when don't have 2 candidates.
  temp$office <- nd$office
  temp$matchup <- nd$matchup
  temp$democrat <- nd$slug.Democrat1
  temp$republican <- nd$slug.Republican1
  temp$predyear <- killResults
  temp$df <- summary(m0)$df[2]
  
  # where we are inflating variance: history of diff sen/pres * openness
  incAndPresDiff <- unique(subset(nd, presSenMostlyOpposite == 1 & open2 == 0)$office)
  openAndPresDiff <- unique(subset(nd, presSenMostlyOpposite == 1)$office)
  openAndPresDiff <- openAndPresDiff[!(openAndPresDiff %in% incAndPresDiff)]
  
  t.star <- qt(.975, temp$df)
  temp$se <- (temp$upr - temp$fit) / t.star
  
  pos <- temp$office %in% incAndPresDiff
  temp$se[pos] <- sqrt(var.inflate[2])*temp$se[pos]
  
  pos <- temp$office %in% openAndPresDiff
  temp$se[pos] <- sqrt(var.inflate[3])*temp$se[pos]

  list(model = temp, 
       var.inflate = var.inflate[2:3], 
       var.inflate.races = list(incAndPresDiff, openAndPresDiff))
}



