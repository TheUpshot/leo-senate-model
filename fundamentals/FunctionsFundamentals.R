### replace $ and ,. Turn () into negatives
cleanDollars <- function(x){
	x <- gsub("\\(", "-", x)
 	x <- as.numeric(gsub("\\$|,|\\)", "", x))
 	x
}


### compute totals as of a certain date. 
fecAsOf <- function(fec, daysOut, dropQ1 = F){

	known <- fec[fec$daysOut  > daysOut, ]

	### don't have Q1 data yet.
	if(dropQ1) print("hi buddy. just fyi, i'm not using any Q1 fec data, because i think you don't have it yet in 2014")
	if(dropQ1) known <- subset(known, daysOut > 203)

#TODO will generate unfair comparisons when one candidate files earlier than the other. be careful around filing deadlines
	totals <- aggregate(list(money = known$money), list(fecid = known$cand_id, year = known$cycle), sum, na.rm = T)
	totals
}



cleanUpResults <- function(results){

	### clean up some things
	results$candidate <- str_trim(results$candidate)
	results$votesmartid[results$votesmartid=="#N/A"] <- NA
	results$fecid[results$fecid=="#N/A"] <- NA

	### add ranks to results
	results <- results[order(results$office),]
	results$rank <- unlist(by(-1*results$vote, results$office, rank, ties.method = "average", na.last = "keep"))

	### simplify party codes
	results$party2 <- results$party
	demids <- c("D", "DEM", "Democrat  ", "D/WF/IDP Combined Parties", "Democratic-Farmer-Labor", "DFL", "D*", "DNL", "Democratic-Nonpartisan League")
	repids <- c("R", "REP", "R/CRV Combined Parties", "Republican  ", "Independent-Republican")
	results$party2[results$party2 %in% demids] <- "Democrat"
	results$party2[results$party2 %in% repids] <- "Republican"
	results$party2[!results$party2%in%c("Democrat", "Republican")] <- "other"

	### add rank by party
	results <- results[order(results$office, results$party2),]
	results$partyrank <- unlist(by(-1*results$vote, paste0(results$office, results$party2), rank, ties.method = "random", na.last = "keep"))
	results$party2 <- paste0(results$party2, results$partyrank)

	### add a total vote
	results <- merge(results, aggregate(list(tvote = results$vote), list(office = results$office), sum, na.rm = T), by = "office", all = T)
	results$pctvote <- 100*results$vote/results$tvote
	results <- results[order(results$vote, decreasing = T), ]

}



## latest cook rating
cookAsOf <- function(data, daysOut){
	known <- data[data$days_til_election > daysOut,]
	names(known)[1] <- "office"
	known <- known[order(known$days_til_election),]
	known <- known[!duplicated(known$office), ]
	known <- known[,c("office", "cook_rating")]
	known
}



thingAsOf <- function(data, daysOut, thing, collapseBy){
	known <- data[data$daysOut >= daysOut,]
	known <- plyr:::arrange(known, as.numeric(daysOut))
	weightingSchemes(known, thing, collapseBy)
}



cleanUpPresApproval <- function(data, var1 = "A0", var2 = "B0", signed = T) {

	data$enddate <- as.Date(data$enddate, format = "%Y-%m-%d")

	if(is.null(data$knownDate)) data$knownDate <- NA

	data$knownDate <- as.Date(data$knownDate, format = "%Y-%m-%d")
	if(all(is.na(data$knownDate))) data$knownDate <- data$enddate

	data <- subset(data, !is.na(enddate))
	data$cycle <-  as.numeric(format(data$enddate, "%Y"))
	data$cycle <- data$cycle + data$cycle%%2 ## broken for days past election day

	## two versions of approval ratings
	data$net <- data[,var1] - data[,var2]
	data$pctapp <- 100*data[,var1]/(data[,var1] + data[,var2])

	## sign the approval ratings
	if(signed){
		reps <- which(data$presparty=="R")
		data$net[reps] <- -1*data$net[reps]
		data$pctapp[reps] <- 100 - data$pctapp[reps]
	}

	data$daysOut <- electionDay(data$cycle) - data$knownDate
	bad <- data$daysOut < 0
	data$cycle[bad] <- data$cycle[bad] + 2
	data$daysOut <- electionDay(data$cycle) - data$knownDate
	data
}



cleanUpPartyFav <- function(data){
	## weirdness so we can use same function as other ones
	data$enddate <- as.character(format(as.Date(data$EndDate, format = "%m/%d/%Y"), format = "%Y-%m-%d"))
	data$A0 <- data$Favorable.dem - data$Unfavorable.dem
	data$B0 <- data$Favorable.rep - data$Unfavorable.rep
	cleanUpPresApproval(data, signed = F)
}



cleanUpPVI <- function(pvi){

	pvi2 <- pvi[ ,grep("pvi|postal", names(pvi))]
	pvi2 <- reshape(pvi2, direction = "long", varying = list(1:16))
	pvi2$year <- 1952 + 4*(16 - pvi2$time)
	names(pvi2)[3] <- "pvi"
	pvi2 <- pvi2[,c("postal", "year", "pvi")]

	pvi3 <- pvi2
	pvi3$year <- pvi3$year + 2

	x <- rbind(pvi2, pvi3)
	rownames(x) <- NULL
	na.exclude(x)
}



realraces <- function(reduced, results){
	these <- c("vote.Democrat1", "vote.Republican1")
	reduced[,these][is.na(reduced[,these])] <- 0
	reduced$demPctMajor <- 100*reduced$vote.Democrat1/(reduced$vote.Democrat1 + reduced$vote.Republican1)
	reduced$demMarginAll <- 100*(reduced$vote.Democrat1 - reduced$vote.Republican1)/reduced$tvote

	reduced$realrace <- as.numeric(reduced$demPctMajor > 10 & reduced$demPctMajor < 90)
	reduced$mostlyARepAndDem <- 1 
	reduced$mostlyARepAndDem [reduced$office %in% subset(results, rank < 3 & party2 == "other1")$office ] <- 0

	## is it really a standard 2-party deal?
	reduced$sigThirdParty <- 0
	reduced$sigThirdParty [reduced$office %in% subset(results, party2 == "other1" & pctvote > 20)$office ] <- 1
	reduced$sigThirdParty10 <- 0
	reduced$sigThirdParty10[reduced$office %in% subset(results, party2 == "other1" & pctvote > 10)$office ] <- 1
	problematicRecodes <- subset(results, xx%in%c("2012MEKing", "2010FLCrist", "2010AKMurkowski", "2006CTLieberman"))$office
	reduced$sigThirdParty10[reduced$office%in%problematicRecodes] <- 1

	reduced$fakeParty <- 0
	reduced$fakeParty[reduced$office%in%problematicRecodes] <- 1

	#reduced$mostlyARepAndDem[ (reduced$vote.Democrat1 + reduced$vote.Republican1) / reduced$tvote  < .80 ] <- 0
	reduced
}



weightingSchemes <- function(known, var, id, lastX = 4){
	known$thing <- known[,var]

	zz <- ddply(known, id, function(X) data.frame(
			x1 = mean(X$thing, na.rm = T), ### mean of everything this election cycle
			x2 = weighted.mean(X$thing, .9^(as.numeric(X$daysOut)/30), na.rm = T), ## time weighting. crazy slow decay.
			x2b = weighted.mean(X$thing, .5^(as.numeric(X$daysOut)/14), na.rm = T), ## half-life of two weeks.
			x2c = weighted.mean(X$thing, .5^(as.numeric(X$daysOut)/30), na.rm = T),
			x3 = X$thing[!is.na(X$thing)][1], ## 1 most recent
			x4 = mean(X$thing[order(X$daysOut) < lastX], na.rm = T) ## 3 most recent
	))
	zz
}



addLags <- function(oldreults, reduced){
	oldresults <- oldresults[,c("year", "class", "postal", "offyear", "office", "demPctMajor")]

	for(i in names(reduced)){
		if(!i%in%names(oldresults)){
			oldresults[,i] <- NA
		}
	}

	oldresults <- oldresults[,names(reduced)]
	oldresults <- subset(oldresults, !is.na(postal))
	reduced <- rbind(reduced, oldresults)

	#### make lags.
	reduced$lastResultsThisSeat <- NA
	reduced$lastResultsThisOffOn <- NA
	reduced$lastResultsThisState <- NA
	reduced <- reduced[order(reduced$year, decreasing = T),]

	### terrible; to improve, handle multi races per state.
	### may want to only do competitive raes? 
	for(i in 1:nrow(reduced)){
		reduced$lastResultsThisSeat[i] <- subset(reduced, postal == reduced$postal[i] & class == reduced$class[i] & year < reduced$year[i] & demPctMajor!=0)$demPctMajor[1]
		reduced$lastResultsThisOffOn[i] <- subset(reduced, postal == reduced$postal[i] & offyear == reduced$offyear[i] & year < reduced$year[i] & demPctMajor!=0)$demPctMajor[1]
		reduced$lastResultsThisState[i] <- subset(reduced, postal == reduced$postal[i] & year < reduced$year[i] & demPctMajor!=0)$demPctMajor[1]
	}

	reduced <- subset(reduced, year > 1990)
	reduced
}



prepareTheData <- function(results, reduced, presApproval, genBallot, partyFav, pvi, fec, incApproval, lastsen, cookRatings, oldresults){

	results <- cleanUpResults(results)

	##############
	### fec clean up
	##############

	##  moved to fec-updates.r

	##############
	### bio collapse. 
	##############

	temp <- names(results)
	bio <- results[ ,grep("city.council", temp):grep("us.senate", temp)]
	bio <- data.frame(nothing = 1, bio)
	results$experience <- apply(bio, 1, function(x) max(which(x == 1))) - 1

	##############
	### 2014, all possible combos
	##############

	finishedRaces <- subset(results, year < 2014)
	finishedRaces$matchup <- 1

	results2014 <- subset(results, year == 2014)
	these <- unique(results2014$office)

	## order by decreasing money, so lower matchup numbers come first
	## temp <- aggregate(list(money = fec$money), list(fecid = fec$cand_id), sum)
	## results2014 <- results2014[order(temp$money[match(results2014$fecid, temp$fecid)], decreasing = T),]

	combos2014 <- NULL
	for(i in these){

		temp <- subset(results2014, office == i)
		dems <- temp[grep("Democrat", temp$party2),]$slug
		reps <- temp[grep("Republican", temp$party2),]$slug

		for(j in 1:length(dems)){
			for(k in 1:length(reps)){

				thisdem <- subset(temp, slug == dems[j])	
				if(nrow(thisdem)>0)
					thisdem$party2 <- "Democrat1"

				thisrep <- subset(temp, slug == reps[k])
				if(nrow(thisrep)>0)
					thisrep$party2 <- "Republican1"

				newmatchup <- rbind(thisdem, thisrep)
				newmatchup$matchup <- paste(j, k, sep = "-")
				combos2014 <- rbind(combos2014, newmatchup)
			}
		}
	}

	combos2014 <- combos2014[-c(grep("0-|-0", combos2014$matchup)),]
	results <- rbind(finishedRaces, combos2014)

	##############
	### drop to a single row per race, keeping data for only the top Democrat and the top Republican
	##############
	reduced <- subset(results, party2 %in% c("Democrat1", "Republican1"))
	reduced <- reshape(reduced, direction = "wide", idvar = c("office", "year", "state", "postal", "class", "tvote", "matchup"), timevar = "party2", drop = c("partyrank", "party"))
	reduced$serious.Republican1[is.na(reduced$candidate.Republican1) & reduced$year == 2014] <- 1
	reduced$serious.Democrat1[is.na(reduced$candidate.Democrat1) & reduced$year == 2014] <- 1

	### some dummys (whether our race is real and offyear), and lagged results by seat
	reduced <- realraces(reduced, results)
	reduced$lastparty <- lastsen$lastparty[match(reduced$office, lastsen$nextoffice)]
	reduced$offyear <- as.numeric(reduced$year%%4 != 0)
	reduced <- addLags(oldresults, reduced)

	reduced$win <- ""
	reduced$win[reduced$win.Democrat1 == 1]  <- "D"
	reduced$win[reduced$win.Republican1 == 1]  <- "R"
	reduced$win[reduced$win.other1 == 1]  <- "O"

	##############
	### identify states where the senator (sitting or newly elected) usually (since 1992) doesn't match the presidential vote.
	##############
	temp <- cbind(postal = pvi$postal, pvi[,grep("diff", names(pvi))] )
	temp2 <- reshape(temp, idvar = "postal", direction = "long", varying = list(2:ncol(temp)))	
	temp2$year <- as.numeric(gsub("diff", "", names(temp)[-c(1)][temp2$time]))
	names(temp2)[3] <- "diff"
	temp2$presparty <- ifelse(temp2$diff > 0, "D", "R")
	tempsen <- lastsen[,c("Party", "term", "state")]
	names(tempsen) <- c("party", "year", "postal")
	temp3 <- merge(tempsen, temp2, by = c("year", "postal"))
	xx <- strsplit(as.character(temp3$party), split = ";")
	temp3$party <- sapply(xx, function(x) paste(unique(x), collapse = ";"))
	temp3 <- subset(temp3, temp3$year > 1992)
	diffsen <- aggregate(list(diffsen = temp3$party!=temp3$presparty), list(postal = temp3$postal), mean)
	diffsen  <- diffsen[order(diffsen$diffsen),]
	reduced$presSenMostlyOpposite <- 0
	reduced$presSenMostlyOpposite [reduced$postal %in% diffsen$postal[diffsen$diffsen > .5]] <- 1

	##############
	### median senate vote
	##############
	medMargin2 <- with(reduced, aggregate(list(x = reduced$demMarginAll), list(postal = reduced$postal), median, na.rm = T))
	reduced$medMargin2 <- medMargin2$x[match(reduced$postal, medMargin2$postal)]

	##############
	### does sitting president party matter (esp in offyear?) - prob not enough data to actaully investigate this
	##############
  	oldpres <- data.frame(year = seq(1990, 2014, by = 2), sittingPres = "D", stringsAsFactors = F)
  	oldpres$sittingPres[oldpres$year %in% c(1990, 1992, 2002, 2004, 2006, 2008)] <- "R"
  	reduced <- merge(reduced, oldpres, by = "year")	

	##############
	### compute margins, name dates consistently, etc. for generic poll data
	##############
	presApproval <- cleanUpPresApproval(presApproval)
	genBallot <- cleanUpPresApproval(genBallot, var1 = "Democrats", var2 = "Republicans", signed = F)
	partyFav <- cleanUpPartyFav(partyFav)
	pvi <- cleanUpPVI(pvi)

	##############
	### add some dates to things
	##############
	incApproval$DATEOUT <- as.Date(incApproval$knownDate, format = "%Y-%m-%d") ### switched from incApproval$DATEOUT to incApproval$knownDate
	incApproval$daysOut <- electionDay(as.numeric(sapply(strsplit(incApproval$relevant.office, split = "-"), function(x) x[5]))) - incApproval$DATEOUT

	save(results, reduced, presApproval, genBallot, partyFav, pvi, fec, incApproval, lastsen, cookRatings, oldresults, file = "fundydata.rdata")

}



prepData <- function(fec2, cookRatings2, incApproval2, presApproval2, genBallot2, partyFav2, reduced){
	
	##############
	### put it all together
	##############

	makeid <- function(data, var, xx = "year") paste(data[,xx], data[,var])
	getMoney <- function(xx) fec2$money[match(makeid(reduced, xx), makeid(fec2, "fecid"))];

	reduced$money.Democrat1   <- getMoney("fecid.Democrat1")
	reduced$money.Republican1 <-  getMoney("fecid.Republican1")
	reduced$cook <- cookRatings2$cook_rating[ match(reduced$office, cookRatings2$office) ]
	reduced$incApproval <- incApproval2$x2[ match(reduced$office, incApproval2$relevant.office) ]
	reduced$presApproval <- presApproval2$x2b[ match(reduced$year, presApproval2$cycle) ]
	reduced$genBallot <- genBallot2$x2c[ match(reduced$year, genBallot2$cycle) ]
	reduced$partyFav <- partyFav2$x2b[ match(reduced$year, partyFav2$cycle) ]
	reduced$pvi <- pvi$pvi [ match(makeid(reduced, "postal"), makeid(pvi, "postal")) ]

	reduced$incumbent <- "open"
	reduced$incumbent[ which(reduced$incumbent.Democrat1 == 1) ] <- "D"
	reduced$incumbent[ which(reduced$incumbent.Democrat1 == "a") ] <- "Da"
	reduced$incumbent[ which(reduced$incumbent.Republican1 == 1) ] <- "R"
	reduced$incumbent[ which(reduced$incumbent.Republican1 == "a") ] <- "Ra"
	reduced$open <- reduced$incumbent == "open"
	reduced$open2 <- !reduced$incumbent %in% c("D", "R")

	### calculate senate percents on a rolling basis
	lastsen2 <- NULL
	for(year in seq(1992, 2014, by = 2)){
		temp <- subset(lastsen, term < year & term >= year - 40)
		demsplits = ddply(temp, "state", function(X) {
			gg <- strsplit(as.character(X$Party), split = ";")
			l <- sapply(gg, length)
			weight <- 1/rep(l, times = l)
			party = unlist(gg)
			weighted.mean(party !="R" , weight)} )
		demsplits$year <- year
		lastsen2 <- rbind(lastsen2, demsplits)
	}
	names(lastsen2)[2] <- "pctDemLast20"
	reduced$pctDemLast20 <- lastsen2$pctDemLast20[ match( paste(reduced$year, reduced$postal), paste(lastsen2$year, lastsen2$state)    )]

	## bad filling in missing ratings
	reduced$temp <- ifelse(reduced$incumbent %in% c("D", "Da"), reduced$demPctMajor, 100 - reduced$demPctMajor)
	missingratings <- which(is.na(reduced$incApproval) & reduced$incumbent != "open")
	temp <- reduced[missingratings,]
	reduced$incApproval[missingratings] <- predict( lm(incApproval ~ pvi + temp, data = subset(reduced, realrace == 1)), newdata = temp )
	reduced$incApproval[is.na(reduced$incApproval)] <- 0

	reduced$cook <- reduced$cook - 4
	reduced$cook[is.na(reduced$cook)] <- 0
	reduced$cook <- as.factor(reduced$cook)
	reduced$cook <- relevel(reduced$cook, ref = "0")

	fixme <- c("money.Democrat1", "money.Republican1", "experience.Democrat1", "experience.Republican1")
	reduced[,fixme][is.na(reduced[,fixme])] <- 0

	reduced$demMoney <- 100 * with(reduced, money.Democrat1 / (money.Democrat1 + money.Republican1) )
	reduced$demMoney[is.na(reduced$demMoney)] <- 50 ## no one has reported money.
	reduced$demMoney <- reduced$demMoney - 50

	reduced$netExp <- reduced$experience.Democrat1 - reduced$experience.Republican1
	reduced$netExp2 <- floor(reduced$experience.Democrat1/3) - floor(reduced$experience.Republican1/3)

	reduced$incApproval2 <- reduced$incApproval
	reps <- reduced$incumbent%in%c("R", "Ra")
	reduced$incApproval2[reps] <- -1 * reduced$incApproval2[reps]

	reduced$incumbentSimple <- substr(reduced$incumbent, 1, 1)
	reduced$incumbentSimple <- relevel(as.factor(reduced$incumbentSimple), ref = 3)

	reduced$notARace <- with(reduced, as.character(cook)%in%c("-3", "3") | is.na(candidate.Democrat1) | is.na(candidate.Republican1))

	reduced$norep <- is.na(reduced$candidate.Republican1)
	reduced$nodem <- is.na(reduced$candidate.Democrat1)
	reduced
}
