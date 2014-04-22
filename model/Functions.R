# Set locale
Sys.setlocale(locale="C")

library(gam)
library(gtools)
library(lubridate)
library(maps)
library(plyr)
library(RJSONIO)

data(state.fips)
state.fips$abb <- as.character(state.fips$abb)
state.fips <- rbind(state.fips, tail(state.fips, 2))
state.fips[64, ]$fips <- 2
state.fips[64, ]$abb <- "AK"
state.fips[65, ]$fips <- 15
state.fips[65, ]$abb <- "HI"

cap <- function(x) {
  # capitalize first character
  s <- strsplit(x, " ")
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="")
}


candidateRename <- function(cand) {
  cand[cand == "Dick Durbin"] <- "Richard Durbin"
  cand[cand == "Mary Landrieu"] <- "Mary L. Landrieu"
  cand[cand == "Ed Markey"] <- "Edward J. Markey"
  cand[cand == "Jim Inhofe"] <- "James M. Inhofe"
  cand[cand == "Mike Enzi"] <- "Michael B. Enzi"
  cand[cand == "Daniel S Sullivan"] <- "Dan Sullivan"
  cand[cand == "Campbell Cavasso"] <- "Cam Cavasso"
  cand[cand == "Matthew Whitaker"] <- "Matt Whitaker"
  cand[cand == "TW Shannon"] <- "T.W. Shannon"
  cand[cand == "Christine ODonnell"] <- "Christine O'Donnell (potential)"
  cand[cand == "Tom Kovach"] <- "Tom Kovach (potential)"
  cand
}


demProb <- function(sims, dem.seats = 34) {
  mean(dem.seats + colSums(sims > 0) >= 50)
}


electionDay <- function(year) {
  # fed numeric or character year, return election day as Date
  year <- as.numeric(year)
  pos <- match("Monday", weekdays(as.Date(paste0(year, "-11-0", 1:7))))
  as.Date(paste0(year, "-11-0", pos + 1))
}


electionSimNew <- function(keep,  
                           breaks=c(-1e-8, 5, 15, 35),
                           s=.840,  # share of error that is nat'l (vs local), by e-day
                           n.sims=10000,  # number of simulations
                           best=F,
                           snowflakes=F,
                           fund=F
) {
  race.names <- unique(keep$office)
  n <- length(race.names)
  if (!best) {
    c <- cumsum(keep$matchupProb)
    t <- 0:(n - 1) + runif(n*n.sims)
    samp <- sapply(t, function(y) match(T, y < c))
    new <- subset(keep[samp, ], select=c(mean, scale, alpha, prior.mean, prior.var, prior.alpha))
  } else {
    new <- keep
  }
  
  mu <- new$mean
  scale <- new$scale
  df <- 2*new$alpha
  if (fund) {
    mu <- new$prior.mean
    scale <- sqrt(new$prior.var)
    df <- 2*new$prior.alpha
  }
  
  days <- keep$d[1]
  
  # flatten covariance. make posdef.
  #   k <- min(1, days / 300)
  #   s <- (1 - k)*s
  
  sims <- mu + (matrix(rt(n*n.sims, df), ncol=n.sims))*sqrt(1 - s^2)*scale +
    rep(rnorm(n.sims, sd=1), each=n)*s*scale
  
  if (days == min(day.seq) & !snowflakes & !best) {
    probs <- rowMeans(sims > 0)
    rating <- 8 - cut(100*probs, breaks=c(breaks, rev(100 - breaks)), labels=F)
    upsets <- colSums(sims[rating <= 2, ] < 0) + colSums(sims[rating >= 6, ] > 0)
    # round down to mult of 5
    k <- 5*floor(100*mean(upsets > 0) / 5)
    h <- mean(sapply(h.eff_list, function(x) {
      temp <- subset(x, d == days)
      temp$mu[match("Public Policy Polling", temp$pollsters)] -
        temp$mu[match("Harper Polling", temp$pollsters)]
    }), na.rm = T)
    #
    mt <- mean(sims[match("mt-senate-class-ii-2014", race.names), ] < 0)
    t <- (1:20)*mt
    pos <- which.min((t - round(t))^2)
    mtNum <- round(pos*mt)
    mtDenom <- pos
    mt <- round(100*mt)
    aan <- ifelse(mt == 8 | mt == 11 | floor(mt / 10) == 8, "an", "a")
    
    filename <- paste0(dataDir, "public/_big_assets/parameters.json")
    
    write(toJSON(list(
      continuingSeats = list(
        dem = 34, rep = 30),
      nationalErrorScale = s,
      upsetProb = k,
      houseEffect = round(2*h) / 2,
      mt = mt,
      mtNum = textify(mtNum),
      mtDenom = textify(mtDenom),
      aan = aan
    )), file=filename)
  }
  
  return(sims)
  
}

getEstimate <- function(results,
                        type,  # choices: normal, polls, fundys
                        house.effects = T,
                        time.weights = T) {
  colname <- paste0("pollAvg", paste(as.numeric(c(house.effects, time.weights)), collapse=""))
  poll.avg <- get(colname, envir = as.environment(results))
  with(results,
       switch(type,
              "normal" = (prior.mean*(kappa - tau) + poll.avg*tau) / kappa,
              "polls" = poll.avg,
              "fundys" = prior.mean
       ))
}


### push data
gitPush <- function(message = "auto update",
                    file = ".",
                    silent = T) {
  system(paste("git add", file), ignore.stdout = silent, ignore.stderr = silent)
  system(paste("git commit -m '", message, file, "'"),
         ignore.stdout = silent, ignore.stderr = silent)
  system("git push", ignore.stdout = silent, ignore.stderr = silent)
}


hardcodeGuys <- function(results, var = "party"){
  # re-code parties where necessary
  results[results$xx == "2000GAMiller", var] <- "DEM" #harmless
  results[results$xx == "2000GAMattingly", var] <- "REP" #harmless
  results[results$xx == "2006CTLieberman", var] <- "REP" # problematic, but lieberman runs to the right of Ned Lamont. means ignoring Alan Schlesinger
  results[results$xx == "2006VTSanders", var] <- "DEM" #harmless
  results[results$xx == "2008MSMusgrove", var] <- "DEM" #harmless
  results[results$xx == "2010AKMurkowski", var] <- "DEM" # problematic, but Murkowski to the left of Joe Miller. means ignoring Scott Adams.
  results[results$xx == "2010FLCrist", var] <- "DEM" # problematic, but Crist to the left of Marco Rubio + would caucus with dems. means ignoring Kendrick B. Meek.
  results[results$xx == "2012CTMurphy", var] <- "DEM" #harmless
  results[results$xx == "2012CTMcMahon", var] <- "REP" #harmless
  results[results$xx == "2012MEKing", var] <- "DEM" # mostly harmless, means ignoring Cynthia Dill
  results[results$xx == "2012MNKlobuchar", var] <- "DEM" #harmless
  results[results$xx == "2012NYGillibrand", var] <- "DEM" #harmless
  results[results$xx == "2012NYLong", var] <- "REP" #harmless
  results[results$xx == "2012NDHeitkamp", var] <- "DEM" #harmless
  results[results$xx == "2012VTSanders", var] <- "DEM" #harmless
  results
}



histoMatic <- function(sims) {
  tab <- table(colSums(sims > 0))
  h <- rep(0, nrow(sims) + 1)
  seats <- 34 + seq_along(h) - 1
  h[as.numeric(names(tab)) + 1] <- tab
  h <- setNames(h, seats)
  return (h / ncol(sims))
}


houseEffects <- function(
  #### Estimate house effects ####
  p,                    # polls
  ratings = NULL,       # pollster ratings (from pollsterRatings function)
  prior = NULL,         # prior houseEffects object, optional
  kappa0 = 4,           # effective sample size of the prior distribution on mu
  lambda.m = 1,         # mean and variance of prior marginal distribution of
  lambda.v = 1/2,       #   precision, lambda
  predictive = T,       # whether the model is intended to be predictive...
  #   ie, whether to not include election results in calc
  election.ss = 50000,  # default sample size given to election result
  alpha0 = NULL,
  beta0 = NULL
){
  # add a poll for each race giving result
  x <- p[1, ]
  if (predictive == F) {
    for (race in as.character(unique(p$office))) {
      ind <- match(race, names(races))
      x$sample_size <- election.ss    # choose strength here
      x$mid_date <- 0
      x$margin <- x$obsMargin <- -diff(races[[ind]]$result)
      x$sample_varnum <- 1
      x$pollster <- "Election"
      x$office <- race
      p <- rbind(p, x)
    }
  }
  
  effective.ss <- matrix(0, nrow(p), nrow(p))
  # matrix gives effective sample size for poll in COLUMN for calculating
  # current mean for poll in ROW
  for (j in 1:nrow(p)) {
    w <- timeWeight(abs(p$mid_date - p$mid_date[j]), p$mid_date[j]) *
      as.numeric(p$office == p$office[j])  # only include if same race
    effective.ss[j, ] <- w*p$sample_size
  }
  
  pollsters <- unique(p$pollster)
  mu <- rep(0, length(pollsters))
  kappa <- rep(kappa0, length(pollsters))
  # choose parameters
  # based on other measures of house effects, house effects of legitimate 
  # pollsters seem to all be in range of +/- 3 points. so use this as prior?
  if (is.null(alpha0)) {
    alpha0 <- lambda.m^2 / lambda.v
  }
  if(is.null(beta0)) {
    beta0 <- lambda.m / lambda.v
  }
  alpha <- rep(alpha0, length(pollsters))
  beta <- rep(beta0, length(pollsters))
  h.eff <- data.frame(pollsters, mu, kappa, alpha, beta, stringsAsFactors=F)
  if (!is.null(prior)) {
    # incorporate prior info if given
    pos <- match(h.eff$pollsters, prior$pollsters)
    h.eff[!is.na(pos), ] <- prior[pos[!is.na(pos)], ]
  }
  poll.pos <- match(p$pollster, h.eff$pollster)
  tab <- table(poll.pos)
  h.eff$number <- as.numeric(tab[order(as.numeric(names(tab)))])
  tau <- 1 / (10000*p$sample_varnum/t(effective.ss))
  
  # est gives poll result after correcting for prior house effect estimate
  p$est <- p$margin - h.eff$mu[poll.pos]
  # xbar gives best estimate of state of race at time of poll
  p$xbar <- apply(tau, 2, function(w) weighted.mean(p$est, w))
  p$tau <- colSums(tau)  # precision of estimate
  
  num <- length(setdiff(h.eff$pollster, "Election"))
  for (i in 1:num) {
    m <- h.eff$mu[i]
    k <- h.eff$kappa[i]
    n <- p$tau[poll.pos == i]
    if (sum(n) == 0) next  # if no weights, nothing updates
    x <- p$margin[poll.pos == i] - p$xbar[poll.pos == i]
    h.eff$mu[i] <- weighted.mean(c(m, x), c(k, n))
    h.eff$alpha[i] <- h.eff$alpha[i] + sum(n)/2
    mu.star <- weighted.mean(x, n)  # weighted mean of observations
    c <- h.eff$beta[i] + sum((x - mu.star)^2) / 2
    h.eff$beta[i] <- c + k*sum(n)*(mu.star - m)^2 / (2*(k + sum(n)))
    h.eff$kappa[i] <- k + sum(n)
  }
  return(h.eff)
}

imputePolls <- function(polls, races, race.name, delete=F, a0=20) {
  # combines multiple candidate combos from single poll into one D-R number.
  # also use to specify what polls say if candidate x wins
  
  # polls:      polls
  # races:      races
  # race.name:  eg, "ak-senate-class-ii-2014"
  
  # add'l arguments for specifying primary winner
  # party:      "DEM" or "REP"
  # candidate:  must match candidate name in races object
  # a0:         dispersion param for Dirichlet dist. passed to probGet fxn
  
  # if candidate specified, delete polls first
  x <- races[[match(race.name, names(races))]]
  
  if (delete) {
    polls <- polls[!(polls$office == race.name), ]
  }
  
  race.polls <- x$polls
  if (is.null(race.polls)) {
    return(polls)
  }
  p <- race.polls[race.polls$branch == "sen_g", ]
  if (is.null(p)) {
    return(polls)
  }
  if (nrow(p) == 0) {
    return(polls)
  }
  p$id <- substr(p$poll_id, 1, 5)
  # if it's general election polling, just make composite ... impute values
  reduced <- p[, -(1:match("type", names(p)))]  # just numbers
  temp <- reduced[apply(reduced, 1, function(x) sum(!is.na(x)) > 0), ]
  # split into Dem table and Rep table
  id <- temp[, ncol(temp)]
  temp <- temp[, -((ncol(temp) - 2):ncol(temp))]
  if (is.null(ncol(temp))) {
    return(polls)
  }
  reps <- data.frame(temp[, x$party == "REP"])
  dems <- data.frame(temp[, x$party == "DEM"])
  pos <- apply(reps, 1, function(x) sum(!is.na(x)) > 0) &
    apply(dems, 1, function(x) sum(!is.na(x)) > 0)
  id <- id[pos]
  reps <- data.frame(reps[pos, ])
  dems <- data.frame(dems[pos, ])
  if (nrow(dems)*nrow(reps) == 0) {
    return(polls)
  }
  colnames(reps) <- r.names <- x$slug[x$party == "REP"]
  colnames(dems) <- d.names <- x$slug[x$party == "DEM"]
  start.column <- match("type", names(p)) + 1
  
  d.probs <- colSums(x$probs)
  r.probs <- rowSums(x$probs)
  
  r <- NULL
  d <- NULL
  # now combine the results into one D vs. R number for each poll
  agg <- r.res <- d.res <- array(NA, dim=c(length(r.names), length(d.names),
                                           length(unique(id))),
                                 dimnames=list(r.names, d.names, unique(id)))
  
  if (length(r.names)*length(d.names) == 1) {
    
    margin <- setNames(dems - reps, "margin")
    other <- setNames(100 - (dems + reps), "other")
    
  } else {
    
    for (j in 1:length(unique(id))) {  
      pos <- id == unique(id)[j]
      r.temp <- data.frame(reps[pos, ])
      d.temp <- data.frame(dems[pos, ])
      jj <- 1
      # 
      for (jj in 1:nrow(r.temp)) {
        row <- which(!is.na(r.temp[jj, ]))
        col <- which(!is.na(d.temp[jj, ]))
        r.res[cbind(row, col, j)] <- pmax(r.res[cbind(row, col, j)],
                                          r.temp[cbind(jj, row)], na.rm=T)
        d.res[cbind(row, col, j)] <- pmax(d.res[cbind(row, col, j)],
                                          d.temp[cbind(jj, col)], na.rm=T)
      }
    }
    imputed <- agg <- apply(d.res - r.res, 3, as.vector)
    other <- agg.other <- apply(100 - (d.res + r.res), 3, as.vector)
    for (j in 1:ncol(agg)) {
      y <- agg[, j]
      m <- colMeans(agg - y, na.rm=T)
      w <- nrow(agg) - colSums(is.na(agg - y))  # weights
      pos <- which(is.na(y))
      if (length(pos) > 0) {
        imputed[pos, j] <- apply(agg[pos, ] - matrix(
          m, length(pos), ncol(agg), byrow=T), 1, weighted.mean, w=w, na.rm=T)
      }
      # other:
      y <- agg.other[, j]
      m <- colMeans(agg.other - y, na.rm=T)
      w <- nrow(agg.other) - colSums(is.na(agg.other - y))  # weights
      pos <- which(is.na(y))
      if (length(pos) > 0) {
        other[pos, j] <- apply(agg.other[pos, ] - matrix(
          m, length(pos), ncol(agg.other), byrow=T), 1, weighted.mean, w=w, na.rm=T)
      }
    }
    imputed[is.na(imputed)] <- NA
    other[is.na(other)] <- NA
    probs <- as.vector(outer(r.probs, d.probs))
    margin <- apply(imputed, 2, weighted.mean, w=probs, na.rm=T)
    other <- apply(other, 2, weighted.mean, w=probs, na.rm=T)
  }
  
  general <- p[match(unique(id), p$id), 1:match("type", names(p))]
  general <- cbind(general, margin, other)
  general$office <- race.name
  
  general$poll_id <- sapply(strsplit(general$poll_id, "_"), function(x) x[1])
  start <- as.Date(general$start, format="%Y-%m-%d")
  end <- as.Date(general$end, format="%Y-%m-%d")
  general$mid_date <- as.numeric(electionDay(2014) - (start + (end - start)/2))
  colnames(general)[match("sample", colnames(general))] <- "sample_size"
  colnames(general)[match("type", colnames(general))] <- "population"
  general <- general[order(general$mid_date), ]
  general <- general[!is.na(general$margin), ]
  
  return(rbind(polls, general))
}



imputePollsNew <- function(polls, races, race.name, probs, delete=F, a0=20) {
  # combines multiple candidate combos from single poll into one D-R number.
  # also use to specify what polls say if candidate x wins
  
  # polls:      polls
  # races:      races
  # race.name:  eg, "ak-senate-class-ii-2014"
  
  # add'l arguments for specifying primary winner
  # party:      "DEM" or "REP"
  # candidate:  must match candidate name in races object
  # a0:         dispersion param for Dirichlet dist. passed to probGet fxn
  
  # if candidate specified, delete polls first
  x <- races[[match(race.name, names(races))]]
  
  if (delete) {
    polls <- polls[!(polls$office == race.name), ]
  }
  
  race.polls <- x$polls
  if (is.null(race.polls)) {
    return(polls)
  }
  p <- race.polls[race.polls$branch == "sen_g", ]
  if (is.null(p)) {
    return(polls)
  }
  if (nrow(p) == 0) {
    return(polls)
  }
  p$id <- substr(p$poll_id, 1, 5)
  # if it's general election polling, just make composite ... impute values
  reduced <- p[, -(1:match("type", names(p)))]  # just numbers
  temp <- reduced[apply(reduced, 1, function(x) sum(!is.na(x)) > 0), ]
  # split into Dem table and Rep table
  id <- temp[, ncol(temp)]
  temp <- temp[, -((ncol(temp) - 2):ncol(temp))]
  if (is.null(ncol(temp))) {
    return(polls)
  }
  reps <- data.frame(temp[, x$party == "REP"])
  dems <- data.frame(temp[, x$party == "DEM"])
  pos <- apply(reps, 1, function(x) sum(!is.na(x)) > 0) &
    apply(dems, 1, function(x) sum(!is.na(x)) > 0)
  id <- id[pos]
  reps <- data.frame(reps[pos, ])
  dems <- data.frame(dems[pos, ])
  if (nrow(dems)*nrow(reps) == 0) {
    return(polls)
  }
  colnames(reps) <- r.names <- rownames(probs)
  colnames(dems) <- d.names <- colnames(probs)
  start.column <- match("type", names(p)) + 1
  
  d.probs <- colSums(probs)
  r.probs <- rowSums(probs)
  
  r <- NULL
  d <- NULL
  # now combine the results into one D vs. R number for each poll
  
  agg <- r.res <- d.res <- array(NA, dim=c(length(r.names), length(d.names),
                                           length(unique(id))),
                                 dimnames=list(r.names, d.names, unique(id)))
  
  if (length(r.names)*length(d.names) == 1) {
    
    margin <- setNames(dems - reps, "margin")
    other <- setNames(100 - (dems + reps), "other")
    
  } else {
    
    for (j in 1:length(unique(id))) {  
      pos <- id == unique(id)[j]
      r.temp <- data.frame(reps[pos, ])
      d.temp <- data.frame(dems[pos, ])
      jj <- 1
      # 
      for (jj in 1:nrow(r.temp)) {
        row <- which(!is.na(r.temp[jj, ]))
        col <- which(!is.na(d.temp[jj, ]))
        r.res[cbind(row, col, j)] <- pmax(r.res[cbind(row, col, j)],
                                          r.temp[cbind(jj, row)], na.rm=T)
        d.res[cbind(row, col, j)] <- pmax(d.res[cbind(row, col, j)],
                                          d.temp[cbind(jj, col)], na.rm=T)
      }
    }
    imputed <- agg <- apply(d.res - r.res, 3, as.vector)
    other <- agg.other <- apply(100 - (d.res + r.res), 3, as.vector)
    for (j in 1:ncol(agg)) {
      y <- agg[, j]
      m <- colMeans(agg - y, na.rm=T)
      w <- nrow(agg) - colSums(is.na(agg - y))  # weights
      pos <- which(is.na(y))
      if (length(pos) > 0) {
        imputed[pos, j] <- apply(agg[pos, ] - matrix(
          m, length(pos), ncol(agg), byrow=T), 1, weighted.mean, w=w, na.rm=T)
      }
      # other:
      y <- agg.other[, j]
      m <- colMeans(agg.other - y, na.rm=T)
      w <- nrow(agg.other) - colSums(is.na(agg.other - y))  # weights
      pos <- which(is.na(y))
      if (length(pos) > 0) {
        other[pos, j] <- apply(agg.other[pos, ] - matrix(
          m, length(pos), ncol(agg.other), byrow=T), 1, weighted.mean, w=w, na.rm=T)
      }
    }
    imputed[is.na(imputed)] <- NA
    other[is.na(other)] <- NA
    probs <- as.vector(outer(r.probs, d.probs))
    margin <- apply(imputed, 2, weighted.mean, w=probs, na.rm=T)
    other <- apply(other, 2, weighted.mean, w=probs, na.rm=T)
    
  }
  
  general <- p[match(unique(id), p$id), 1:match("type", names(p))]
  general <- cbind(general, margin, other)
  general$office <- race.name
  
  general$poll_id <- sapply(strsplit(general$poll_id, "_"), function(x) x[1])
  start <- as.Date(general$start, format="%Y-%m-%d")
  end <- as.Date(general$end, format="%Y-%m-%d")
  # add middle date as days until election
  general$mid_date <- as.numeric(electionDay(2014) - (start + (end - start)/2))
  colnames(general)[match("sample", colnames(general))] <- "sample_size"
  colnames(general)[match("type", colnames(general))] <- "population"
  colnames(general)[match("cycle", colnames(general))] <- "year"
  general <- general[order(general$mid_date), ]
  general <- general[!is.na(general$margin), ]
  
  return(rbind(polls, general))
}


matchupSwitch <- function(race.name, d) {
  
  x <- new.races[[match(race.name, race.names)]]
  
  fec.ids <- x$fecid
  parties <- x$party  
  
  d.pos <- which(parties == "DEM")
  r.pos <- which(parties == "REP")  
  
  d.names <- x$slug[d.pos]
  r.names <- x$slug[r.pos]
  
  x$serious[x$day_in < d] <- 0
  x$serious[x$day_out < d] <- 1
  
  if (!is.null(x$polls)) {
    race.polls <- subset(x$polls, add_date >= d)
    if (nrow(race.polls) == 0) {
      race.polls <- NULL
    }
  } else {
    race.polls <- NULL
  }
  
  fec.temp <- subset(fec.new, daysOut >= d)
  fec.temp <- aggregate(list(money = fec.temp$money),
                        list(fecid = fec.temp$cand_id), sum, na.rm = T)
  money <- fec.temp$money[match(fec.ids, fec.temp$fecid)]
  money[is.na(money)] <- 0
  
  d.probs <- probGet(d.names, d.pos, "D", money, race.polls)
  r.probs <- probGet(r.names, r.pos, "R", money, race.polls)
  
  probs <- outer(r.probs, d.probs)
  
  if (sum(probs) == 0) {
    probs <- probs + 1
  }
  
  rownames(probs) <- r.names
  colnames(probs) <- d.names
  
  return(probs)
}


outputGen <- function(results, sim_list, sim_list_fund, breaks = c(-1e-8, 5, 15, 35),
                      best = F, snowflakes = F) {
  
  # output directory
  wd <- paste0(dataDir, "public/_big_assets/")
  
  # postfix
  pf <- paste0(ifelse(best, "-best", ""), ifelse(snowflakes, "-snowflakes", ""))
  
  res <- ddply(results, .(office, d), summarize,
               mean = weighted.mean(mean, matchupProb),
               probs = weighted.mean(prob, matchupProb))
  res <- arrange(res, office, desc(d))
  race.names <- unique(res$office)
  day.seq <- rev(sort(unique(res$d)))
  
  ##### state ratings #####
  df <- subset(res, d == min(day.seq))
  df$state <- toupper(substr(df$office, 1, 2))
  # set breakpoints
  df$fips <- state.fips$fips[match(df$state, state.fips$abb)]
  df$probs <- 100*df$probs
  df$rating <- 8 - cut(df$probs, breaks=c(breaks, rev(100 - breaks)), labels=F)
  df$state[match("ok-senate-class-iii-2014", df$office)] <- "OK-2"
  df$state[match("sc-senate-class-iii-2014", df$office)] <- "SC-2"
  df$fips[match("ok-senate-class-iii-2014", df$office)] <- "40-2"
  df$fips[match("sc-senate-class-iii-2014", df$office)] <- "45-2"
  rownames(df) <- NULL
  
  df <- subset(df, select = c(probs, state, office, fips, rating))
  colnames(df) <- c("dem-pct", "state", "office", "fips", "nyt-rating")
  new.df <- list()
  for (i in 1:nrow(df)) {
    new.df[[i]] <- df[i, ]
  }
  
#   df <- list("date"=format(Sys.time(), format = "%a %b %d %Y %H:%M:%S GMT-0500 (EST)"),
#              "data"=new.df)
  df <- list("date"=format(Sys.time(), format = "%Y-%m-%dT%T%z"),
             "data"=new.df)
  filename <- paste0(wd, "nyt-ratings-today", pf, ".json")
  write(toJSON(df), file=filename)
  
  ##### histogram #####
  x <- sim_list[[length(sim_list)]]
  hist <- histoMatic(x)
  tab <- cbind(34:70, hist)
  filename <- paste0(wd, "histogram", pf, ".tsv")
  write.table(tab, file=filename, quote=F, sep='\t', row.names=F,
              col.names=c("dem", "chance"))
  
  ##### line chart #####
  z <- sapply(sim_list, demProb)
  tab <- cbind(trim(format(electionDay(2014) - day.seq, "%e-%b-%y")),
               100*z)
  if (just.today) {
    filename <- paste0(wd, "senate-likelihood", pf, "-new.tsv")
  } else {
    filename <- paste0(wd, "senate-likelihood", pf, ".tsv")
  }
  write.table(tab, file = filename, quote = F, sep='\t', row.names = F,
              col.names = c("date", "dem"))
  
  ##### all states time-series #####
  z <- res$probs
  tab <- cbind(rep(race.names, each = length(day.seq)),
               trim(format(electionDay(2014) - day.seq, "%e-%b-%y")),
               100*z)
  if (just.today) {
    filename <- paste0(wd, "senate-likelihood-all", pf, "-new.tsv")
  } else {
    filename <- paste0(wd, "senate-likelihood-all", pf, ".tsv")
  }
  write.table(tab, file = filename,
              quote = F, sep = '\t', row.names = F,
              col.names = c("office", "date", "dem"))
}

pollPredict <- function(yr, days, polls, races,
                        fund.mean,
                        fund.se,
                        ratings = NULL,
                        race.names = NULL,
                        # parameters for bayes stuff:
                        prior.alpha = 200,
                        k.min = "useSpline",
                        ...) {  # add'l parameters passed to houseEffects fxn
  # prior.alpha  numeric
  # k.min        if "useSpline", the empirical spline is used. else should be numeric.
  load("~/Google Drive/senate/louisiana/la.RData")
  
  #### incorporate non-polling info as prior ####
  if (is.null(race.names)) {
    race.names <- names(races)[sapply(races, function(x) x$cycle == yr)]
  }
  
  res <- data.frame(office = race.names,
                    prior.mean = fund.mean,
                    prior.var = fund.se^2,
                    mean = fund.mean,
                    var = fund.se^2,
                    prior.alpha = prior.alpha,
                    stringsAsFactors = F, row.names = NULL)
  
  # add polling info
  polls <- subset(polls, year == yr & add_date >= days)
  
  ### throw out pollsters
  dropme <- c() # "Public Policy Polling", "Rasmussen Reports", "Harper Polling",  "Wenzel Strategies"
  if (length(dropme) > 0) {
    polls <- subset(polls, !(pollster %in% dropme))
  }
  
  polls <- ddply(polls, .(office), mutate, min_days = min(mid_date))
  h.eff <- houseEffects(polls, ratings, ...)  # calculate house effects
  
  # apply house effects
  poll.pos <- match(polls$pollster, h.eff$pollster)
  polls$est <- polls$margin - h.eff$mu[poll.pos]
  
  # additional variance due to house effect estimation
  polls$var <- (h.eff$beta / ((h.eff$alpha - 1)*h.eff$kappa))[poll.pos]
  polls$pie <- 0
  polls$tau <- NA
  
  for (i in seq_along(unique(polls$office))) {
    pos <- which(polls$office == unique(polls$office)[i])
    dd <- polls$min_days[pos[1]]
    polls$tau[pos] <- (1 / (10000*polls$sample_varnum/(polls$sample_size*timeWeight(
      polls$mid_date - dd, dd)) + polls$var + polls$pie^2))[pos]
  }
  
  load("optimizing/splines-new.RData")
  
  gamma <- gamma.spline$y[match(round(days), gamma.spline$x)]
  k.min <- gamma*k.spline$y[match(round(days), k.spline$x)]
  cap <- cap.spline$y[match(round(days), cap.spline$x)]
  
  res$kappa <- k <- k.min
  res$prior.beta <- (res$prior.alpha - 1)*k*res$prior.var
  res$poll.var <- res$pollAvg11 <- res$pollAvg10 <- res$pollAvg01 <- res$pollAvg00 <- res$trimmed <- res$jackknife1 <- res$simpAvg <- res$prob50 <- res$fit50 <- res$tau <- res$pollAvg <- res$beta <- res$alpha <- NA
  poll.df <- NULL
  for (i in 1:nrow(res)) {
    race <- res$office[i]
    p <- polls[polls$office == race, ]
    if (nrow(p) == 0) next
    m <- res$prior.mean[i]
    v <- res$prior.var[i]
    alpha <- res$prior.alpha[i]
    k <- k.min
    beta <- (alpha - 1)*k*v
    tau <- p$tau / (max(p$tau))
    n <- gamma*min(1, cap / sum(tau))*tau
    p$weight <- n
    res$tau[i] <- tau <- sum(n)
    res$pollAvg[i] <- mu.star <- weighted.mean(p$est, n)
    # remove lv-adjustment for simp average
    q <- p$margin
    q[p$population == "rv"] <- q[p$population == "rv"] - lv.adj
    
    # 01 :  time-weights
    # 10 :  house effects + pollster ratings + lv-adjust
    
    res$pollAvg00[i] <- mean(q)
    res$pollAvg01[i] <- weighted.mean(q, timeWeight(p$mid_date - days, days))
    res$pollAvg10[i] <- mean(p$est)
    res$pollAvg11[i] <- mu.star
    
    res$mean[i] <- weighted.mean(c(m, p$est), c(k, n))
    
    # poll tracker
    poll.temp <- rbind(p, c(rep(NA, ncol(p) - 1), k))
    # add fundamentals model
    poll.temp$office[nrow(poll.temp)] <- race
    poll.temp$est[nrow(poll.temp)] <- m
    poll.df <- rbind(poll.df, poll.temp)
    
    #
    q <- quantile(p$est, probs = c(.15, .85))
    pos <- (p$est - q[1])*(p$est - q[2]) < 0
    res$trimmed[i] <- weighted.mean(p$est[pos], p$tau[pos])
    
    #
    if (yr == 2014) {
      land.mean <- weighted.mean((100 - p$other + p$est) / 2, n)
      other <- weighted.mean(p$other, n)
      new <- t(setNames(c(NA, land.mean, other),
                        c("result", "pollAvg", "other")))
      pred <- predict.lm(lm.obj, newdata=as.data.frame(new),
                         se=T, interval="prediction")
      t.star <- qt(.975, pred$df)
      scale <- -diff(pred$fit[, 1:2]) / t.star
      m <- pred$fit[1, 1]
      res$prob50[i] <- pt((m - 50) / scale, pred$df)
    }
    #
    res$alpha[i] <- alpha + sum(n)/2
    res$poll.var[i] <- sum(n*(p$est - mu.star)^2) / sum(n)
    c <- beta + sum((p$est - mu.star)^2) / 2
    res$beta[i] <- c + k*sum(n)*(mu.star - m)^2 / (2*(k + sum(n)))
    res$kappa[i] <- k + sum(n)
  }
  pos <- is.na(res$beta)
  res$alpha[pos] <- res$prior.alpha[pos]
  res$beta[pos] <- res$prior.beta[pos]
  res$tau[pos] <- 0
  res$poll.var[pos] <- res$pollAvg11[pos] <- res$pollAvg01[pos] <- res$pollAvg10[pos] <- res$pollAvg00[pos] <- res$pollAvg[pos] <- 100*sign(res$prior.mean[pos])
  res$scale <- sqrt((res$beta / res$alpha)*(1/res$kappa))
  
  # probability of Dem win
  res$prob <- pt(res$mean / res$scale, 2*res$alpha)
  return(list(forecast = res,
              poll.df = poll.df,
              h.eff = h.eff))
}


pollPrep <- function(races, max.margin=20) {
  # does some things
  # skip unopposed races
  races <- races[sapply(races, function(x) length(x$result)) == 2]
  # restrict to "close" races 
  pos <- sapply(races, function(x) (abs(diff(x$result)) < max.margin))
  races <- races[pos]
  # with polling...
  pos <- sapply(races, function(x) !is.null(x$polls))
  races <- races[pos]
  pos <- sapply(races, function(x) nrow(x$polls) > 0)
  races <- races[pos]
  
  ##### rv vs. lv #####
  results <- data.frame(stringsAsFactors=F)
  names <- NULL
  for (i in seq_along(races)) {
    p <- races[[i]]$polls
    p.id <- apply(cbind(p$end_date, p$pollster), 1, function(x) paste(x, collapse=""))
    tab <- table(p.id)
    dupes <- names(tab)[tab > 1]
    for (d in dupes) {
      pos <- which(p.id == d)
      ord <- match(c("lv", "rv", "a"), p[pos, ]$population)
      #     ord[is.na(ord)] <- 0
      results <- rbind(results, c(p[pos[1], ]$mid_date, p[pos, ]$margin[ord],
                                  races[[i]]$cycle))
      names <- c(names, p[pos[1], ]$pollster)
    }
  }
  results <- cbind(results, names)
  colnames(results) <- c("mid_date", "lv", "rv", "a", "cycle", "pollster")
  lv.adj <<- mean(results$lv - results$rv, na.rm=T)
  
  # if both a and rv, use rv. if both a/rv and lv, use lv.
  for (i in seq_along(races)) {
    p <- races[[i]]$polls
    p <- p[p$population == "lv" | p$population == "rv", ]
    p.id <- apply(cbind(p$end_date, p$pollster), 1, function(x) paste(x, collapse=""))
    tab <- table(p.id)
    dupes <- names(tab)[tab > 1]
    for (d in dupes) {
      pos <- which(p.id == d)
      bad <- pos[match("rv", p[pos, ]$population)]
      if (is.na(bad)) {
        bad <- NULL
      }
      if (length(bad) > 0) {
        p <- p[-bad, ]
        p.id <- p.id[-bad]
      }
    }
    races[[i]]$polls <- p
  }
  
  # remove overlapping polls
  for (i in seq_along(races)) {
    p <- races[[i]]$polls
    ord <- rev(order(as.Date(p$end, format="%m/%d/%Y")))
    p <- p[ord, ]
    bads <- NULL
    for (pollster in unique(p$pollster)) {
      pos <- which(p$pollster == pollster)
      if (length(pos) > 1) {
        starts <- as.Date(p[pos, ]$start, format="%m/%d/%Y")
        ends <- as.Date(p[pos, ]$end, format="%m/%d/%Y")
        for (j in pos) {
          if (j %in% bads) next
          k.pos <- which(starts[j] <= ends)
          bads <- c(bads, k.pos[k.pos > j])
        }
      }
    }
    if (length(bads) > 0) {
      races[[i]]$polls <- p[-bads, ]
    }
  }
  for (i in seq_along(races)) {
    p <- races[[i]]$polls
    races[[i]]$polls <- p[order(p$mid_date), ]
  }
  
  polls <- do.call(rbind, lapply(races, function(x) x$polls))
  pos <- match(sapply(strsplit(rownames(polls), ".", fixed=T), function(x) x[1]),
               names(races))
  temp <- t(sapply(races[pos], function(x) x$result))
  office <- names(races[pos])
  polls <- cbind(polls, temp, office)
  polls$obsMargin <- polls$"1" - polls$"2"
  # add year
  polls$year <- as.numeric(sapply(strsplit(rownames(polls), ".", fixed=T),
                                  function(x) {
                                    y <- x[1]
                                    substr(y, nchar(y) - 3, nchar(y))
                                  }))
  polls$pollster[polls$pollster == "CNN/Opinion Research"] <- "CNN / Opinion Research"
  polls <- polls[!is.na(polls$margin), ]
  # add field for sample variance
  p2 <- (100 - polls$other - polls$margin)/200
  p1 <- p2 + polls$margin/100
  # if sample size is zero, assign sample size equal to 350
  polls$sample_size[polls$sample_size == 0] <- 350
  # reduce sample size to adjust for those answering other than top two
  polls$sample_size <- (100 - polls$other)*polls$sample_size / 100
  # numerator for sampling variance:
  polls$sample_varnum <- (p1*(1 - p1) + p2*(1 - p2) + 2*p1*p2)
  # adjust the rv polls
  x <- polls[polls$population == "rv", ]$margin
  polls[polls$population == "rv", ]$margin <- x + lv.adj
  
  return(polls)
}


pollPrep2014 <- function(polls, lv.adj) {
  # for prepping 2014 polls, in form they come in from the combined csv.
  # needs lv adjustment to be specified (saved to global env when you calc
  # pollster ratings)
  
  polls$pollster <- gsub(" (D)", "", polls$pollster, fixed=T)
  polls$pollster <- gsub(" (R)", "", polls$pollster, fixed=T)
  p <- polls[polls$population == "lv" | polls$population == "rv", , drop = F]
  p.id <- apply(cbind(p$start_date, p$mid_date, p$pollster, p$office),
                1, function(x) paste(x, collapse=""))
  tab <- table(p.id)
  dupes <- names(tab)[tab > 1]
  for (d in dupes) {
    pos <- which(p.id == d)
    bad <- pos[match("rv", p[pos, ]$population)]
    if (is.na(bad)) {
      bad <- NULL
    }
    if (length(bad) > 0) {
      p <- p[-bad, ]
      p.id <- p.id[-bad]
    }
  }
  
  # remove overlapping polls
  polls <- p[rev(order(as.Date(p$end, format="%Y-%m-%d"))), ]
  for (office in unique(polls$office)) {
    pos <- polls$office == office
    p <- polls[pos, ]
    polls <- polls[!pos, ]
    bads <- NULL
    for (pollster in unique(p$pollster)) {
      pos <- which(p$pollster == pollster)
      if (length(pos) > 1) {
        starts <- as.Date(p[pos, ]$start, format="%m/%d/%Y")
        ends <- as.Date(p[pos, ]$end, format="%m/%d/%Y")
        for (j in pos) {
          if (j %in% bads) next
          k.pos <- which(starts[j] <= ends)
          bads <- c(bads, k.pos[k.pos > j])
        }
      }
    }
    if (length(bads) > 0) {
      cat(unlist(p[bads, ]))
      p <- p[-bads, ]
    }
    polls <- rbind(polls, p)
  }
  
  colnames(polls)[match("cycle", colnames(polls))] <- "year"
  # add field for sampling variance
  p2 <- (100 - polls$other - polls$margin)/200
  p1 <- p2 + polls$margin/100
  # numerator for sampling variance:
  polls$sample_varnum <- (p1*(1 - p1) + p2*(1 - p2) + 2*p1*p2)
  # if sample size is zero, assign sample size equal to 350
  polls$sample_size[polls$sample_size == 0] <- 350
  # reduce sample size to adjust for those answering other than top two
  polls$sample_size <- (100 - polls$other)*polls$sample_size / 100
  # adjust the rv polls
  x <- polls[polls$population == "rv", ]$margin
  polls[polls$population == "rv", ]$margin <- x + lv.adj
  
  return(polls)
}


# primaries...
probGet <- function(names, pos, party, money, days,
                    race.polls = NULL, a0 = 20, dir.sims = 100000) {
  n <- length(pos)
  primary.code <- paste0("sen_", toupper(substr(party, 1, 1)), "_p")
  if (n <= 1) return(1)
  x.fec <- money[pos]
  x.fec[is.na(x.fec)] <- 0
  fec.probs <- (x.fec + 1) / (sum(x.fec) + n)
  if (!is.null(race.polls)) {
    start.column <- match("type", names(race.polls)) + 1
    gen <- race.polls[start.column:(ncol(race.polls) - 1)]
    id <- substr(rownames(gen), 1, 5)
    init.probs <- colMeans(cbind(aggregate(gen, by=list(id), function(x) mean(!is.na(x)))[, -1]))
    poll.probs <- init.probs[pos]
    poll.probs <- nrow(gen)*poll.probs / sum(poll.probs)
    flatten <- rep(1/n, n)
    probs <- rowSums(cbind(poll.probs, flatten), na.rm=T)
    probs <- probs / sum(probs)
  } else {
    probs <- rep(1/n, n)
  }
  probs <- (probs + 10*fec.probs) / sum(probs, 10*fec.probs)
  
  if (!is.null(race.polls)) {
    primary <- race.polls[race.polls$branch == primary.code, ]
    # then incorporate primary polling if it exists
    if (nrow(primary) > 0) {
      primary[is.na(primary)] <- 0
      tau <- (primary$sample*timeWeight(primary$mid_date - days, days))
      tau <- tau / sum(tau)
      start.column <- 1 + match("type", colnames(primary))
      if (race.name == "tx-senate-class-ii-2014" & primary.code == "sen_D_p") {
        election <- primary[1, ]
        election[start.column:(ncol(election) - 1)] <- 0
        election$"david-alameel" <- 47.1
        election$"kesha-rogers" <- 21.7
        election$"maxey-scherr" <- 17.7
        election$"harry-kim" <- 8.9
        election$"mike-fjetland" <- 4.6
        primary <- rbind(primary, election)
        tau <- c(tau, 3)
      }
      # get party
      p <- rbind(matrix(as.numeric(unlist(primary[, start.column:(ncol(primary) - 1)])),
                        nrow=length(tau))[, pos])
      est <- colSums(rbind(p*tau))
      est <- 4*est / sum(est)
      probs <- (est + probs) / sum(est + probs)
      # simulate to get win probabilities from vote estimates
      results <- table(max.col(rdirichlet(dir.sims, a0*probs)))
      probs <- 0*probs
      probs[as.numeric(names(results))] <- results / sum(results)
    }
  }
  # flatten.
  probs <- probs + .001
  s <- x$serious[pos] + (1 - max(x$serious[pos]))
  probs*s / sum(probs*s)
}


slugMake <- function(string) {
  # convert to lowercase, replace spaces with hyphens and delete all but
  # hyphens and alphanumeric characters
  string <- tolower(gsub(" ", "-", string))
  return(gsub("[^\\w\\-]", "", string, perl=T))
}


textify <- function(n, d = 1) {
  n <- d*round(n / d)
  ones <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  return(
    if (n < 10 & n == round(n)) {
      ones[n + 1]
    } else {
      as.character(n)
    })
}


timeWeight <- function(days.old, h=NA) {
  # h gives days until election. used to specify poll half-life. as election day
  # gets closer, polls become "stale" more quickly
  if (any(is.na(h))) { 
    h <- -as.numeric(electionDay(2014) - Sys.Date())
  }
  h <- -abs(h)
  h <- min(h, -6)
  h <- 140/(20.945732*1.0075898097^h)
  # h determines half-life of poll in days
  # ensure days.old is positive
  days.old <- abs(days.old)
  1 / 2^(as.numeric(days.old)/h)
}


# remove leading/trailing whitespace from string
trim <- function(x) gsub("^\\s+|\\s+$", "", x)




