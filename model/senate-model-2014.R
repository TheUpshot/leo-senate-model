
source("Functions.R")
load("data/senateData.RData")

source("../fundamentals/FunctionsFundamentals.R")
source("../fundamentals/fundamentals-source.R")
load("../fundamentals/data/fundydata.rdata")

library(nnet)  
library(zoo)

new.races <- races[sapply(races, function(x) x$cycle == 2014)]
race.names <- names(new.races)
new.races <- new.races[order(race.names)]
races <- races[sapply(races, function(x) x$cycle != 2014)]
old.polls <- pollPrep(races, max.margin=20)  # only needed for pollster ratings / lv adjustment

day.start <- as.numeric(electionDay(2014) - as.Date("2014-01-01"))
day.end <- as.numeric(electionDay(2014) - today())

if (just.today) {
  day.start <- day.end
}

if (n.days == "all") {
  day.seq <- day.start:day.end
} else {
  day.seq <- unique(round(seq(day.start, day.end, length = n.days)))
}

#####
# louisiana - handle possiblility Landrieu gets more than 50% in jungle primary 
load("louisiana/la.RData")

# fec
d <- max(day.seq)
fec.new <- subset(read.csv("../fundamentals/data/2014-fec-update/fec.csv", as.is = T), cycle == 2014)
fec.start <- subset(fec.new, daysOut >= d)
fec.start <- aggregate(list(money = fec.start$money),
                       list(fecid = fec.start$cand_id), sum, na.rm = T)

# initialize
polls <- data.frame()
race.names <- names(new.races)
for (race.name in race.names) {
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
  
  money <- fec.start$money[match(fec.ids, fec.start$fecid)]
  money[is.na(money)] <- 0
  
  d.probs <- probGet(d.names, d.pos, "D", money, days = d, race.polls)
  r.probs <- probGet(r.names, r.pos, "R", money, days = d, race.polls)
  
  probs <- outer(r.probs, d.probs)
  
  if (sum(probs) == 0) {
    probs <- probs + 1
  }
  
  rownames(probs) <- r.names
  colnames(probs) <- d.names
  
  new.races[[match(race.name, names(new.races))]]$probs <- probs
  
  polls <- imputePolls(polls, new.races, race.name)
}
colnames(polls)[match("cycle", colnames(polls))] <- "year"
new.polls <- pollPrep2014(polls, lv.adj)

##### save matchup probabilities by day
prob_list <- setNames(vector("list", length(race.names)), race.names)
for (race.name in race.names) {
  cat(race.name, "\n")
  ind <- match(race.name, names(new.races))
  x <- new.races[[ind]]
  
  fec.ids <- x$fecid
  parties <- x$party
  
  d.pos <- which(parties == "DEM")
  r.pos <- which(parties == "REP")
  
  d.names <- x$slug[d.pos]
  r.names <- x$slug[r.pos]
  
  for (i in seq_along(day.seq)) {
    x <- new.races[[ind]]
    
    d <- day.seq[i]
    
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
    
    d.probs <- probGet(d.names, d.pos, "D", money, days = d, race.polls)
    r.probs <- probGet(r.names, r.pos, "R", money, days = d, race.polls)
    
    probs <- outer(r.probs, d.probs)
    
    if (sum(probs) == 0) {
      probs <- probs + 1
    }
    
    rownames(probs) <- r.names
    colnames(probs) <- d.names
    
    prob_list[[match(race.name, names(prob_list))]][[i]] <- probs
  }
}
probs <- unlist(prob_list)

##### fundamentals
fm_list <- lapply(day.seq, fitModel, q1FECmissing = T)

##### polling averages
m00 <- fm_list[[1]]$model
d.pos <- as.numeric(substr(m00$matchup, 1, 1))
r.pos <- as.numeric(substr(m00$matchup, 3, 3))
old.race <- m00[1, ]$office
# save.image("../output/modelData.RData")
# load("../output/modelData.RData")

results <- data.frame()
h.eff_list <- list()
poll.df_list <- list()
index <- data.frame()
for (i in 1:nrow(m00)) {
  cat(i, "\n")
  
  key.race <- m00[i, ]$office
  
  if (key.race != old.race) {
    x <- new.races[[match(old.race, names(new.races))]]
    polls <- imputePollsNew(polls, new.races, old.race, x$probs, delete=T)
    old.race <- key.race
  }
  
  key.dem <- m00[i, ]$dem
  key.rep <- m00[i, ]$rep
  
  x <- new.races[[match(key.race, names(new.races))]]
  
  kd.pos <- match(key.dem, x$slug)
  kr.pos <- match(key.rep, x$slug)
  
  if (x$probs[cbind(r.pos[i], d.pos[i])] == 0 & 
        all(x$day_in[c(kd.pos, kr.pos)] > 307)) {
    next
  }
  
  index <- rbind(index,
                 data.frame(key.race = key.race,
                            key.dem = key.dem,
                            key.rep = key.rep))
  probs <- 0*x$probs
  probs[cbind(r.pos[i], d.pos[i])] <- 1
  
  polls <- imputePollsNew(polls, new.races, key.race, probs, delete=T)
  new.polls <- pollPrep2014(polls, lv.adj)
  
  p <- prob_list[[match(key.race, names(prob_list))]]
  
  h.eff <- NULL
  poll.df <- NULL
  for (ii in seq_along(day.seq)) {
    temp.probs <- cbind(p[[ii]])
    if (temp.probs[cbind(r.pos[i], d.pos[i])] != 0) {
      d <- day.seq[ii]
      m0 <- fm_list[[ii]]$model[i, ]
      forecast.obj <- pollPredict(2014, d, new.polls, new.races,
                                  race.names = m0$office,
                                  fund.mean = m0$fit,
                                  fund.se = m0$se,
                                  prior.alpha = m0$df / 2)
      res <- forecast.obj$forecast
      res$d <- d
      forecast.obj$h.eff$d <- d
      if (!is.null(forecast.obj$poll.df)) {
        forecast.obj$poll.df$d <- d
      }
      res$key.dem <- key.dem
      res$key.rep <- key.rep
      results <- rbind(results, res)
      h.eff <- rbind(h.eff, forecast.obj$h.eff)
      poll.df <- rbind(poll.df, forecast.obj$poll.df)
    }
  }
  h.eff_list[[length(h.eff_list) + 1]] <- h.eff
  poll.df_list[[length(poll.df_list) + 1]] <- if (is.null(poll.df)) {
    NA
  } else {
    poll.df
  }
}
index$index <- 1:nrow(index)

##### adjust louisiana #####
pos <- which(results$office == "la-senate-class-ii-2014")
p <- (results$prob50 + (1 - results$prob50)*results$prob)[pos]
results$mean[pos] <- qt(p, 2*results$alpha[pos])*results$scale[pos]
results$prob[pos] <- p

# insert matchup probs into dataframe
results$matchupProb <- NA
day.seq <- rev(sort(unique(results$d)))
for (i in seq_along(day.seq)) {
  d <- day.seq[i]
  pos <- which(results$d == d)
  p <- unlist(sapply(prob_list, function(x) x[[i]]))
  p <- p[p != 0]
  results$matchupProb[pos] <- p
}

filename <- format(Sys.time(), "../output/%m%d%H%M.RData")
save(h.eff_list, poll.df_list, prob_list, results, file = filename)

###################### -- 
#####   OUTPUT   #####
###################### -- 

temp <- subset(results, d == day.end)
df <- data.frame(statePostal = toupper(substr(temp$office, 1, 2)),
                 secondary = "",
                 probability = temp$matchupProb,
                 mean = temp$mean,
                 scale = temp$scale,
                 office = temp$office,
                 dem = temp$key.dem,
                 rep = temp$key.rep,
                 stringsAsFactors = F)
df$secondary[grep("(ok|sc).+i{3}", temp$office)] <- "-2"
filename <- paste0(dataDir, "public/_big_assets/matchups.tsv")
write.table(df, quote = F, sep = "\t", file = filename, row.names = F)

matching <- NULL
for (i in seq_along(new.races)) {
  x <- new.races[[i]]
  matching <- rbind(matching, cbind(x$slug, str_trim(x$candidates)))
}
# middle initials, etc.
matching[, 2] <- candidateRename(matching[, 2])
filename <- paste0(dataDir, "public/_big_assets/slug-lookup.tsv")
write.table(matching, quote = F, sep = "\t", file = filename, row.names = F,
            col.names = c("slug", "name"))

results$tau[is.na(results$tau)] <- 0
results$estN11 <- getEstimate(results, "normal")
results$estP11 <- getEstimate(results, "polls")
results$estF11 <- getEstimate(results, "fundys")
results$estN10 <- getEstimate(results, "normal", T, F)
results$estP10 <- getEstimate(results, "polls", T, F)
results$estF10 <- getEstimate(results, "fundys", T, F)
results$estN01 <- getEstimate(results, "normal", F)
results$estP01 <- getEstimate(results, "polls", F)
results$estF01 <- getEstimate(results, "fundys", F)
results$estN00 <- getEstimate(results, "normal", F, F)
results$estP00 <- getEstimate(results, "polls", F, F)
results$estF00 <- getEstimate(results, "fundys", F, F)

## adjust louisiana
pos <- which(results$office == "la-senate-class-ii-2014")
results$estN11[pos] <- qt(results$prob[pos], 2*results$alpha[pos])*results$scale[pos]


##### SIMULATE #####
day.seq <- rev(sort(unique(results$d)))
breakpoints <- c(-1e-8, 5, 15, 35)
load("optimizing/error-spline.RData")

for (snowflakes in c(F)) {
  
  cat("\n\n#####   SIMULATING THINGS   #####\n")
  sim_list_fund <- sim_list <- list()
  
  for (i in seq_along(day.seq)) {
    d <- day.seq[i]
    cat(d, "\n")
    keep <- results[results$d == d, ]
    # outputs some parameters too
    sim_list[[i]] <- electionSimNew(keep, n.sims = n.sims,
                                    s = error.spline$y[match(round(d), error.spline$x)],
                                    breaks = breakpoints)
  }
  
  outputGen(results, sim_list, sim_list_fund, breaks = breakpoints,
            best = F, snowflakes = snowflakes)
  
}
if (just.today) {
  source("combine-data.R")
  filename <- format(Sys.time(), "../output/%m%d%H%M-sims.RData")
  save(sim_list, file = filename)
}

# la tipping-point prob
pos <- match("la-senate-class-ii-2014", race.names)
sims <- sim_list[[length(sim_list)]]
prob <- (sum(34 + colSums(sims > 0) == 50 & sims[pos, ] > 0) +
  sum(34 + colSums(sims < 0) == 51 & sims[pos, ] < 0)) / n.sims
print(paste("## prob that control of senate determined by result in LA:   ", prob))

# current party
currentParty <- sapply(new.races, function(x) {
  x$party[match(1, x$inc)]
})
currentParty["ga-senate-class-ii-2014"] <- "REP"
currentParty["ia-senate-class-ii-2014"] <- "DEM"
currentParty["mi-senate-class-ii-2014"] <- "DEM"
currentParty["ne-senate-class-ii-2014"] <- "REP"
currentParty["ok-senate-class-iii-2014"] <- "REP"
currentParty["sd-senate-class-ii-2014"] <- "DEM"
currentParty["wv-senate-class-ii-2014"] <- "DEM"

# prob that REPS hold every currently-held seat
w <- apply(sims[currentParty == "REP", ] < 0, 2, all)
mean(w)

# prob that all of our forecasts are right:
res <- ddply(results, .(office, d), summarize,
             mean = weighted.mean(mean, matchupProb),
             probs = weighted.mean(prob, matchupProb))
res <- arrange(res, office, desc(d))
res <- subset(res, d == min(day.seq))
dem.call <- res$probs > .5
w <- apply(sims, 2, function(x) all(!xor((x > 0), dem.call)))
mean(w)


#####

# some plots if you want
# par(mfcol = c(4, 4), las = 1)
# race.name <- "nc-senate-class-ii-2014"
# day <- min(results$d)
# sims <- sapply(sim_list, function(x) rowMeans(x > 0))
# for (i in seq_along(race.names)) {
#   par(mar = c(2, 4, 4, 2))
#   race.name <- race.names[i]
#   temp <- subset(results, office == race.name & d == day)
#   pos <- which.is.max(temp$matchupProb)
#   dem <- temp$key.dem[pos]
#   rep <- temp$key.rep[pos]
#   temp <- subset(results, office == race.name & key.dem == dem & key.rep == rep)
#   if (nrow(temp) == 0) next
#   plot.new()
#   plot.window(range(day.seq), c(-30, 30))
#   fit <- temp$mean
#   upper <- fit + qt(.975, df=2*temp$alpha)*temp$scale
#   lower <- fit + qt(.025, df=2*temp$alpha)*temp$scale
#   polygon(c(day.seq, rev(day.seq)), c(rev(lower), upper), border=NA,
#           col=adjustcolor("light gray", .5))
#   upper <- fit + qt(.75, df=2*temp$alpha)*temp$scale
#   lower <- fit + qt(.25, df=2*temp$alpha)*temp$scale
#   polygon(c(day.seq, rev(day.seq)), c(rev(lower), upper), border=NA,
#           col=adjustcolor("light gray", 1))
#   points(rev(day.seq), fit, type="l")
#   race.polls <- polls[polls$office == race.name, ]
#   for (j in seq_along(unique(race.polls$pollster))) {
#     q <- subset(race.polls, pollster == unique(race.polls$pollster)[j])
#     x <- min(day.seq) + max(day.seq) - q$mid_date
#     points(x, q$margin, pch=j)
#   }
#   points(rev(day.seq), temp$prior.mean, type="l", lty=2)
#   points(rev(day.seq), temp$pollAvg, type="l", col="dark blue")
#   axis(2)  
#   title(race.name)
#   title(paste(dem, rep, sep = "   |   "), line = .2, font.main = 1)
#   ###
#   par(mar = c(3, 4, 2, 2))
#   plot.new()
#   plot.window(xlim = range(day.seq), ylim = c(0, 1))
#   points(rev(day.seq), sims[i ,], type = "l", lwd = 2, col = "blue")
#   points(rev(day.seq), 1 - sims[i ,], type = "l", lwd = 2, col = "red")
#   axis(2)
# }
# 
# 
# 
