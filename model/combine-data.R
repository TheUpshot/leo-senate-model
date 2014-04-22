####
wd <- paste0(dataDir, "public/_big_assets/")
today <- format(today(), format = "%e-%b-%y")

# update topline number
filename <- paste0(wd, "senate-likelihood.tsv")
df <- read.table(filename, header = T, as.is = T)
df.new <- read.table(paste0(wd, "senate-likelihood-new.tsv"), header = T,
                     as.is = T)
if (tail(df$date, 1) == today) {
  df <- df[-nrow(df), ]
}
df.new$date[nrow(df.new)] <- today
df <- rbind(df, df.new)
write.table(df, file = filename, quote = F, sep='\t', row.names = F,
            col.names = c("date", "dem"))

# update state numbers
filename <- paste0(wd, "senate-likelihood-all.tsv")
df <- read.table(filename, header = T, as.is = T)
df.new <- read.table(paste0(wd, "senate-likelihood-all-new.tsv"), header = T,
                     as.is = T)
if (tail(df$date, 1) == today) {
  df <- df[-nrow(df), ]
}
df.new$date[nrow(df.new)] <- today
df <- rbind(df, df.new)
write.table(df, file = filename, quote = F, sep='\t', row.names = F,
            col.names = c("office", "date", "dem"))