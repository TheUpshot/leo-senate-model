####
wd <- paste0(dataDir, "public/_big_assets/")
today <- format(today(), format = "%e-%b-%y")

# update topline number
filename <- paste0(wd, "senate-likelihood.tsv")
if (file.exists(filename)) {
	df <- read.table(filename, header = T, as.is = T)
	df <- df[df$date != today, ]
	df.new <- read.table(paste0(wd, "senate-likelihood-new.tsv"), header = T,
		as.is = T)
	df.new$date[nrow(df.new)] <- today
	df <- rbind(df, df.new)
	write.table(df, file = filename, quote = F, sep='\t', row.names = F,
		col.names = c("date", "dem"))
} else {
	file.copy(paste0(wd, "senate-likelihood-new.tsv"), to = filename)
}

# update state numbers
filename <- paste0(wd, "senate-likelihood-all.tsv")
if (file.exists(filename)) {
	df <- read.table(filename, header = T, as.is = T)
	df <- df[df$date != today, ]
	df.new <- read.table(paste0(wd, "senate-likelihood-all-new.tsv"), header = T,
		as.is = T)
	df.new$date <- today
	df <- rbind(df, df.new)
	write.table(df, file = filename, quote = F, sep='\t', row.names = F,
		col.names = c("office", "date", "dem"))
} else {
	file.copy(paste0(wd, "senate-likelihood-all-new.tsv"), to = filename)
}
