# PURPOSE OF SCRIPT
# Read in the turnout data from Elections Canada, and get it 
# into a usable format.
library(dplyr)
library(xlsx)
library(lubridate)

# Read in the sheets corresponding to the 2004, 2006, 2008, and 2011 GEs. 
# It's in an Excel file, but that's Election Canada's fault, not mine.
elect <- "data/raw/41st_GE_turnout_e.xls"
elect2011 <- read.xlsx2(elect, 1, header = FALSE, startRow = 6, endRow = 515)[,1:12]
elect2008 <- read.xlsx2(elect, 2, header = FALSE, startRow = 6, endRow = 515)
elect2006 <- read.xlsx2(elect, 3, header = FALSE, startRow = 6, endRow = 175)
elect2004 <- read.xlsx2(elect, 4, header = FALSE, startRow = 6, endRow = 175)

# Define column names. Two sets must be defined as the available variables
# differ across elections.
varsLate  <- c("province", "sex", "age", "turnout", "turnoutlower", 
	             "turnoutupper", "pctelectorvote", "pctelectorvotelower",
	             "pctelectorvoteupper", "totalelectors", "listedelectors",
	             "totalvotes")
varsEarly <- c("province", "age", "turnout", "turnoutlower", "turnoutupper",
	             "pctelectorvote", "pctelectorvotelower", "pctelectorvoteupper",
	             "totalelectors", "listedelectors", "totalvotes")

colnames(elect2011) <- varsLate
colnames(elect2008) <- varsLate
colnames(elect2006) <- varsEarly
colnames(elect2004) <- varsEarly

# Fill in ID variables (sex, province, age group).
provinces <- c("Canada", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB",
	             "BC", "YT", "NT", "NU", "Atlantic", "Prairies", "Territories")

elect2011 <- elect2011 %>% 
	mutate(year = 2011, 
		sex = rep(c("All", "Male", "Female"), each = 170),
		province = rep(provinces, each=10, times=3)) %>% 
	select(province, sex, year, everything())
elect2008 <- elect2008 %>% 
	mutate(year = 2008,
		sex = rep(c("All", "Male", "Female"), each = 170),
		province = rep(provinces, each=10, times=3)) %>% 
	select(province, sex, year, everything())

elect2006 <- elect2006 %>% 
	mutate(year = 2006, 
		sex = "All", 
		province = rep(provinces, each = 10)) %>% 
	select(province, sex, year, everything())
elect2004 <- elect2004 %>% 
	mutate(year = 2004,
		sex = "All", 
		province = rep(provinces, each = 10)) %>% 
	select(province, sex, year, everything())

# Merge sheets into a single dataframe.
elect <- rbind(elect2011, elect2008, elect2006, elect2004)
elect$age <- as.character(elect$age)
elect[, 5:13] <- sapply(sapply(elect[, 5:13], as.character), as.numeric)

# Clean up the age labels a bit.
elect[elect$age == "1st time2", "age"] <- "First Time Eligible"
elect[elect$age == "Not 1st time3", "age"] <- "Previously Eligible"
elect[elect$age == ">= 75", "age"] <- "75 and Older"

# Calculate the share of votes cast and share of population per age group.
allVotes <- elect %>% 
	filter(age == "All", sex == "All") %>%
	select(province, year, totalelectors, listedelectors, totalvotes) %>%
	rename(allelectors = totalelectors, 
		     alllistedelectors = listedelectors,
		     allvotes = totalvotes)

elect <- left_join(elect, allVotes, by=c("province", "year"))

elect <- elect %>% 
	mutate(propelectors = totalelectors / allelectors * 100,
		     proplistedelectors = listedelectors / alllistedelectors * 100,
		     propvotes = totalvotes / allvotes * 100,
		     contrib = propvotes - propelectors)

# Export the result to an .rda file.
save(elect, "data/clean/elect.rda")