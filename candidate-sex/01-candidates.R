# Read in data giving candidate names and districts. Classify candidates by
# sex by using their names to predict their sex. Output a cleaned dataset
# enhanced by the inclusion of candidate sex.
library(dplyr)
library(ggplot2)
library(gender)

#######################
### Candidate Names ###
#######################

# Read in the candidate data, province codes, district population.
candidates    <- read.csv("data/raw/candidates.csv")
provincecodes <- read.csv("data/raw/provincecodes.csv", colClasses = "character")
districtpop   <- read.csv("data/raw/districtpop.csv", 
	header=FALSE, 
	col.names=c("districtnum", "districtname", "population"),
	colClasses=c("numeric", "character", "numeric"))

colnames(candidates) <- c("districtnum", "districtname", "fdistrictname",
	                        "affil", "faffil", "familyname", "firstname", 
	                        "initial", "phone", "dateconfirm", "timeconfirm")

candidates <- candidates %>% select(districtnum, districtname, affil,
		                                familyname, firstname)
candidates[, 2:5] <- sapply(candidates[, 2:5], as.character)

# Lots of people have middle names and middle initials, or hyphenated names.
# Split these into first and middle names; the latter can be used in cases
# where the first doesn't yield a match to a gender.
candidates$firstname <- gsub("-", " ", candidates$firstname)
candidates$middlename <- NA
candidates$middlename[grep("^.* ", candidates$firstname)] <- 
	gsub("^.* ", "", candidates$firstname[grep("^.* ", candidates$firstname)])
candidates$firstname <- gsub(" .*$", "", candidates$firstname)

# Merge with electoral district information (provincecodes, districtpop).
candidates$provcode <- substr(candidates$districtnum, 1, 2)
candidates <- left_join(candidates, provincecodes, by="provcode")
candidates <- left_join(
	candidates, 
	districtpop %>% select(districtnum, population), 
	by = "districtnum")

candidates <- candidates %>% 
	select(districtnum, districtname, population, everything())

####################
### French Names ###
####################

# Read in the Acadian/French-Canadian names and clean them up.
frnames <- read.csv("data/raw/namesraw.csv", header=FALSE, 
		                colClasses = "character", col.names = "name") %>%
	filter(nchar(name) > 1)

frnames$genderfr <- sapply(strsplit(frnames$name, "\\("), "[[", 2)
frnames$genderfr <- gsub("m)", "male", frnames$genderfr)
frnames$genderfr <- gsub("f)", "female", frnames$genderfr)
frnames$genderfr <- gsub(" ", "", frnames$genderfr)
frnames$genderfr <- gsub("mfemale", "female", frnames$genderfr)
frnames$name     <- gsub(" .*$", "", frnames$name)

##########################
### Identifying gender ###
##########################

# The predictions from this set are likely more accurate, hence, they will
# be given precedence in predicting sex.
engnames <- unique(rbind(gender(candidates$firstname, method = "ssa"),
		                     gender(candidates$middlename, method = "ssa"))) %>%
	select(name, gender) %>%
	rename(firstname = name)

# English matches
candidates <- left_join(candidates, engnames, by = "firstname")
candidates <- left_join(candidates, 
	engnames %>% rename(middlename = firstname, gendermid = gender), 
	by = "middlename")

# French matches
candidates <- left_join(candidates, 
	frnames %>% rename(firstname = name), 
	by = "firstname")
candidates <- left_join(candidates, 
	frnames %>% rename(middlename = name, genderfrmid = genderfr), 
	by = "middlename")

candidates[ , c("gender", "gendermid", "genderfr", "genderfrmid")] <-
	sapply(candidates[ , c("gender", "gendermid", "genderfr", "genderfrmid")], 
		     as.factor)

# Now it's time to figure out which gender classification to use. Things are
# about to get sloppy.
candidates$finalgender <- NA

nagender      <- is.na(candidates$gender)
nagendermid   <- is.na(candidates$gendermid)
nagenderfr    <- is.na(candidates$genderfr)
nagenderfrmid <- is.na(candidates$genderfrmid)

# English first name match, no other matches
matchEngOnly <- nagendermid & nagenderfr & nagenderfrmid
candidates$finalgender[matchEngOnly] <- candidates$gender[matchEngOnly]

# French first name match and no english first name match
matchFrOnly <- nagender & !nagenderfr
candidates$finalgender[matchFrOnly] <- candidates$genderfr[matchFrOnly]

# First name and middle name match gender.
matchFirstMiddle <- !nagender & !nagendermid & 
                    (candidates$gender == candidates$gendermid)
candidates$finalgender[matchFirstMiddle] <- candidates$gender[matchFirstMiddle]

# French first and middle names match gender.
matchFrFirstMiddle <- !nagenderfr & !nagenderfrmid & 
                      (candidates$genderfr == candidates$genderfrmid)
candidates$finalgender[matchFrFirstMiddle] <- 
	candidates$genderfr[matchFrFirstMiddle]

# French and English first names match gender.
matchEngFrFirst <- !nagender & !nagenderfr & 
                   (candidates$gender == candidates$genderfr)
candidates$finalgender[matchEngFrFirst] <- candidates$gender[matchEngFrFirst]

# No match for first name, French and English middle names match gender.
matchEngFrMiddle <- nagender & nagenderfr & !nagenderfrmid & !nagendermid & 
	                  (candidates$gendermid == candidates$genderfrmid)
candidates$finalgender[matchEngFrMiddle] <- 
	candidates$gendermid[matchEngFrMiddle]

# Remaining Jeans and Claudes are men, hopefully (except Claude Boucher).
candidates$finalgender[is.na(candidates$finalgender) & 
                       (candidates$firstname == "Jean")] <- "male"
candidates$finalgender[is.na(candidates$finalgender) & 
                       (candidates$firstname == "Claude")] <- "male"
candidates$finalgender[candidates$firstname == "Claude" & 
                       candidates$familyname == "Boucher"] <- "female"

# Remaining names.
malenames <- c("Dany", "Akhtar", "Alupa", "Amarjeet", "Amarjit", "Arezki",
	"Bal", "Bien", "Birinder", "Bronek", "Chungsen", "Corneliu", "Devinder",
	"Émile", "Éric", "Érick", "Étienne", "Fang", "Fayçal", "Fobozof", "Fodé",
	"Geng", "Gord", "Harbaljit", "Harinderpal", "Harjit", "Inky", "Jagdish",
	"Jasbir", "Jasvir", "Jati", "JiCi", "Jobson", "KM", "Kornelis", "Lynn",
	"Mauril", "Naval", "Ninder", "Parm", "Patrice", "Pouyan", "Réjean", "Rhéal",
	"Riba", "Robin", "Roelof", "Sahajvir", "Senthi", "Seonaigh", "Slavko",
	"Soulèye", "Sucha", "Sukh", "Sukhdev", "Toban", "W.", "Wladyslaw", "Dougal")

femalenames <- c("Adaoma", "Affine", "Aino", "Bardish", "Carol", "Chrystia", 
	"Djaouida", "Dolmine", "Élaine", "Élise", "Ève", "Gudie", "Jaymini", "Kédina",
	"Laurence", "Lysane", "Marilène", "Mebreate", "Meghan", "Mylène", "Rathika", 
	"Rehya", "Rosannie", "Sacki", "Simmi")

malenames <- iconv(malenames, from="UTF-8", to="LATIN1")
femalenames <- iconv(femalenames, from="UTF-8", to="LATIN1")

candidates$finalgender[is.na(candidates$finalgender) & 
                       candidates$firstname %in% malenames] <- "male"
candidates$finalgender[is.na(candidates$finalgender) & 
                       candidates$firstname %in% femalenames] <- "female"

# Factorize finalgender.
candidates$finalgender <- factor(Hmisc::capitalize(candidates$finalgender), 
	                               levels = c("Male", "Female"))

##############
### Output ###
##############

# The five gender columns aren't necessary for the final output, retain
# only the final gender. Rename it simply as gender for simplicity.
candidates <- candidates %>% 
	select(everything(), -gender, -gendermid, -genderfr, -genderfrmid) %>%
	rename(gender = finalgender)

save(candidates, file = "data/clean/candidatesex.rda")
write.csv(candidates, file = "data/csv/candidatesex.csv")
