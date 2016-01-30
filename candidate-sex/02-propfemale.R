# Using the cleaned candidate sex data, calculate the proportion of candidates
# in a given region/party of each sex.
library(dplyr)

load("data/clean/candidatesex.rda")

bigparties <- c("Conservative Party of Canada",
				        "Liberal Party of Canada",
				        "New Democratic Party")

provfemale <- candidates %>% count(province, gender)
totalcand <- candidates %>% group_by(province) %>% summarise(total = n())

# Aggregate for each province.
provfemale <- left_join(provfemale, totalcand, by="province") %>%
	mutate(proportion = (1 - n / total) * 100) %>%
	filter(gender == "Male") %>%
	select(province, proportion)

# Province by party.
partyfemale <- candidates %>% count(province, affil, gender) %>%
	filter(affil %in% bigparties)
partytotalcand <- candidates %>% filter(affil %in% bigparties) %>%
	group_by(province, affil) %>% 
	summarise(total = n())

# Aggregate for each province and party.
partyfemale <- left_join(partyfemale, partytotalcand, 
	                       by=c("province", "affil")) %>%
	mutate(proportion = (1 - n / total) * 100) %>%
	filter(gender == "Male") %>%
	select(province, affil, proportion)

save(provfemale, file = "data/clean/provfemale.rda")
write.csv(provfemale, file = "data/csv/provfemale.csv")