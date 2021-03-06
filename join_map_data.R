library(dplyr)

data <- read.csv(file = "data/data.csv", header = T, sep = ";", stringsAsFactors = F)
d2 <- read.csv(file = "data/join_ct_on_map.csv", header = T, sep = ";", stringsAsFactors = F) 

data <- inner_join(data, d2, by = "census_tract")

write.table(data, "data/mp_data.csv", quote = F, row.names = F, sep=";", fileEncoding = "UTF-8")
