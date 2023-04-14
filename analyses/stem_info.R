library(readr)
UCI_STEM_definitions_Sheet2 <- read_csv("data/UCI STEM definitions - Sheet2.csv")
View(UCI_STEM_definitions_Sheet2)

table()

tolower(term_features$major_1)


terms=get_term_data(ids=students$mellon_id)

names(table(UCI_STEM_definitions_Sheet2$`CIP Code`))
names(table(terms$major_cip_code_1))
