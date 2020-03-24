## code to prepare `firms` dataset goes here

firms <- unique(df$firm)[-1]

usethis::use_data(firms)
