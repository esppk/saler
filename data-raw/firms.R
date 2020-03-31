## code to prepare `firms` dataset goes here

firms <- unique(pydf$firm)

usethis::use_data(pydf)

usethis::use_data(firms)
