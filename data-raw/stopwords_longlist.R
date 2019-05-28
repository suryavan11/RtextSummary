stopwords_longlist =  readLines('data-raw/stopwords_longlist.txt')  
usethis::use_data(stopwords_longlist, overwrite = TRUE)
