library(readr)
library(tidyverse)
library(fs)

setwd('data-raw/opinosis')
  

opinosis_txt <- dir_ls(path = 'topics')


opinosis_txt1 = opinosis_txt %>%
  purrr::map_chr(read_file)%>% 
  cbind.data.frame(.)

colnames(opinosis_txt1) = 'text'
opinosis_txt1$topics = str_replace_all( row.names(opinosis_txt1), '(topics/)|(\\.txt\\.data)', '')
rownames(opinosis_txt1) = NULL

opinosis_summaries <- dir_ls(path = 'summaries-gold',glob = '*gold$', recursive = TRUE)

opinosis_summaries1 = opinosis_summaries %>%
  purrr::map_chr(read_file)%>% 
  cbind.data.frame(.)

colnames(opinosis_summaries1) = 'summary'
opinosis_summaries1$topics = str_replace_all( row.names(opinosis_summaries1), 
                                    '(.*/)|(\\.[0-9]\\.gold$)', '')
opinosis_summaries1$summary_number = as.numeric(str_extract_all( row.names(opinosis_summaries1), 
                                                      '(?<=\\.)[0-9](?=\\.)', ''))
rownames(opinosis_summaries1) = NULL

opinosis_summaries2 = opinosis_summaries1%>%
  spread(summary_number, summary, sep='_' )


opinosis  = opinosis_txt1%>%
  inner_join(opinosis_summaries2, by = 'topics')

usethis::use_data(opinosis, overwrite = TRUE)
