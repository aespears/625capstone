library(dplyr)
library(tibble)
library(words)
library(ggplot2)
library(chron)
library(stringr)
library(pubtheme)

### Note: Raw data is too large and too much to upload to gradescope!
### Can send separately, or is compiled and read in Rdata files generated here

# read in all csvs
r <- lapply(list.files('RawData/pages'),
       function(x) return (read.csv(paste0('RawData/pages/', x))))

names(r) <- list.files('RawData/pages')[1:length(r)]

# Add column to dataframes for the date
r <- lapply(names(r), function(name) {
  df <- r[[name]]
  df$date <- as.Date(gsub('.csv', '', name))
  return(df)
})

# Bind all into big dataframe
big_table <- bind_rows(r)

big_table <- big_table |> 
  mutate(weekday = weekdays(date),
         len = nchar(answers),
         percent_vowel = nchar(gsub('[^AEIOU]', '', answers))/nchar(answers)) |>
  group_by(answers) |> mutate(ans_freq = n()) |> ungroup() |> group_by(clues) |> 
  mutate(clue_freq = n()) |> ungroup() |> arrange(date, X)


# Scrabble score - check avg "letter commonness"
big_table$scrabble <- sapply(big_table$answers, function(x){
  vecx <- tolower(unlist(strsplit(x, '')))
  score <- 1*sum(grepl('e|a|i|o|n|r|t|l|s|u', vecx)) + 
    2*sum(grepl('d|g', vecx)) + 3*sum(grepl('b|c|m|p', vecx)) + 
    4*sum(grepl('f|h|v|w|y', vecx)) + 5*sum(grepl('k', vecx)) + 
    8*sum(grepl('j|x', vecx)) + 10*sum(grepl('q|z', vecx))
  
  return(score/length(vecx))
})

# Use `words` library to see if clues are "dictionary words"
big_table$real_word <- big_table$answers %in% toupper(words)

# How many words in clue? 
big_table$clue_wc <- sapply(big_table$clues, 
                            function(x) length(unlist(strsplit(x, ' '))))

# Clue and answer ratios
big_table <- big_table |> group_by(clues, answers) |> 
  mutate(ca_pair = n()) |> ungroup() |>
  mutate(clue_ratio = ifelse(ca_pair > 1, ca_pair/clue_freq, 0), 
         ans_ratio = ca_pair/ans_freq)

big_table |> arrange(-ans_freq) |> select(answers, ans_freq) |> distinct()

big_table |> arrange(-clue_freq) |> select(clues, answers, ca_pair, clue_freq) |> distinct()

saveRDS(big_table, file = 'Data/big_table.rds')

###############################################################################

difficulty_table <- data.frame(
  date = gsub('.txt', '', list.files('RawData/difficulty/'))
  )

get_dif <- function(x){
  d <- try(readLines(paste0('RawData/difficulty/', x, '.txt'))[3], silent = T)
  if (inherits(page, 'try-error')){
    cat("Error, skipping.")
    return(NA)
  }
  return(d)
}


difs <- sapply(difficulty_table$date, get_dif)
difficulty_table$dif <- sapply(difs, 
                               function(x) gsub(' Difficulty ', '', 
                                                gsub('Median Solve .*', '', x)))
difficulty_table$time <- sapply(difs, function(x) gsub('Median Solver.*', '',
                                                       gsub('.*Median Solve Time ', '', x))) 
difficulty_table$solver <- sapply(difs, function(x) gsub('.*Median Solver ', '', x))

difficulty_table$date <- as.Date(difficulty_table$date)

difficulty_table <- difficulty_table |> 
  mutate(time = ifelse(str_count(time, ':') == 1, paste0("00:", time), time)) |>
  filter(time != "NA") 

difficulty_table$time <- sapply(difficulty_table$time, function(x){
                                (unlist(strsplit(x, ':')) |> as.numeric() * c(3600, 60, 1) ) |> sum()})


dif_order <- c("Very Easy ", "Easy ", "Average ", "Hard ", "Very Hard ")
difficulty_table$dif <- factor(difficulty_table$dif, levels = dif_order)



difficulty_table |>  ggplot(aes(x = format(date, '%Y'), fill = dif)) +
  geom_bar() + theme_pub() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # + facet_wrap(~weekdays(date)) 



difficulty_table |>  sample_n(1000) |>
  ggplot(aes(x = date, y = time, color = dif)) + geom_point() + facet_wrap(~wd, scales = 'free')

ggplot(big_table, aes(x = ans_ratio, y = clue_ratio, color = real_word)) + 
  geom_jitter(height = 0.01, width = 0.01, alpha = 0.5)


saveRDS(difficulty_table, file = 'Data/dif_table.rds')

##############################################################################

joint_table <- left_join(big_table, difficulty_table, by = join_by(date))
saveRDS(joint_table, file = 'Data/joint_table.rds')
