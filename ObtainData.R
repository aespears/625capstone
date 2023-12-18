library(rvest)
library(dplyr)

### Note: Raw data is too large and too much to upload to gradescope!
### Can send separately, or is compiled and read in Rdata files generated in PrepData.R


# Start with last page scraped
start_date <- as.Date(list.files('RawData/pages')[length(list.files('RawData/pages'))])
end_date <- as.Date('2023/11/21')

counter = 0
for (i in start_date:end_date){
  path = paste0(format(as.Date(i), '%Y-%m-%d'), '.csv')
  if(!(path %in% list.files('RawData/pages'))){
  print(path)
    
  day = format(as.Date(i), "%m/%d/%Y")

  
  page <- read_html(paste0('https://www.xwordinfo.com/Crossword?date=', 
                           as.character(day)))
  
  w <- html_elements(page, 'div.numclue') %>%
    html_elements('div') %>% 
    html_text()
  
  clues <- w[!grepl('+\\d', w)] %>% gsub(':[^:]*$', '', .)
  answers <- w[!grepl('+\\d', w)] %>% gsub('.*: ', '', .)
  
  f <- data.frame(clues, answers)
  print(head(f))
  write.csv(f, file = paste0('RawData/pages/', path))
  Sys.sleep(runif(1, 0.5, 20))
  counter <- counter + 1
  print(counter)
  # Account for connection timeouts by adding in a longer pause every 15 steps
  if (counter %% 15 == 0){Sys.sleep(runif(1, 20, 30)); print("Sleeping")}
  }
}


########### Getting Statistics #############
# start_date <- as.Date('1993/11/21')
start_date <- as.Date(list.files('RawData/difficulty')[length(list.files('RawData/difficulty'))]) + 1
end_date <- as.Date('2023/11/21')


for (i in start_date:end_date){
  day <- format(as.Date(i), '%Y-%m-%d')
  
  
  page <- try(read_html(paste0('https://www.xwstats.com/puzzles/', 
                               as.character(day))), silent= T)
  
  if (inherits(page, 'try-error')){
    cat("Error, skipping.")
    next
  }
  w <- html_elements(page, '.content') %>% html_elements('div')
  
  diff <- w %>% html_text()
  
  writeLines(gsub("\\s+", " ", diff[4:10]), paste0('RawData/difficulty/', day, '.txt'))
}

