date_to_week_range <- function(unit_type, date){
  suppressPackageStartupMessages(library(lubridate))
  
  week_begin <- lubridate::floor_date(date, unit = unit_type)
  week_end <- week_begin + 6
  
  # paste(week_begin, 'to', week_end)
  paste(ifelse(unit_type == 'week', 'Week: ', 'Month: '), week_begin)
}
