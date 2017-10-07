# compile_kss <- function(namespace, date_begin = FALSE, date_end = FALSE){
  library(dplyr)
  library(ggplot2)
  library(svglite)
  library(knitr)
  library(bigrquery)
  library(grid)
  library(gtable)
  library(scales)
  library(ggrepel)
  library(gridExtra)
  library(purrr)
  Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc")
             
  error_check <- function(argument, delayed_response, error_msg){
    if(argument == TRUE) {
      response <- error_msg
      system(paste('python /home/rstudio/scripts/slack/scripts/python/delayed_response.py', delayed_response, '\'', error_msg, '\''))
      stop()
    }
  }
  
  request_input <- commandArgs(trailingOnly = TRUE)
  print(request_input)
  namespace <- request_input[1]
  date_begin.format <- unname(lubridate::guess_formats(request_input[2], c("Ymd", "mdY", "BdY", "Bdy", "bdY", "bdy", "mdy"))[1])
  date_end.format <- unname(lubridate::guess_formats(request_input[3], c("Ymd", "mdY", "BdY", "Bdy", "bdY", "bdy", "mdy"))[1])
  date_begin <- lubridate::as_date(request_input[2], date_begin.format)
  date_end <- lubridate::as_date(request_input[3], date_end.format)
  user_name <- request_input[4]
  delayed_response <- request_input[5]
  
  
  error_check(all(date_end < date_begin, !is.na(date_begin), !is.na(date_end)), 
              delayed_response, 
              paste0('You entered a date ending before the date begins: ', date_begin, ' (date begin)   ', date_end, ' (date end)'))
  
  error_check(all(nchar(namespace) <= 3, is.na(date_begin), is.na(date_end)), 
              delayed_response, 
              'Date inputs are incorrect.  Acceptable formats are:  YYYY-/MM-/DD, MM-/DD-/YY, MM-/DD-/YYYY.')
  
  
  setwd('/home/rstudio/scripts/kss/')
  source('./functions/top_n_messages_engagement.R')
  source('./functions/top_bottom_campaigns.R')
  source('./functions/goal_som_by_channel.R')
  source('./functions/largest_volume_sends.R')
  source('./functions/wow_metrics.R')
  source('./functions/campaign_x_y_performance.R')
  source('./functions/function_create_table_output.R')
  source('./functions/function_find_metric.R')
  source('./functions/campaign_performance_data.R')
  source('./functions/function_date_to_week_range.R')
  
  #' borrowed functions
  source('/home/rstudio/scripts/qbr/functions/campaign_summary.R')
  source('/home/rstudio/scripts/qbr/functions/compare_campaign_messaged_control_engagement.R')
  source('/home/rstudio/scripts/qbr/functions/compare_goal_rates_by_algo.R')
  source('/home/rstudio/scripts/qbr/functions/ctr_message_length.R')
  source('/home/rstudio/scripts/qbr/functions/compare_algo_type_vs_control.R')
  source('/home/rstudio/scripts/qbr/functions/goal_count_over_time.R')
  source('/home/rstudio/scripts/qbr/functions/compare_app_opens.R')
  source('/home/rstudio/scripts/qbr/functions/goal_completion_times.R')
  source('/home/rstudio/scripts/qbr/functions/user_push_retention.R')
  source('/home/rstudio/scripts/qbr/functions/function_is_error.R')
  #load connections
  source('/home/rstudio/scripts/db_connection.R')
  
  
  file_path <- '/home/rstudio/gdrive/Reports/KSS/'
  
  
  #' gather namespace data from database connection
  flush.console()
  print('gathering namespace data from database')
  daily_counters <- dbGetQuery(db_connection, paste0("select * from bq.counters where table_id = '", namespace, "'"))
  assign('daily_counters', value = daily_counters, pos = 1)
  daily_campaign <- dbGetQuery(db_connection, paste0("select * from bq.campaigns where key like '", namespace, "%'"))
  assign('daily_campaign', value = daily_campaign, pos = 1)
  goals <- dbGetQuery(db_connection, paste0("select * from bq.goals where table_id = '", namespace, "'"))
  assign('goals', value = goals, pos = 1)
  
  error_check(any(nrow(daily_counters) == 0, nrow(daily_campaign) == 0, nrow(goals) == 0), 
              delayed_response, 
              'No data found.  Make sure inputs are correct.')
  
  #check if namespace used in benchmark has data
  #if it does not, then error out
  if(!(namespace %in% unique(daily_counters$table_id))){
    stop(paste0(toupper(namespace), ' does not exist is data'))
  } else if (sum(daily_counters$total[daily_counters$table_id == namespace]) < 1000){
    stop(paste0(toupper(namespace), ' does not have enough data to create KSS'))
  }
  
  
  if(date_begin == FALSE){
    date_begin <- lubridate::floor_date(seq(from = Sys.Date(), by = '-1 year', length.out = 2)[2], unit = 'year')
  } else {
    date_begin <- as.Date(date_begin)
  }
  if(date_end == FALSE){
    date_end <- lubridate::ceiling_date(seq(from = Sys.Date(), by = '-1 month', length.out = 2)[1], unit = 'month') - 1
  } else {
    date_end <- as.Date(date_end)
  }
  
  
  
  flush.console()
  print('campaign_summary')
  campaign_summary_graph <- campaign_summary(namespace = namespace,
                                             date_begin = date_begin,
                                             date_end = date_end,
                                             return_chart = FALSE)
  
  #' create tmp file of campaign_summary_graph
  png('./output/campaign_summary_graph.png', width = 700, height = 600, units = 'px')    
  grid.draw(campaign_summary_graph)
  dev.off()
  
  flush.console()
  print('largest volume sends')
  largest_volume_sends_graph <- largest_volume_sends(namespace = namespace, 
                                                     date_begin = date_begin, 
                                                     date_end = date_end)
  
  flush.console()
  print('campaign performance: revenue')
  campaign_performance_revenue_gtable <- campaign_performance_data(namespace = namespace, 
                                                                     metric = 'delivered, campaign_value', 
                                                                     channel = 'push, email, in_app', 
                                                                     date_begin = date_begin, 
                                                                     date_end = date_end, 
                                                                     table_output = TRUE)
  
  flush.console()
  print('campaign performance: engagement rate')
  campaign_performance_engagement_gtable <- campaign_performance_data(namespace = namespace, 
                                                                     metric = 'delivered, engagement_rate', 
                                                                     channel = 'push, email, in_app', 
                                                                     date_begin = date_begin, 
                                                                     date_end = date_end, 
                                                                     table_output = TRUE)
  flush.console()
  print('ALL campaign performance: goal rate')
  all_campaign_performance_goal_rate_gtable <- campaign_performance_data(namespace = namespace, 
                                                                                     metric = 'delivered, goal', 
                                                                                     goal_value = TRUE, 
                                                                                     channel = 'push, email, in_app', 
                                                                                     date_begin = date_begin, 
                                                                                     date_end = date_end, 
                                                                                     table_output = TRUE)
  
  flush.console()
  print('campaign performance: goal rate')
  campaign_performance_goal_gtable <- campaign_performance_data(namespace = namespace, 
                                                               metric = 'delivered, goal_rate', 
                                                               channel = 'push, email, in_app', 
                                                               date_begin = date_begin, 
                                                               date_end = date_end, 
                                                               table_output = TRUE)
  
  flush.console()
  print('campaign performance: revenue + engagement rate')
  campaign_performance_revenue_engagement_gtable <- campaign_performance_data(namespace = namespace, 
                                                                             metric = 'delivered, engagement_rate, campaign_value', 
                                                                             channel = 'push, email, in_app', 
                                                                             date_begin = date_begin, 
                                                                             date_end = date_end, 
                                                                             table_output = TRUE)
  
  flush.console()
  print('PUSH campaign performance: goals, ctr, engagement')
  push_campaign_performance_goals_ctr_engagement_gtable <- campaign_performance_data(namespace = namespace, 
                                                                             metric = 'delivered, engagement_rate, campaign_value, clickthru_rate, goal_rate', 
                                                                             channel = 'push', 
                                                                             date_begin = date_begin, 
                                                                             date_end = date_end, 
                                                                             table_output = TRUE)
  
  flush.console()
  print('PUSH campaign performance: attrition')
  push_campaign_performance_attrition_gtable <- campaign_performance_data(namespace = namespace, 
                                                                                    metric = 'delivered, uninstall_rate, optout_rate', 
                                                                                    channel = 'push', 
                                                                                    date_begin = date_begin, 
                                                                                    date_end = date_end, 
                                                                                    table_output = TRUE)
  
  flush.console()
  print('PUSH WoW metrics')
  push_wow_metrics_graph <- wow_metrics(namespace = namespace, 
                                         channels = 'push', 
                                         date_begin = date_begin, 
                                         date_end = date_end)
  
  flush.console()
  print('EMAIL campaign performance: goals, uctr, uor, engagement')
  email_campaign_performance_goals_ctr_engagement_graph <- campaign_performance_data(namespace = namespace, 
                                                                                    metric = 'delivered, engagement_rate, campaign_value, uniqueclickthru_rate, uniqueopens_rate, goal_rate', 
                                                                                    channel = 'email', 
                                                                                    date_begin = date_begin, 
                                                                                    date_end = date_end, 
                                                                                    table_output = TRUE)
  
  flush.console()
  print('EMAIL campaign performance: attrition')
  email_campaign_performance_attrition_graph <- campaign_performance_data(namespace = namespace, 
                                                                                    metric = 'delivered, unsubscribe_rate', 
                                                                                    channel = 'email', 
                                                                                    date_begin = date_begin, 
                                                                                    date_end = date_end, 
                                                                                    table_output = TRUE)
  
  flush.console()
  print('EMAIL WoW metrics')
  email_wow_metrics_graph <- wow_metrics(namespace = namespace, 
                                         channels = 'email', 
                                         date_begin = date_begin, 
                                         date_end = date_end)
  
  flush.console()
  print('IN APP campaign performance: goals, ctr, engagement')
  inapp_campaign_performance_goals_ctr_engagement_graph <- campaign_performance_data(namespace = namespace, 
                                                                                     metric = 'delivered, goal', 
                                                                                     goal_value = TRUE, 
                                                                                     channel = 'in_app', 
                                                                                     date_begin = date_begin, 
                                                                                     date_end = date_end, 
                                                                                     table_output = TRUE)
  
  flush.console()
  print('Goal SOM by channel')
  goal_som_by_channel_graph <- goal_som_by_channel(namespace = namespace,
                                                   date_begin = date_begin,
                                                   date_end = date_end)
  
  flush.console()
  print('EXPERIENCES: new user onboarding')
  experiences_new_user_graph <- campaigns_xy(namespace = namespace, 
                                             campaign_name = 'new user on-boarding', 
                                             date_begin = date_begin, 
                                             date_end = date_end)
  
  flush.console()
  print('EXPERIENCES: inactive users')
  experiences_inactive_graph <- campaigns_xy(namespace = namespace, 
                                             campaign_name = 'inactive users', 
                                             date_begin = date_begin, 
                                             date_end = date_end)

  flush.console()
  print('Top performing campaigns')
  top_campaigns_gtable <- top_bottom_campaigns(namespace = namespace,
                                              n = 5,
                                              type = 'top',
                                              channels = 'all',
                                              date_begin = date_begin,
                                              date_end = date_end,
                                              draw_table = FALSE)
  
  png('./output/top_campaigns.png', width = 1500, height = 600, units = 'px')    
  grid.draw(top_campaigns_gtable)
  dev.off()
  
  flush.console()
  print('Bottom performing campaigns')
  bottom_campaigns_gtable <- top_bottom_campaigns(namespace = namespace,
                                                 n = 5,
                                                 type = 'bottom',
                                                 channels = 'all',
                                                 date_begin = date_begin,
                                                 date_end = date_end,
                                                 draw_table = FALSE)
  
  png('./output/bottom_campaigns.png', width = 1500, height = 600, units = 'px')    
  grid.draw(bottom_campaigns_gtable)
  dev.off()
   

  #' check if directory exists for namespace
  if(file.exists(paste0(file_path, namespace)) == FALSE){
    dir.create(paste0(file_path, namespace))
  }
  
  print(getwd())
  #' remove any existing KSS reports
  system(paste0("rm /home/rstudio/gdrive/Reports/KSS/", namespace, "/*"))
  
  #' knit KSS report together
  rmarkdown::render('/home/rstudio/scripts/kss/scripts/render_html.R',
                    params = list(namespace = namespace_param, date_begin = date_begin, date_end = date_end),
                    output_dir = paste0(file_path, namespace, '/'),
                    output_file = paste0('KSS_', 
                                         format(Sys.Date(), format = '%b%d%Y'), 
                                         '_daterange_',
                                         date_begin,
                                         '_',
                                         date_end,
                                         '.html'))
  
  
  #' push file to googledrive
  system(paste0("/home/rstudio/go/bin/drive push -ignore-name-clashes -exclude-ops \"delete\" -force -quiet ", 
                '"/home/rstudio/gdrive/Reports/KSS/', namespace, "/", '"'))
  
  #' get file URL
  url_location <- system(paste0("/home/rstudio/go/bin/drive url ", 
                                '"/home/rstudio/gdrive/Reports/KSS/', namespace, "/", 
                                'KSS_', format(Sys.Date(), format = '%b%d%Y'), '_daterange_', date_begin, '_', date_end, '.html"'), intern = TRUE)
  url_location <- strsplit(url_location, ': ')[[1]][2]
  
  #' notify user_name on slack
  httr::POST('https://slack.com/api/chat.postMessage', body = list(token='xoxb-135728745299-qVSLyow0JyGzr5UGHr9HqH8G', 
                                                                   channel = paste0('@', user_name),
                                                                   text = paste0('Ok, your report is ready... ', url_location),
                                                                   username = 'blake_the_bot',
                                                                   as_user = FALSE,
                                                                   icon_emoji = ':robot_face:'))
  
  rm(daily_counters, daily_campaign, goals)
  gc()
  gc()
  
  
# }
