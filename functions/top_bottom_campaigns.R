#' function to determine message length
#' if the length is too long, currently defined as > 100 characters
#' then it will be split into multiple lines
function_newline_string <- function(x, split_length = 100){
  x_length <- nchar(x)
  
  if(x_length > split_length){
    x_splits <- floor(x_length / split_length) #' how many newline characters
    x_split_index <- seq(from = split_length, by = split_length, length.out = x_splits) #' ideal newline indexes
    x_white_index <- as.vector(gregexpr(' ', x)[[1]]) #' available newline indexes
    
    #'find closest match of ideal newline indexes (x_split_index) and
    #'available newline indexes (x_white_index)
    x_newline_index <- findInterval(x_split_index, x_white_index)
    x_newline_index <- x_white_index[x_newline_index]
    
    #' replace x_newline_index with newline carriage
    for(i in seq_along(x_newline_index)){
      substr(x, x_newline_index[i], x_newline_index[i]) <- '\n'
    }
  }
  x
}



top_bottom_campaigns <- function(namespace, n = 5, type = 'top', channels = 'all', date_begin = FALSE, date_end = FALSE, draw_table = FALSE){
  
  #' Reset device screen
  if(!is.null(dev.list())){
    dev.off()
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
  
  #' determine channel filter
  channels <- switch(channels,
         all = c('push', 'email', 'in_app'),
         push = c('push'),
         email = c('email'),
         in_app = c('in_app'))
  
  daily_counters %>% 
    filter(table_id == namespace,
           date >= date_begin,
           date <= date_end,
           name %in% c('delivered', 'ia_delivered', 'email_unique_opens', 'engaged', 'goal'),
           channel %in% channels,
           label == 'non-control') %>%
    select(key, name, channel, total) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
    group_by(key, name, channel) %>%
    summarize(total = sum(total, na.rm = TRUE)) %>%
    filter(total > 0) -> tmp_counters_df
  
  goals %>%
    filter(table_id == namespace,
           when >= date_begin,
           when <= date_end) %>%
    select(key2, goal_name, count) %>%
    group_by(key2, goal_name) %>%
    summarize(count = sum(count, na.rm = TRUE)) -> tmp_goals_df
  
  daily_campaign %>%
    filter(grepl(namespace, key)) %>%
    select(key, display_name, message_text) -> tmp_campaign_df
  
  tmp_counters_df %>% 
    left_join(tmp_goals_df, by = c('key' = 'key2')) %>%
    filter(!(is.na(goal_name))) %>% 
    left_join(tmp_campaign_df, by = c('key' = 'key')) %>%
    ungroup() %>%
    select(-key) %>%
    group_by(display_name, name, channel, goal_name) %>%
    summarize(total = sum(total, na.rm = TRUE),
              goal_count = sum(count, na.rm = TRUE)) %>%
    tidyr::spread(goal_name, goal_count) -> tmp_merged
  
  
  warning_flag <- 0
  #' check if tmp_merged is empty, if so do not continue
  if(nrow(tmp_merged) == 0){
    warning_flag <- 1
  }
  
  if(warning_flag == 0){
    #' get top 5 campaigns based on engagement rate
    tmp_merged %>% 
      filter(name %in% c('delivered', 'engaged', 'email_unique_opens', 'goal')) %>% 
      select(display_name, name, channel, total) %>% 
      group_by(display_name, name, channel) %>%
      summarize(total = sum(total)) %>%
      tidyr::spread(name, total) %>%
      arrange(desc(delivered)) %>% 
      ungroup() %>% 
      mutate(totalprop = sum(delivered, na.rm = TRUE)) %>%
      mutate(totalprop = log(delivered) / log(totalprop)) %>%
      mutate(rate = ifelse(channel %in% c('push', 'email'), engaged / delivered, goal / delivered),
             rate = round(rate, 3),
             delivered = paste(' ', formatC(delivered, big.mark = ',', format = 'd')),
             channel = case_when(
               .$channel == 'email' ~ '  Email',
               .$channel == 'push' ~ '  Push',
               .$channel == 'in_app' ~ '  In App')
      ) %>% 
      filter(!is.na(rate), rate <= 1,
             totalprop >= .50) %>% 
      top_n(ifelse(type == 'top', n, -n), rate) %>%
      arrange_(.dots = paste0(ifelse(type == 'top', 'desc(rate)', 'rate'))) %>%
      mutate(rate = paste(' ', scales::percent(rate))) -> tmp_top5_campaigns
    
    
    #' get message copy for tmp_top5_campaigns
    #' find matching keys from top5 and tmp_campaign
    matching_keys <- tmp_campaign_df[tmp_campaign_df$display_name %in% tmp_top5_campaigns$display_name,]
    
    
    #' collapse goals into a separated line using newline separator
    #' this will be joined with tmp_top5_campaigns for eventual html table conversion
    matching_keys %>%
      filter(message_text != 'CONTROL',
             message_text != '<none>') %>%
      mutate(message_text = purrr::map(message_text, function_newline_string)) %>%
      group_by(display_name) %>%
      summarize(message_text = paste(message_text, collapse = '\n\n')) -> matching_keys
    
    tmp_top5_campaigns$message_text <- matching_keys$message_text[match(tmp_top5_campaigns$display_name, matching_keys$display_name)]
    
    #' rearrange tmp_top5 for desired column indexing
    tmp_top5_campaigns <- tmp_top5_campaigns[,c('display_name', 'message_text', 'channel', 'delivered', 'rate')]
    names(tmp_top5_campaigns) <- c('Campaign', 'Message Text', 'Channel', 'Delivered', 'Eng. Rate (Push, Email)\nGoal Rate (In App)')
    
    if(draw_table == TRUE){
      grid.draw(tableGrob(tmp_top5_campaigns, 
                          theme = ttheme_default(core = list(
                            fg_params = list(
                              hjust = 0, x = 0.02))),
                          rows = NULL))
    } else {
      tableGrob(tmp_top5_campaigns, 
                          theme = ttheme_default(core = list(
                            fg_params = list(
                              hjust = 0, x = 0.02))),
                          rows = NULL)
    }
  } else {
    message('No data to continue')
  }
  
}
