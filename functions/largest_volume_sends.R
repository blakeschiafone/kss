largest_volume_sends <- function(namespace, n = 5, date_begin = FALSE, date_end = FALSE, draw_table = FALSE){
  
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
  
  
  daily_counters %>% 
    filter(table_id == namespace,
           date >= date_begin,
           date <= date_end,
           name %in% c('delivered', 'ia_delivered', 'email_unique_opens', 'engaged', 'goal'),
           label == 'non-control') %>%
    select(key, name, channel, algo_type, total) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
    group_by(key, name, channel, algo_type) %>%
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
    select(key, display_name) -> tmp_campaign_df
  
  tmp_counters_df %>% 
    left_join(tmp_goals_df, by = c('key' = 'key2')) %>%
    filter(!(is.na(goal_name))) %>% 
    left_join(tmp_campaign_df, by = c('key' = 'key')) %>%
    ungroup() %>%
    select(-key) %>% 
    group_by(display_name, name, channel, algo_type, goal_name) %>%
    summarize(total = sum(total, na.rm = TRUE),
              goal_count = sum(count, na.rm = TRUE)) %>%
    tidyr::spread(goal_name, goal_count) -> tmp_merged
  
  
  #' get top 5 campaigns based on message volume
  tmp_merged %>% 
    filter(name %in% c('delivered', 'engaged', 'email_unique_opens')) %>% 
    select(display_name, name, channel, algo_type, total) %>% 
    group_by(display_name, name, channel, algo_type) %>%
    summarize(total = sum(total, na.rm = TRUE)) %>%
    tidyr::spread(name, total) %>%
    arrange(desc(delivered)) %>% 
    filter(!is.na(engaged)) %>%
    ungroup() %>% 
    top_n(n, delivered) %>%
    mutate(rate = ifelse(channel == 'push', engaged / delivered, 
                         ifelse(channel == 'email', email_unique_opens / delivered,
                            engaged / delivered)),
           rate = paste(' ', scales::percent(rate)),
           delivered = paste(' ', formatC(delivered, big.mark = ',', format = 'd')),
           channel = case_when(
             .$channel == 'email' ~ '  Email',
             .$channel == 'push' ~ '  Push',
             .$channel == 'in_app' ~ '  In App'),
           algo_type = case_when(
             .$algo_type == 'so' ~ 'SendOptimally',
             .$algo_type == 'miq' ~ 'Message Optimization',
             .$algo_type == 'tiq' ~ 'Time Optimization',
             .$algo_type == 'none' ~ 'No Optimization',
             .$algo_type == 'miq/so' ~ 'Message Opt. & SendOptimally',
             .$algo_type == 'miq/tiq' ~ 'Message Opt. & Time Opt.'
           )) -> tmp_top5_campaigns
  
  
  #' get goal count for tmp_top5_campaigns
  #' find matching keys from top5 and tmp_campaign
  matching_keys <- tmp_campaign_df[tmp_campaign_df$display_name %in% tmp_top5_campaigns$display_name,]
  tmp_goals_df <- tmp_goals_df[tmp_goals_df$key2 %in% matching_keys$key,]
  #' get campaign name by matching keys
  tmp_goals_df$display_name <- tmp_campaign_df$display_name[match(tmp_goals_df$key2, tmp_campaign_df$key)]
  tmp_goals_df %>% 
    group_by(display_name, goal_name) %>% 
    summarize(count = sum(count, na.rm = TRUE)) %>%
    mutate(goal_name = stringr::str_replace(goal_name, 'email_', 'Email: '),
           goal_name = stringr::str_replace(goal_name, 'ia_', 'In App: '),
           goal_name = ifelse(!grepl('Email:|In App:', goal_name), stringr::str_c('Push: ', goal_name), goal_name)) %>%
    mutate(goal_name = toupper(stringr::str_replace(goal_name, '-', ' '))) %>%
    arrange(desc(count)) -> tmp_goals_df
  #' collapse goals into a separated line using newline separator
  #' this will be joined with tmp_top5_campaigns for eventual html table conversion
  tmp_goals_df %>% 
    select(display_name, goal_name, count) %>% 
    group_by(display_name, goal_name) %>% 
    summarize(count = sum(count, na.rm = TRUE)) %>%
    summarize(goal_name = paste(paste(goal_name, formatC(count, big.mark = ',', format = 'd'), sep = ' - '), collapse = '\n')) -> tmp_goals_df
    
  tmp_top5_campaigns$goals_achieved <- tmp_goals_df$goal_name[match(tmp_top5_campaigns$display_name, tmp_goals_df$display_name)]
  #' rearrange tmp_top5 for desired column indexing
  tmp_top5_campaigns <- tmp_top5_campaigns[,c('display_name', 'channel', 'algo_type', 'delivered', 'goals_achieved', 'rate')]
  names(tmp_top5_campaigns) <- c('Campaign', 'Channel', 'Optimization Type', 'Reach', 'Goals Achieved', 'UOR (Email)\nEng. Rate (Push)')
  
  
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
  
}
