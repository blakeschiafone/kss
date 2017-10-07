metric_calculation <- function(channels){
  
  if(all(channels == 'push')){
    p1 <- setNames(list(engagement_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('engaged'),
                            b = as.name('delivered')))), 'engagement_rate')
  } else if(all(channels == 'email')){
    p1 <- setNames(list(engagement_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('engaged'),
                            b = as.name('delivered')))), 'engagement_rate')
    p2 <- setNames(list(uniqueopen_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('email_unique_opens'),
                            b = as.name('delivered')))), 'uniqueopen_rate')
    p3 <- setNames(list(uniqueclickthru_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('email_unique_clicks'),
                            b = as.name('delivered')))), 'uniqueclickthru_rate')
  } else if(all(channels == c('push', 'email'))){
    p1 <- setNames(list(engagement_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('engaged'),
                            b = as.name('delivered')))), 'engagement_rate')
  }
  
  
  if(all(channels == 'email')){
    returned_list <- c(p1, p2, p3)
    return(returned_list)
  } else {
    returned_list <- p1
    return(p1)
  }
}



top_n_messages_engagement <- function(namespace, n = 5, channels = 'all', date_begin = FALSE, date_end = FALSE){
  
  
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
                     all = c('push', 'email'),
                     push = c('push'),
                     email = c('email'),
                     in_app = c('in_app'))
  
  warning_flag <- 0
  if(any(channels == 'in_app')){
    warning_flag <- 1
  }
  
  if(warning_flag == 0){
    daily_counters %>% 
      filter(table_id == namespace, 
             label == 'non-control', 
             date >= date_begin, 
             date <= date_end, 
             name %in% c('delivered', 'ia_delivered', 'engaged', 'email_unique_opens', 'email_unique_clicks', 'email_unique_opens'),
             channel %in% channels) %>% 
      select(key, channel, campaign_type, name, total, algo_type) %>%
      mutate(algo_type = recode(algo_type,
                                'tiq' = 'Time Optimization',
                                'so' = 'Send Optimally',
                                'miq' = 'Message Optimization',
                                'miq/tiq' = 'Message Optimization + Time Optimization',
                                'none' = 'No Optimization',
                                'miq/so' = 'Message Optimization + Send Optimally')) %>%
      mutate(name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
      left_join(daily_campaign, by = c("key" = "key")) %>%
      select(display_name, name, channel, campaign_type, algo_type, total, message_text) %>%
      group_by(display_name, name, channel, campaign_type, algo_type, message_text) %>%
      summarize(total = sum(total)) %>%
      tidyr::spread(name, total) %>%
      mutate_(.dots = metric_calculation(channels)) %>%
      filter(is.numeric(engagement_rate),
             delivered > 200, 
             message_text != '<none>') %>%
      ungroup() %>%
      top_n(n, engagement_rate) %>%
      arrange(desc(engagement_rate)) %>%
      kable(.)
  } else {
    message('In-App channel not supported since there is not an engagement rate')
  }
}
