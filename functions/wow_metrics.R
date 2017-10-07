metric_calculation <- function(channels){
  
  if(all(channels == 'push')){
    p1 <- setNames(list(engagement_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('engaged'),
                            b = as.name('delivered')))), 'engagement_rate')
    p2 <- setNames(list(uniqueclickthru_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('unique_clicks'),
                            b = as.name('delivered')))), 'uniqueclickthru_rate')
    p3 <- setNames(list(revenue_messagedelivered_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('campaign_value'),
                            b = as.name('delivered')))), 'revenue_messagedelivered_rate')
    p4 <- setNames(list(goal_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('goal'),
                            b = as.name('delivered')))), 'goal_rate')
    p5 <- setNames(list(revenue_messageclick = 
                          substitute(round((a/100)/b, digits = 5), list(
                            a = as.name('campaign_value'),
                            b = as.name('unique_clicks')))), 'revenue_messageclick_rate')
    
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
    p4 <- setNames(list(revenue_messageclick = 
                          substitute(round((a/100)/b, digits = 5), list(
                            a = as.name('campaign_value'),
                            b = as.name('email_unique_clicks')))), 'revenue_messageclick_rate')
    p5 <- setNames(list(revenue_messagedelivered_rate = 
                          substitute(round((a/100)/b, digits = 5), list(
                            a = as.name('campaign_value'),
                            b = as.name('delivered')))), 'revenue_messagedelivered_rate')
    p6 <- setNames(list(goal_rate = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('goal'),
                            b = as.name('delivered')))), 'goal_rate')
    
  }
  
  
  if(all(channels == 'push')){
    return(c(p1, p2, p3, p4, p5))
  } else {
    return(c(p1, p2, p3, p4, p5, p6))
  }
}



wow_metrics <- function(namespace, channels = 'push', date_begin = FALSE, date_end = FALSE){
  
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
             name %in% c('delivered', 'ia_delivered', 'engaged', 'email_unique_opens', 
                         'email_unique_clicks', 'email_unique_opens', 'campaign_value', 
                         'ia_campaign_value', 'email_campaign_value', 'goal',
                         'unique_clicks'),
             channel %in% channels) %>% 
      select(name, date, total) %>%
      mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
             name = ifelse(name %in% c('ia_campaign_value', 'email_campaign_value'), 'campaign_value', name),
             date = date_to_week_range(unit_type = 'week', date)) %>%
      group_by(date, name) %>%
      summarize(total = sum(total)) %>%
      tidyr::spread(name, total) %>%
      mutate_(.dots = metric_calculation(channels)) %>%
      mutate(campaign_value = campaign_value / 100) %>%
      ungroup() %>%
      slice((n()-1):n()) %>%
      select(date, delivered, campaign_value, contains('_rate')) -> tmp_df
    
    #' calculate lift WoW
    tmp_df[nrow(tmp_df)+1, ] <- NA #' add blank row
    tmp_df[nrow(tmp_df),1] <- 'Lift' #' label blank row
    tmp_df[3,c(2:ncol(tmp_df))] <- (tmp_df[2,c(2:ncol(tmp_df))] / tmp_df[1,c(2:ncol(tmp_df))]) - 1 #' calculate lift
    
    #' gather data for graphic output
    tmp_df %>% tidyr::gather(name, value, 2:ncol(tmp_df)) -> tmp_df
    
    #'rename variables for graphic output
    tmp_df$name <- sapply(1:nrow(tmp_df), function(x) switch(tmp_df$name[x],
                          delivered = 'Delivered WoW',
                          campaign_value = 'Revenue WoW',
                          engagement_rate = 'Engagement % WoW',
                          uniqueopen_rate = 'Unique Opens % WoW',
                          uniqueclickthru_rate = 'Unique Click % WoW',
                          goal_rate = 'Goal % WoW',
                          revenue_messagedelivered_rate = ifelse(all(channels == 'email'), 'RPE WoW', 'RPM WoW'),
                          revenue_messageclick_rate = 'RPUC WoW'
                          ))
    
    #' add value label used for geom_text
    tmp_df$label <- ifelse(grepl('%', tmp_df$name) & tmp_df$date != 'Lift', paste0(plyr::round_any(tmp_df$value, .001) * 100, '%'),
                           ifelse(grepl('Revenue|RP', tmp_df$name) & tmp_df$date != 'Lift', paste0('$', formatC(tmp_df$value, big.mark = ',', format = 'fg')),
                                  ifelse(tmp_df$date != 'Lift', formatC(tmp_df$value, big.mark = ',', format = 'd'), NA)))
    tmp_df$label[is.na(tmp_df$label)] <- paste0((plyr::round_any(tmp_df$value[is.na(tmp_df$label)], .01) * 100), '%')
    
    
    #' create table of lift values for each metric
    #' append to tmp_df$name column, because when graphed it will show
    #' the lift in facet header for each metric
    tmp_names <- tmp_df[tmp_df$date == 'Lift',][c('name', 'label')]
    tmp_names$new_label <- paste0(tmp_names$name,'\n','Lift: ', tmp_names$label)
    tmp_df$new_name <- tmp_names$new_label[match(tmp_df$name, tmp_names$name)]
    
    #' plot
    ggplot(tmp_df[tmp_df$date != 'Lift',], aes(x = date, y = value, fill = date, label = value, width = .70)) +
      geom_bar(stat = 'identity') +
      facet_wrap(~ new_name, scales = 'free_y', strip.position = 'bottom') +
      scale_fill_manual(values = c('#999999ff', '#66308dff')) +
      #scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
      theme_bw() + 
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            #rect = element_blank(),
            line = element_blank(),
            panel.spacing.y =unit(2, "lines"),
            strip.text.y = element_text(size = 11),
            legend.direction = 'horizontal',
            legend.position = 'bottom',
            legend.title = element_blank()) +
      labs(x = '',
           y = '') +
      geom_text(aes(x = date, y = value, label = label, 
                    vjust = -.5), size = 3.5)
      
  } else {
    message('In-App channel not supported since there is not an engagement rate')
  }
  
  
}
