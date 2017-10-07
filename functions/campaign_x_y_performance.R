campaigns_xy <- function(namespace, campaign_name, date_begin = FALSE, date_end = FALSE){
  
  
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
  
  daily_campaign %>%
    filter(grepl(namespace, key),
           grepl(campaign_name, display_name, ignore.case = TRUE)) %>%
    select(key) %>%
    as.list(.) -> campaign_tmp
  
  daily_counters %>% 
    filter(table_id == namespace, 
           label == 'non-control', 
           date >= date_begin, 
           date <= date_end, 
           name %in% c('delivered', 'ia_delivered', 'engaged', 'goal'),
           key %in% campaign_tmp$key) %>% 
    select(key, name, total, algo_type, campaign_type) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive')) %>%
    left_join(daily_campaign, by = c("key" = "key")) %>%
    select(display_name, name, campaign_type, algo_type, total) %>%
    group_by(display_name, name, campaign_type, algo_type) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    mutate(engagement_rate = engaged / delivered,
           conversion_rate = goal / delivered,
           ranked_rate = (goal + engaged) / delivered) %>%
    filter(delivered > 1000, engagement_rate < 1, conversion_rate < 1) %>%
    mutate(display_name2 = ifelse(grepl('10.13.16-Email Welcome', display_name), 'Welcome Email (10/13/16)', display_name)) %>%
    mutate(display_name2 = ifelse(grepl('03.06.2017-Email Welcome', display_name), 'Welcome Email (03/06/17)', display_name2)) %>%
    mutate(display_name2 = ifelse(grepl('Push #1', display_name), 'Push #1 (Welcome Book)', display_name2)) %>%
    mutate(display_name2 = ifelse(grepl('Push #2', display_name), 'Push #2 (Reminder Book)', display_name2)) %>%
    mutate(display_name2 = ifelse(grepl('Fun Facts', display_name), 'Fun Facts', display_name2)) %>%
    mutate(display_name2 = ifelse(grepl('Clone_1 10-19-2016 Inactive Users 10 Days - Book Again-Book Again - General', display_name), 'General (10/19/16)', display_name2)) %>%
    mutate(display_name2 = ifelse(grepl('V2 03.09.2017 - Inactive Users 10 Days - Book Again-Book Again - General', display_name), 'General (03/09/17)', display_name2)) %>%
    arrange(desc(ranked_rate)) %>%
    ungroup() %>%
    top_n(50) %>%
    mutate(total_goal = sum(goal) / sum(delivered),
           total_engagement = sum(engaged) / sum(delivered)) %>%
    
    ggplot(., aes(x = conversion_rate, 
                  y = engagement_rate, 
                  # color = display_name2, 
                  group = display_name2,
                  label = display_name2)) + 
    geom_point(alpha = .7) +
    scale_x_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1)) +
    #geom_text(aes(x = conversion_rate, y = engagement_rate, label = display_name2), size = 4, position = position_nudge(x = 3)) +
    geom_text_repel(aes(label = display_name2)) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      rect = element_blank(),
      line = element_blank(),
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5, size = 11)
    ) +
    guides(size = FALSE) +
    labs(
      x = 'Goal Conversion Rate',
      y = 'Engagement Rate'
      # title = 'Inactive Users 10 Days, Book Again'
    )
}
