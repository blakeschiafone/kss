goal_som_by_channel <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
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
           name %in% c('goal'),
           label == 'non-control') %>%
    select(date, channel, total) %>%
    mutate(date = date_to_week_range(unit_type = 'week', date)) %>%
    mutate(channel = case_when(
      .$channel == 'push' ~ 'Push',
      .$channel == 'email' ~ 'Email',
      .$channel == 'in_app' ~ 'In App')
    ) %>%
    group_by(date, channel) %>%
    summarize(total = sum(total, na.rm = TRUE)) %>%
    mutate(som = total / sum(total)) -> tmp_df
  
  #' get min/max for each channel
  tmp_df %>% 
    group_by(channel) %>% 
    summarize(min = min(som), 
              max = max(som)) %>%
    tidyr::gather(name, value, 2:3) -> tmp_df_min_max
  tmp_df_min_max$date <- tmp_df$date[match(tmp_df_min_max$value, tmp_df$som)]
  
  ggplot(tmp_df, aes(x = date, y = som, group = channel, color = channel)) + 
  geom_line(lwd = 1.5) +
  geom_label(data = tmp_df_min_max, aes(x = date, y = value, group = channel, label = scales::percent(value)), 
             size = 4, vjust = -.5, show.legend = FALSE) +
  geom_hline(aes(yintercept = .5), color = 'grey', linetype = 'dashed') +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(), limits = c(0,1)) +
  scale_color_manual(values = c('Push' = '#ae0a45ff', 'Email' = '#66308dff', 'In App' = '#27ab7eff')) +
  labs(x = '',
       y = '% Goals Achieved by Channel\n\n') +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1, angle = 25),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        rect = element_blank(),
        line = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.title = element_blank())
}


# daily_counters %>% 
#   filter(table_id == namespace,
#          date >= date_begin,
#          date <= date_end,
#          name %in% c('goal', 'delivered', 'ia_delivered'),
#          label == 'non-control') %>%
#   select(date, name, channel, total) %>%
#   mutate(date = date_to_week_range(date),
#          name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
#   mutate(channel = case_when(
#     .$channel == 'push' ~ 'Push',
#     .$channel == 'email' ~ 'Email',
#     .$channel == 'in_app' ~ 'In App')
#   ) %>%
#   group_by(date, name, channel) %>%
#   summarize(total = sum(total, na.rm = TRUE)) %>%
#   tidyr::spread(name, total) %>%
#   mutate(som = goal / sum(goal)) -> tmp_df
# 
# 
# ggplot(tmp_df, aes(x = date, y = som, group = channel, color = channel)) +
#   geom_line(aes(color = channel, size = delivered)) +
#   #geom_label(data = tmp_df_min_max, aes(x = date, y = value, group = channel, label = scales::percent(value)),
#   #           size = 4, vjust = -.5, show.legend = FALSE) +
#   geom_hline(aes(yintercept = .5), color = 'grey', linetype = 'dashed') +
#   scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(), limits = c(0,1)) +
#   scale_color_manual(values = c('Push' = '#ae0a45ff', 'Email' = '#66308dff', 'In App' = '#27ab7eff')) +
#   labs(x = '',
#        y = '% Goals Achieved by Channel\n\n') +
#   theme_bw() +
#   theme(axis.text.x = element_text(hjust = 1, angle = 25),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         rect = element_blank(),
#         line = element_blank(),
#         legend.position = 'bottom',
#         legend.direction = 'horizontal',
#         legend.title = element_blank())
