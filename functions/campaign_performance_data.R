campaign_performance_data <- function(namespace, metric, goal_value = FALSE, channel, date_begin = FALSE, date_end = FALSE, table_output = FALSE){
  
  #' get goal mapping
  goal_mapping <- suppressWarnings(read.table(file = '/home/rstudio/scripts/kss/data/customer_settings/connect_goals'))
  # goal_mapping <- stringi::stri_split_fixed(goal_mapping$V1, '\t')
  # goal_mapping <- as.data.frame(goal_mapping, stringsAsFactors = FALSE)
  # tmp_name <- as.list(unname(goal_mapping[1,]))
  names(goal_mapping) <- c('Goal', 'Value')
  # goal_mapping <- goal_mapping[2,]
  # goal_mapping <- goal_mapping %>% tidyr::gather(goal, value, 1:2)
  
  metric_list <- as.list(gsub(' ', '', stringi::stri_split_fixed(metric, ',')[[1]]))
  metric_list_original <- as.list(gsub(' ', '', stringi::stri_split_fixed(metric, ',')[[1]]))
  goal_name_list <- if(goal_value != FALSE) as.list(gsub(' ', '', stringi::stri_split_fixed(goal_value, ',')[[1]]))
  channel_list <- as.list(gsub(' ', '', stringi::stri_split_fixed(channel, ',')[[1]]))
  #goal_value <- if(goal_value != FALSE) as.list(gsub(' ', '', stringi::stri_split_fixed(goal_value, ',')[[1]]))
  
  if(any(metric_list == 'delivered')) metric_list <- as.list(c(metric_list, 'ia_delivered'))
  if(any(metric_list == 'campaign_value')) metric_list <- as.list(c(metric_list, 'email_campaign_value', 'ia_campaign_value'))
  if(any(metric_list == 'engagement_rate')) metric_list <- as.list(c(metric_list, 'engaged'))
  if(any(metric_list == 'goal_rate')) metric_list <- as.list(c(metric_list, 'goal'))
  if(any(metric_list == 'uninstall_rate')) metric_list <- as.list(c(metric_list, 'uninstalled_ghost', 'delivered_ghost'))
  if(any(metric_list == 'optout_rate')) metric_list <- as.list(c(metric_list, 'opt_outs', 'delivered_ghost'))  
  if(any(metric_list == 'clickthru_rate')) metric_list <- as.list(c(metric_list, 'clicks', 'delivered'))  
  if(any(metric_list == 'uniqueclickthru_rate')) metric_list <- as.list(c(metric_list, 'unique_clicks', 'email_unique_clicks', 'delivered'))  
  if(any(metric_list == 'uniqueopens_rate')) metric_list <- as.list(c(metric_list, 'email_unique_opens', 'delivered'))  
  if(any(metric_list == 'unsubscribe_rate')) metric_list <- as.list(c(metric_list, 'unsubs', 'delivered'))  
  
  
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
  
  
  #' get goal counts, to be used later in filtering and graphing
  if(goal_value != FALSE){
    goals_tmp <- goals[goals$table_id == namespace & goals$when >= date_begin & goals$when <= date_end,][c('key2', 'when', 'goal_name', 'count')]
    goals_tmp$name <- 'goal'
    goals_tmp$goal_high_action <- NA
    goals_tmp$goal_high_action <- goal_mapping$Goal[match(goals_tmp$goal_name, goal_mapping$Value)]
    goals_tmp <- goals_tmp %>% map_if(is.factor, as.character)
    goals_tmp$goal_high_action[is.na(goals_tmp$goal_high_action)] <- 'Behavioral Goal'
    goals_tmp <- as.data.frame(goals_tmp, stringsAsFactors = FALSE)
    
    #find unique goal values and append to metric_list
    #metric_list <- c(as.list(unique(as.character(goal_mapping$Goal))), 'Behavioral Goal', metric_list)
  } else {
    goals_tmp <- data.frame(key2 = NA, when = NA, name = NA, goal_name = NA, count = NA, goal_high_action = NA)
  }

  
  daily_counters %>%
    filter(namespace == table_id,
           date >= date_begin,
           date <= date_end,
           name %in% metric_list,
           channel %in% channel_list,
           label == 'non-control') %>%
    select(key, date, name, total) -> tmp_counters
  
  #' pseudo way to get week count.  need to pass to function date_to_week_range
  #' and determine if grouping should be weekly, bi-weekly, or monthly
  week_count <- round(length(unique(tmp_counters$date)) / 7)
  unit_type <- ifelse(week_count <= 8, 'week', 'month')
  
  
  if(goal_value != FALSE){
    tmp_counters %>%
      left_join(goals_tmp, by = c('key' = 'key2', 'date' = 'when', 'name' = 'name')) -> tmp_counters
    
    metric_list <- c(as.list(unique(na.omit(as.character(tmp_counters$goal_high_action)))), metric_list)
  }
  
  # if(goal_value != FALSE){
  #   group_char <- c('week', 'name', 'goal_high_action')
  # } else {
    group_char <- c('week', 'name')
  # }
  
  
  tmp_counters %>%
    mutate(week = date_to_week_range(unit_type, date),
           name = ifelse(name == 'ia_delivered', 'delivered', name),
           name = ifelse(name %in% c('ia_campaign_value', 'email_campaign_value'), 'campaign_value', name),
           name = ifelse(name == 'email_unique_clicks', 'unique_clicks', name),
           goal_name = ifelse(name == 'goal', goal_value, NA)) %>%
    mutate(name = ifelse(name == 'goal' & goal_value == TRUE, goal_high_action, name)) %>%
    filter(!is.na(name),
           !is.na(total)) %>%
    group_by_(.dots = group_char) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    mutate_(.dots = find_metric(metric_list)) %>%
    tidyr::gather(name, total, -week) %>%
    mutate(total = ifelse(name == 'campaign_value', total / 100, total)) %>%
    assign('table_conversion', value = ., pos = 1) %>%
    filter(name %in% metric_list_original |  grepl('_rate', name)) %>%
    mutate(total_rescale = ifelse(grepl('rate', name), total * (summary(.$total[.$name == 'delivered'])[4] - 0) / max(.$total[.$name != 'delivered']) + (0 - min(.$total[.$name != 'delivered'], na.rm = TRUE)), total)) %>%
    mutate(name = ifelse(name == 'campaign_value', 'Revenue', name)) %>%
    mutate(name = ifelse(name == 'engagement_rate', 'Engagement Rate', name)) %>%
    mutate(name = ifelse(name == 'clickthru_rate', 'Click-Thru Rate', name)) %>%
    mutate(name = ifelse(name == 'goal', 'Goals', name)) %>%
    mutate(name = ifelse(name == 'goal_rate', 'Goal Completion Rate', name)) %>%
    mutate(name = ifelse(name == 'uninstall_rate', 'Uninstall Rate', name)) %>%
    mutate(name = ifelse(name == 'delivered', 'Delivered', name)) %>%
    mutate(name = ifelse(name == 'behavioralgoal_rate', 'Behavioral Goal Rate', name)) %>%
    mutate(name = ifelse(name == 'purchasedealgoal_rate', 'Purchase Deal Goal Rate', name)) %>%
    mutate(name = ifelse(name == 'purchasegoal_rate', 'Purchase Goal Rate', name)) %>%
    mutate(name = ifelse(name == 'optout_rate', 'Opt-Out Rate', name)) %>%
    mutate(name = ifelse(name == 'uniqueopens_rate', 'Unique Open Rate', name)) %>%
    mutate(name = ifelse(name == 'uniqueclickthru_rate', 'Unique Click-Thru Rate', name)) %>%
    mutate(name = ifelse(name == 'unsubscribe_rate', 'Unsubscribe Rate', name)) %>%
    mutate(name = ifelse(name == 'bookclassgoal_rate', 'Book Class Goal Rate', name)) %>%
    ggplot(., aes(x = week, y = total, group = name, color = name)) +
    geom_area(data = function(x){x[x$name == 'Delivered', ]}, aes(x = week, y = total, group = name), fill = 'grey', color = 'grey') +
    geom_line(data = function(x){x[x$name != 'Delivered', ]}, aes(x = week, y = total_rescale, group = name, color = name)) +
    geom_text(data = function(x){x[x$name != 'Delivered', ]}, aes(x = week,
                                                                  y = total_rescale,
                                                                  label = ifelse(total < 1, stringr::str_c(round(total, 3) * 100, '%'), 
                                                                                 ifelse(name == 'Revenue', stringr::str_c('$', formatC(total, format = 'd', big.mark = ',')), round(total))),
                                                                  color = name),
              size = 3, vjust = -1, show.legend = FALSE) +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
    scale_color_manual(values = c('Revenue' = "#28235E","#2FD3B8","#FCC212", "Behavioral Goal Rate" = "#FF7070", "#6E66CB","Purchase Deal Goal Rate" = "#597FD9", "#6995FF","Purchase Goal Rate" = "#4141A8",
                                  "#524BA1","Uninstall Rate" = "#597FD9","Goal Completion Rate" = "#e88e27","Click-Thru Rate" = "#6D6D6D","Engagement Rate" = "#8FADFF","#151B28","#4e79a5","#f18f3b",
                                  "Unsubscribe Rate" = "#af0a64","Book Class Goal Rate" = "#e0585b","Unique Click-Thru Rate" = "#5aa155","Unique Open Rate" = "#edc958","Opt-Out Rate" = "#77b7b2")) +
    labs(x = '',
         y = 'Messages Delivered\n\n') +
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
          legend.title = element_blank()) -> p1_chart
  
  #' pass metric_list_original to function_create_table_output
  #' this creates a table of metrics and is aligned below the graph
  if(table_output){
    p2_chart <- create_table_output(table = table_conversion, metrics = c(metric_list_original, paste0(unique(tolower(goal_mapping$Goal)), 'goal_rate'), 'behavioralgoal_rate'))
  }
  
  
  #' determine how many unique time periods are shown on x-axis
  #' if the count is too large, we need to specify a new width when
  #' graphs are generated below
  week_count <- length(unique(table_conversion$week))
  
  
  #' generate graphs
  #' p1_chart is always generated and we need to check if p2_chart (table_grob) exists
  if(exists('p2_chart')){
    grid.arrange(p1_chart, p2_chart, heights = c(10, 3), widths = ifelse(week_count < 8, 10, 40))
  } else {
    grid.arrange(p1_chart, heights = 10, widths = ifelse(week_count < 8, 10, 40))
  }
}


#ifelse(name == 'campaign_value', scales::dollar(round(total)), 
# ifelse(total < 1, scales::percent(total)),
# total)