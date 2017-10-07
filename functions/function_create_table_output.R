create_table_output <- function(table, metrics){
  library(gridExtra)
  
  
  df <- as.data.frame(table)
  set_levels <- c('delivered', 'engaged', 'engagement_rate', 'clickthru_rate', 'uniqueclickthru_rate', 'goal', 'goal_rate', 'uninstall_rate', 'optout_rate', 
                  'uniqueopens_rate', 'unsubscribe_rate', 'campaign_value', 'delivered_ghost', 'opt_outs', 'uninstalled_ghost', 'unique_clicks', 'email_unique_opens', 
                  'unsubs', 'purchasegoal_rate', 'purchasedealgoal_rate', 'behavioralgoal_rate', 'bookclassgoal_rate', 'Purchase', 'PurchaseDeal', 'BookClass', 'Behavioral Goal')
  new_names <- c('delivered' = 'Messages Delivered', 'engagement_rate' = 'Engagement Rate', 'clickthru_rate' = 'Click-Thru Rate', 'goal' = 'Goals',
                 'goal_rate' = 'Goal Completion Rate', 'uninstall_rate' = 'Uninstall Rate', 'optout_rate' = 'Opt-out Rate', 'uniqueopens_rate' = 'Unique Open Rate', 
                 'unsubscribe_rate' = 'Unsubscribe Rate', 'campaign_value' = 'Revenue', 'uniqueclickthru_rate' = 'Unique Click-Thru Rate',
                 'purchasegoal_rate' = 'Purchase Goal', 'purchasedealgoal_rate' = 'Purchase Deal Goal', 'bookclassgoal_rate' = 'Book Class Goal', 'behavioralgoal_rate' = 'Behavioral Goal')
  
  df_spread <- df %>% tidyr::spread(week, total)
  #' set name to factor and reset levels
  #' then order the levels to desired output
  df_spread$name <- as.factor(df_spread$name)
  df_spread$name <- factor(df_spread$name, levels = set_levels)
  df_spread <- df_spread[order(df_spread$name),]
  df_spread$name <- as.character(df_spread$name)
  
  #' create total column 
  #' we will also need to calculate total weighted ratios for:
  #' engagement_rate, goal_rate, optout_rate, uninstall_rate, clickthru_rate,
  #' uniqueclickthru_rate, uniqueopens_rate, unsubscribe_rate
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(!grepl('_rate', name), rowSums(.[2:ncol(.)], na.rm = TRUE), 0))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('engagement_rate', name), df_spread$TOTAL[which(df_spread$name == 'engaged')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('goal_rate', name), df_spread$TOTAL[which(df_spread$name == 'goal')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('clickthru_rate', name), df_spread$TOTAL[which(df_spread$name == 'clicks')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('uniqueclickthru_rate', name), df_spread$TOTAL[which(df_spread$name == 'unique_clicks')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('uninstall_rate', name), df_spread$TOTAL[which(df_spread$name == 'uninstalled_ghost')] / df_spread$TOTAL[which(df_spread$name == 'delivered_ghost')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('optout_rate', name), df_spread$TOTAL[which(df_spread$name == 'opt_outs')] / df_spread$TOTAL[which(df_spread$name == 'delivered_ghost')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('uniqueopens_rate', name), df_spread$TOTAL[which(df_spread$name == 'email_unique_opens')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('unsubscribe_rate', name), df_spread$TOTAL[which(df_spread$name == 'unsubs')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('purchasegoal_rate', name), df_spread$TOTAL[which(df_spread$name == 'Purchase')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('purchasedealgoal_rate', name), df_spread$TOTAL[which(df_spread$name == 'PurchaseDeal')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('behavioralgoal_rate', name), df_spread$TOTAL[which(df_spread$name == 'Behavioral Goal')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  df_spread <- df_spread %>% mutate(TOTAL = ifelse(grepl('bookclassgoal_rate', name), df_spread$TOTAL[which(df_spread$name == 'BookClass')] / df_spread$TOTAL[which(df_spread$name == 'delivered')], TOTAL))
  
  #' remove un-needed rows based on names column
  #' filtered to metric_list_original
  df_spread <- df_spread %>% filter(name %in% metrics)
  
  #' apply numeric formatting based on name
  df_spread[df_spread$name %in% c('delivered', 'engaged', 'goal'),][2:ncol(df_spread)] <- sapply(df_spread[df_spread$name %in% c('delivered', 'engaged', 'goal'),][2:ncol(df_spread)], 
                                                                                                 function(x){formatC(x, big.mark = ',', format = 'd')})
  if(any(df_spread$name == 'campaign_value')){
    df_spread[df_spread$name == 'campaign_value',][2:ncol(df_spread)] <- sapply(df_spread[df_spread$name == 'campaign_value',][2:ncol(df_spread)], 
                                                                              function(x){paste0('$', formatC(as.integer(x), big.mark = ',', format = 'd'))})
  }
  df_spread[df_spread$name %in% c('engagement_rate', 'goal_rate', 
                                  'clickthru_rate', 'uniqueclickthru_rate',
                                  'uninstall_rate', 'optout_rate',
                                  'uniqueopens_rate', 'unsubscribe_rate',
                                  'purchasegoal_rate', 'purchasedealgoal_rate', 'behavioralgoal_rate', 'bookclassgoal_rate'),][2:ncol(df_spread)] <- sapply(df_spread[df_spread$name %in% c('engagement_rate', 'goal_rate', 
                                                                                                                                         'clickthru_rate', 'uniqueclickthru_rate',
                                                                                                                                         'uninstall_rate', 'optout_rate',
                                                                                                                                         'uniqueopens_rate', 'unsubscribe_rate',
                                                                                                                                         'purchasegoal_rate', 'purchasedealgoal_rate', 'behavioralgoal_rate', 'bookclassgoal_rate'),][2:ncol(df_spread)], 
                                                                                                         function(x){scales::percent(as.numeric(x))})

  
  #' set new names of table labels
  df_spread$name <- new_names[df_spread$name]
  rownames(df_spread) <- df_spread$name
  df_spread$name <- NULL
  
  #df_spread_img <- grid.draw(tableGrob(df_spread, theme = ttheme_default()))
  df_spread_grob <- tableGrob(df_spread, theme = ttheme_default())
  
  return(df_spread_grob)
}
