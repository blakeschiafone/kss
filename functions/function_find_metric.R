find_metric <- function(metric_list){
  
    if(any(metric_list == 'engagement_rate')){
      p1 <- setNames(list(engagement_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('engaged'),
                              b = as.name('delivered')))), 'engagement_rate')
    }
  
    if(any(metric_list == 'goal_rate')){
      p2 <- setNames(list(goal_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('goal'),
                              b = as.name('delivered')))), 'goal_rate')
      
  }
  
  if(any(metric_list == 'uninstall_rate')){
      p3 <- setNames(list(uninstall_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('uninstalled_ghost'),
                              b = as.name('delivered_ghost')))), 'uninstall_rate')
    
  }
  
  if(any(metric_list == 'optout_rate')){
      p4 <- setNames(list(optout_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('opt_outs'),
                              b = as.name('delivered_ghost')))), 'optout_rate')
    
  }
  
  if(any(metric_list == 'clickthru_rate')){
      p5 <- setNames(list(clickthru_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('clicks'),
                              b = as.name('delivered')))), 'clickthru_rate')
    
  }
  
  if(any(metric_list == 'uniqueclickthru_rate')){
      p6 <- setNames(list(uniqueclickthru_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('unique_clicks'),
                              b = as.name('delivered')))), 'uniqueclickthru_rate')
    
  }
  
  if(any(metric_list == 'uniqueopens_rate')){
      p7 <- setNames(list(uniqueopens_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('email_unique_opens'),
                              b = as.name('delivered')))), 'uniqueopens_rate')
    
  }
  
  if(any(metric_list == 'unsubscribe_rate')){
      p8 <- setNames(list(unsubscribe_rate = 
                            substitute(round(a/b, digits = 5), list(
                              a = as.name('unsubs'),
                              b = as.name('delivered')))), 'unsubscribe_rate')
    
  }
  
  if(any(metric_list == 'Behavioral Goal')){
      p9 <- setNames(list(behavioralgoal_rate = 
                            substitute(round(a/b, digits = 5), list(
                            a = as.name('Behavioral Goal'),
                            b = as.name('delivered')))), 'behavioralgoal_rate')
    
  }
  
  if(any(metric_list == 'Purchase')){
      p10 <- setNames(list(purchasegoal_rate = 
                            substitute(round(a/b, digits = 5), list(
                            a = as.name('Purchase'),
                            b = as.name('delivered')))), 'purchasegoal_rate')
    
  }
  
  if(any(metric_list == 'PurchaseDeal')){
    p11 <- setNames(list(purchasedealgoal_rate = 
                           substitute(round(a/b, digits = 5), list(
                             a = as.name('PurchaseDeal'),
                             b = as.name('delivered')))), 'purchasedealgoal_rate')
    
  }
  
  if(any(metric_list == 'BookClass')){
    p12 <- setNames(list(bookclassgoal_rate = 
                           substitute(round(a/b, digits = 5), list(
                             a = as.name('BookClass'),
                             b = as.name('delivered')))), 'bookclassgoal_rate')
    
  }
  
  
  
  returned_list <- c(if(exists('p1')){p1}, if(exists('p2')){p2}, if(exists('p3')){p3}, 
                     if(exists('p4')){p4}, if(exists('p5')){p5}, if(exists('p6')){p6}, 
                     if(exists('p7')){p7}, if(exists('p8')){p8}, if(exists('p9')){p9},
                     if(exists('p10')){p10}, if(exists('p11')){p11}, if(exists('p12')){p12})

  return(returned_list)
}
