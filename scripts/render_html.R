#' ---
#' title: 'Kahuna Strategic Services'
#' author: '`r NULL`'
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

```{r global_options, echo = FALSE, results = "hide", message = FALSE}
knitr::opts_chunk$set(fig.width=14, fig.height=14, echo=FALSE, warning=FALSE, message=FALSE)
```
<br>
  <br>
  <br>
  
### `r paste0('Date range used: ', date_begin, ' to ', date_end)`  
<br>
<br>
<br>

```{r}
#' ### Campaign Summary 
#' This table shows total message volume, goal volume, revenue and engagement rates.  The data is shown for each
#' channel (push, email, in-app) and campaign type (lifecycle, one time, conversion, adaptive, experience).

#if(is.gtable(campaign_summary_graph)){grid.draw(campaign_summary_graph)}
knitr::include_graphics('/home/rstudio/scripts/kss/output/campaign_summary_graph.png', dpi = NA)
```
<br>
  <br>
  <br> 
  
  
```{r}
#' ### Largest Volume Sends
#' This graph shows the top 5 campaigns that sent the most messages.  *Please note:  if a campaign does not
#' have goals attached, then it will not show  up in this list*.  
if(is.gtable(largest_volume_sends_graph)){grid.draw(largest_volume_sends_graph)}
```
<br>
  <br>
  <br>  
  
  
```{r}
#' ### Campaign Performance: Revenue
if(is.gtable(campaign_performance_revenue_gtable)){grid.draw(campaign_performance_revenue_gtable)}
```
<br>
  <br>
  <br>
  
  
```{r}
#' ### Campaign Performance: Purchase/Behavioral Conversion Rates
if(is.gtable(all_campaign_performance_goal_rate_gtable)){grid.draw(all_campaign_performance_goal_rate_gtable)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Engagment Rate
if(is.gtable(campaign_performance_engagement_gtable)){grid.draw(campaign_performance_engagement_gtable)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Total Goal Conversion Rate
if(is.gtable(campaign_performance_goal_gtable)){grid.draw(campaign_performance_goal_gtable)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Engagement & Revenue
if(is.gtable(campaign_performance_revenue_engagement_gtable)){grid.draw(campaign_performance_revenue_engagement_gtable)}
```
<br>
  <br>
  <br>
  

```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Push - Goals, clicks, engagement
if(is.gtable(push_campaign_performance_goals_ctr_engagement_gtable)){grid.draw(push_campaign_performance_goals_ctr_engagement_gtable)}
```
<br>
  <br>
  <br>
  

```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Push - Attrition metrics
if(is.gtable(push_campaign_performance_attrition_gtable)){grid.draw(push_campaign_performance_attrition_gtable)}
```
<br>
  <br>
  <br>
  

```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Push - WoW metrics
if(!is.error(ggplot_build(push_wow_metrics_graph))){plot(push_wow_metrics_graph)}
```
<br>
  <br>
  <br>
  

```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Email - Goals, clicks, engagement, unique open rate
if(is.gtable(email_campaign_performance_goals_ctr_engagement_graph)){grid.draw(email_campaign_performance_goals_ctr_engagement_graph)}
```
<br>
  <br>
  <br>


```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Email - Attrition metrics
if(is.gtable(email_campaign_performance_attrition_graph)){grid.draw(email_campaign_performance_attrition_graph)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Email - WoW metrics
if(!is.error(ggplot_build(email_wow_metrics_graph))){plot(email_wow_metrics_graph)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: In App - Goals
if(is.gtable(inapp_campaign_performance_goals_ctr_engagement_graph)){grid.draw(inapp_campaign_performance_goals_ctr_engagement_graph)}
```
<br>
  <br>
  <br>
  

```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Goal SOM by Channel
if(!is.error(ggplot_build(goal_som_by_channel_graph))){plot(goal_som_by_channel_graph)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Experiences New User On-Boarding
if(!is.error(ggplot_build(experiences_new_user_graph))){plot(experiences_new_user_graph)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Experiences In-Active User
if(!is.error(ggplot_build(experiences_inactive_graph))){plot(experiences_inactive_graph)}
```
<br>
  <br>
  <br>

  
```{r fig.width = 30, fig.height = 8}
#' ### Top Performing Campaigns (channel agnostic)
#pushViewport(viewport(width=1.8,height=0.90,x=0.5,y=0.80,clip="on"))
#if(is.gtable(top_campaigns_gtable)){grid.draw(top_campaigns_gtable)}
knitr::include_graphics('/home/rstudio/scripts/kss/output/top_campaigns.png', dpi = NA)
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 30, fig.height = 8}
#' ### Bottom Performing Campaigns (channel agnostic)
#pushViewport(viewport(width=1.8,height=0.90,x=0.5,y=0.80,clip="on"))
# if(is.gtable(bottom_campaigns_gtable)){grid.draw(bottom_campaigns_gtable)}
knitr::include_graphics('/home/rstudio/scripts/kss/output/bottom_campaigns.png', dpi = NA)
```
<br>
  <br>
  <br>
  