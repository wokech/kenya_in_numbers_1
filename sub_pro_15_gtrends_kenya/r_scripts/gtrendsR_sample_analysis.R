# Google Trends + R: Leverage gtrendsR Package for More 
# Powerful Search Trend Analytics

# https://catbirdanalytics.wordpress.com/2021/08/29/google-trends-r-leverage-gtrendsr-package-for-more-powerful-analytics/

# Install and load packages

library(gtrendsR) ## package for accessing Google Trends 
library(tidyverse)
library(gridExtra)
library(patchwork)

# Single term query

gt_results <- gtrends(keyword='cryptocurrency',
                      geo="",
                      time="now 7-d",
                      gprop=c("web"),
                      category=0)

# The query returns a bundle of 7 data frames with different info, 
# reflecting what is shown in the Google Trends interface

names(gt_results)

# Interest over time

# The ‘interest_over_time’ data frame is the main data object, with 
# relative search volume for the selected search term, country, period, 
# property, and category

chart_title <- "Searches for: cryptocurrency"
sub_title <- "Period: past 7 days; Geo: world; Prop: 'web'; Category: all"

## create chart based on search interest over time
gt_results$interest_over_time %>% ggplot(aes(x=date, y=hits, color=keyword))+geom_line()+
  labs(title=chart_title, subtitle=sub_title, x="", y="")

# Related Topics

# The ‘related_topics’ data frame holds data on queries related to 
# the main search term (‘cryptocurrency’ in this case).

str(gt_results$related_topics) # Not working

# Related Queries

# Related Queries module has similar structure to Related Topics:

str(gt_results$related_queries)

chart_title <- "crytopcurrency: related queries"
## 
top <- gt_results$related_queries %>% filter(related_topics=='top' & !is.na(subject) &
                                               subject!='<1')
## convert value to factor and subject to numeric
top$value <- as.factor(top$value)
top$subject <- as.numeric(top$subject)
## PLOT related topics
top %>% ggplot(aes(x=reorder(value, subject), y=subject))+geom_col()+
  coord_flip()+
  scale_y_continuous(expand=expansion(add=c(0,10)))+
  labs(title=chart_title, y='', x='')

# Multi-Term Query

# The same approach used to query for single terms can be extended 
# to multiple terms. The example below shows how to load up a 
# collection of terms, as well as leveraging other variables 
# for the query.

## create list of multiple search terms
srch_term <- c("cryptocurrency",
               "bitcoin",
               "ethereum",
               "stock market",
               "real estate")
period <- "today 12-m"
ctry <- "" ## blank = world; based on world countries ISO code
prop <- c("web")
cat <- 0 ## 0 = all categories

## user-friendly versions of parameters for use in chart titles or other query descriptions
ctry_ <- ifelse(ctry=="","world",ctry)
prop_ <- paste0(prop, collapse=", ")
cat_ <- ifelse(cat==0,"all",cat)

## use gtrendsR to call google trends API
gt_results <- gtrends(keyword=srch_term,
                      geo=ctry,
                      time=period,
                      gprop=prop,
                      category=cat)


# Interest over time

chart_title <- paste0("Search trends: ", paste(srch_term[1:2], collapse=", "), " +")
sub_title <- paste0("Period: ", period, "; Geo: ", ctry_, "; Prop: ", prop_, "; Category: ", cat_)

## create chart based on search interest over time
gt_results$interest_over_time %>% ggplot(aes(x=date, y=hits, color=keyword))+geom_line()+
  scale_y_continuous(expand=expansion(add=c(0,0)))+
  labs(title=chart_title, subtitle=sub_title, x="", y="")

# Search Terms vs Search ‘Topics’

# Don't mix terms with topics
# Replace the ‘%2Fm%2F’ with ‘/m/’ and use the rest as is


# Example with topics

## create list of multiple search terms using topic codes, separated by commas in URL and decoded
srch_term <- c("/m/0vpj4_b",
               "/m/05p0rrx")

srch_topic <- c("Cryptocurrency_topic",
                "Bitcoin_currency")

period <- "today 12-m"
ctry <- "" ## blank = world; based on world countries ISO code
prop <- c("web")
cat <- 0 ## 0 = all categories

## user-friendly versions of parameters for use in chart titles or other query descriptions
ctry_ <- ifelse(ctry=="","world",ctry)
prop_ <- paste0(prop, collapse=", ")
cat_ <- ifelse(cat==0,"all",cat)

## use gtrendsR to call google trends API
gt_results <- gtrends(keyword=srch_term,
                      geo=ctry,
                      time=period,
                      gprop=prop,
                      category=cat)

## replace codes with topics
## - extract interest_over_time data frame
gt_interest <- gt_results$interest_over_time
## - replace codes with corresponding terms
gt_interest <- gt_interest %>% mutate(
  keyword=ifelse(keyword==srch_term[1],srch_topic[1],
                 ifelse(keyword==srch_term[2], srch_topic[2],""))
)



## create chart based on search interest over time
pint1 <- gt_interest %>% ggplot(aes(x=date, y=hits, color=keyword))+geom_line(linewidth=2)+
  scale_y_continuous(expand=expansion(add=c(0,0)))+
  scale_color_manual(values=c("red","blue"))+
  theme(legend.position = 'top')+
  labs(x="", y="")

pint2 <- gt_interest %>% group_by(keyword) %>% summarize(avg_int=mean(hits)) %>%
  ggplot(aes(x=keyword, y=avg_int, fill=keyword))+geom_col()+
  scale_y_continuous(limit=c(0,100))+
  scale_fill_manual(values=c("red","blue"))+
  theme(legend.position = 'none',
        axis.text.x = element_blank())+
  labs(x="Average", y="")

grid.arrange(pint2, pint1, nrow=1, widths=c(2,8))

#OR

pint1 + pint2
