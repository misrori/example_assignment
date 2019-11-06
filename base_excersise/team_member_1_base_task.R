# this is an example of the individual assignment
# Both team member should download a news site search result. 
# write function, use lapply, and rbindlist

# In the example i will download news from https://fishingnews.co.uk
# the search result base is https://fishingnews.co.uk/?s=tuna
# the second page lokks like this: https://fishingnews.co.uk/page/2/?s=tuna
# i will dowload just the first 3 page result
library(rvest)
library(data.table)

my_search_term <- 'tuna'
page_to_download <- 5
my_base_url <-paste0("https://fishingnews.co.uk/?s=", my_search_term)


# make dataframe of one page
t <- read_html(my_base_url)

my_titles <- 
t %>%
  html_nodes('.results-list a')%>%
  html_text()

my_links <- 
t %>%
  html_nodes('.results-list a')%>%
  html_attr('href')

my_summary <- 
t %>%
  html_nodes('#content p')%>%
  html_text()

data.frame('titles' = my_titles, 'links'= my_links, 'summary' = my_summary)

# OK! now put it into a function

get_one_page <- function(my_base_url){
  
  
  # make dataframe of one page
  t <- read_html(my_base_url)
  
  my_titles <- 
    t %>%
    html_nodes('.results-list a')%>%
    html_text()
  
  my_links <- 
    t %>%
    html_nodes('.results-list a')%>%
    html_attr('href')
  
  my_summary <- 
    t %>%
    html_nodes('#content p')%>%
    html_text()
  
  return(data.frame('titles' = my_titles, 'links'= my_links, 'summary' = my_summary))
  
}



# make the url 
first_link <- paste0("https://fishingnews.co.uk/?s=", my_search_term)

# the rest
rest_link <- paste0('https://fishingnews.co.uk/page/',c(2:page_to_download),'/?s=',my_search_term)

my_links<- c(first_link, rest_link )

df_lists  <- lapply(my_links, get_one_page)

final_df <- rbindlist(df_lists)



# it is nicer if your df is comming for every search result box 

get_one_page_from_boxes <- function(my_base_url) {
  
  # make dataframe of one page
  t <- read_html(my_base_url)
  res_boxes <- t %>%
    html_nodes('#content li')
  
  res_boxes <- res_boxes[1:10]
  
list_of_boxes_df <- 
  lapply(res_boxes, function(x){
    my_titles <- 
      x %>%
      html_nodes('a')%>%
      html_text()
    
    my_links <- 
      x %>%
      html_nodes('a')%>%
      html_attr('href')
    
    my_summary <- 
      x %>%
      html_nodes('p')%>%
      html_text()
    
    return(data.frame('titles' = my_titles, 'links'= my_links, 'summary' = my_summary))
  })

  
df_of_the_page <- rbindlist(list_of_boxes_df) 

return(df_of_the_page )  
}

# we can use the same links

df_lists  <- lapply(my_links, get_one_page_from_boxes)

final_df <- rbindlist(df_lists)



