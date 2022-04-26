# search recent Demography articles for indications of reproduction materials available

library(tidyverse)
library(rvest)
library(ggtext)

# volumes and issues from 2011
volumes_and_issues <- tibble(volume = 48:59, n_issues = 4)
volumes_and_issues <- volumes_and_issues %>%
  mutate(n_issues = case_when(
    volume==59~1,
    volume>49~6,
    TRUE~4
  ))


volumes_and_issues_full <- tibble(volume = rep(volumes_and_issues$volume, each = volumes_and_issues$n_issues))

# make url string
volumes_and_issues_full <- volumes_and_issues_full %>%
  group_by(volume) %>% 
  mutate(issue = 1:n()) %>% 
  mutate(url = paste("https://read.dukeupress.edu/demography/issue", volume, issue, sep = "/"))


# grab articles and search for key phrases
article_df <- tibble()

for(i in 1:nrow(volumes_and_issues_full)){
  issue_page <- volumes_and_issues_full$url[i]
  issue_page_html <- read_html(issue_page)
  
  article_pages <- issue_page_html %>% 
    html_nodes("div.al-article-items") %>% 
    html_nodes("h5") %>% 
    html_nodes("a") %>% 
    str_extract_all("\"/demography/.*\"") %>% 
    str_remove_all("\"") %>% 
    as_tibble() %>% 
    mutate(value = paste0("https://read.dukeupress.edu", value))
  
  for(j in 1:nrow(article_pages)){
    print(article_pages$value[j])
    page <- read_html(article_pages$value[j])
    
    text <- page %>% 
      html_nodes("p")  %>% 
      html_text() %>% 
      as_tibble()
    
    has_stuff_available <- text %>% 
      filter(str_detect(value, "available here")|str_detect(value, "available at")|str_detect(value, "publicly available")|str_detect(value, "downloadable")|str_detect(value, "available through")) %>% 
      mutate(mentions_data = str_detect(value, "data"),
             mentions_code = str_detect(value, "code")|str_detect(value, "calculations"),
             ipums = str_detect(value, "IPUMS"),
             github_link = str_detect(value, "git"),
             dataverse = str_detect(value, "dataverse"),
             osf_link = str_detect(value, "osf.io"))
    
    
    on_request <- text %>%
      filter(str_detect(value, "on request")) 
    
    citation_year <- text %>% 
      filter(str_detect(value, "doi")) %>% 
      mutate(year = str_extract(value, "\\d{4}")) %>% 
      rename(citation = value) %>% 
      slice(1)
    
    kwds <- page %>% 
      html_nodes("div.kwd-group") %>% 
      html_text()
    
    this_df <- tibble()
    for(k in 1:nrow(has_stuff_available)) {
      this_row <- bind_cols(citation_year, keywords = kwds, has_stuff_available[k,]) 
      this_row <- this_row %>% mutate(on_request = ifelse(nrow(on_request)>0, TRUE, FALSE))
      this_df <- bind_rows(this_df, this_row)
    }
    this_df <- this_df %>% mutate(volume = volumes_and_issues$volume[i], issue = volumes_and_issues$issue[i])
    
    article_df <- bind_rows(article_df, this_df)
    Sys.sleep(5)
  }
  
}


## oops fix year
article_df <- article_df %>% 
  mutate(year = as.numeric(str_extract(citation, "20[1,2]\\d")))

# save
write_csv(article_df, file = "output/demog_2011_plus.csv")


article_df <- read_csv("output/demog_2011_plus.csv")
# plot
article_df %>% 
  filter(year>2010) %>% 
  group_by(citation) %>% 
  slice(1) %>% 
  group_by(year) %>%
  summarize(n= n(),n_obs = sum(!is.na(value)), across(mentions_data:on_request, sum, na.rm = TRUE)) %>% 
  mutate(prop_obs = n_obs/n, prop_data = mentions_data/n, prop_req = on_request/n) %>% 
  ggplot(aes(year, prop_obs)) + 
  geom_line(aes(col = "some materials available"))+ geom_point(aes(col = "some materials available"))+
  geom_line(aes(year, prop_req, col = "on request")) + 
  geom_point(aes(year, prop_req, col = "on request")) + 
  scale_x_continuous(breaks = 2011:2022) + 
  scale_color_brewer(palette = "Set1", name = "")+
  labs(y = "proportion of articles", title = "Availability of reproduction materials in *Demography*") + 
  theme_bw()+
  theme(plot.title = ggtext::element_markdown())
ggsave(("slides/fig/demog_repro.png"))
