# Analysis for Russell

# Publications

library(tidyverse)
library(RefManageR)

source("function.R")

rjh <- prepare_bib_report("~/git/CV/rjhpubs.bib")
rjh <- rjh %>%
  filter(bibtype=="Article") %>%
  mutate(
    Rank = as.character(Rank),
    Group = recode(Rank,
              `A*` = "Group 1",
              `A`  = "Group 2"
    ),
    Group = if_else(journal == "Journal of the American Statistical Association",
                    "Group 1+",
                    Group)
  ) %>%
  select(title,author,year,journal,Rank,Group) %>%
  arrange(desc(year), Group) 

rjh %>% 
  group_by(Group) %>%
  filter(year >= 2012, year <= 2018) %>%
  summarise(count = n())


rjh %>% filter(Group == "Group 1+", year < 2019)
rjh %>% filter(Group == "Group 1", year < 2019) 
rjh %>% filter(Group == "Group 2", year < 2019)
rjh %>% filter(!grepl("Group", Group)) 

# MyPlan Achievements 2019

rjh %>% 
  filter(year==2019) %>% 
  arrange(Group) %>%
  select(-author, -title)

rjh %>% 
  filter(year == 2019) %>%
  group_by(Group) %>%
  summarise(count = n())

# Grants

read_csv("Grant_income.csv") %>%
  filter(Start >= 2012) %>%
  summarise(Amount = sum(Amount))
  
