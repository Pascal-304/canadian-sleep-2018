#### Preamble ####
# Purpose: The purpose of this code is to clean up     Clean the survey data downloaded from [...UPDATE ME!!!!!]That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed.
# Author: Pascal Lee Slew
# Date: 29 April 2022
# Contact: pascal.leeslew@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the CCHS data and saved it to inputs/data
# - Gitignore it



#### Workspace setup ####

library(haven)
library(tidyverse)
library(janitor)
library(readr)
# Read in the raw data. 
#raw_data <- readr::read_csv("inputs/data/cchs.csv")

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("inputs/data/cchs.csv")
dict <- read_lines("inputs/data/cchs_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("inputs/data/chhs_labels.txt")
                     

### Setup the dictionary ###
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))


# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()


# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables

cchs_data <- raw_data %>% 
  select(CASEID,
         slp_010,
         slp_015,
         slp_020,
         dhhgage,
         dhhgms,
         dhh_sex,
         slpg005,
         ccc_035,
         ccc_195,
         ccc_200,
         depdvsev,
         incdgper,
         gen_005,
         gen_015,
         gen_020,
         gen_025,
         gen_010) %>% 
  mutate_at(vars(slp_010:gen_010), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(slp_010:gen_025),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
cchs_data1 <- cchs_data %>% 
  clean_names() %>% 
  rename(caseid = caseid,
         diff_sleep_or_stay_asleep = slp_010,
         refreshing_sleep = slp_015,
         diff_stay_awake = slp_020,
         age = dhhgage,
         marital_status = dhhgms,
         sex = dhh_sex,
         num_hrs_sleep = slpg005,
         sleep_apnea = ccc_035,
         mood_disorder = ccc_195,
         anxiety_disorder = ccc_200,
         depression_scale = depdvsev,
         perceived_health = gen_005,
         life_satisfaction = gen_010,
         per_mental_health = gen_015,
         per_life_stress = gen_020,
         per_stress_work = gen_025,
         income = incdgper)


# save file
write_csv(cchs_data1, "outputs/paper/cchs_data1.csv")





         