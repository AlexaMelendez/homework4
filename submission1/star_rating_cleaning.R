source("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/Homework4/submission1/read_variables.R")
## Assign yearly datasets and clean star rating information

## 2008
ma.path.2008a <- paste0("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/input/ma_star_ratings/2008/2008_Part_C_Report_Card_Master_Table_2009_11_30_data.csv")
star.data.2008a <- read_csv(ma.path.2008a,
                         skip=4,
                         col_names=rating.vars.2008)

ma.path.2008b <- paste0("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/input/ma_star_ratings/2008/2008_Part_C_Report_Card_Master_Table_2009_11_30_domain.csv")
star.data.2008b <- read_csv(ma.path.2008b,
                         skip=2,
                         col_names=c("contractid","contract_name","healthy","getting_care",
                                     "timely_care","chronic","appeal","new_contract"))
star.data.2008b <- as_tibble(star.data.2008b) %>%
  mutate(new_contract=replace(new_contract,is.na(new_contract),0)) %>%
  select("contractid","new_contract")

star.data.2008 <- (star.data.2008a %>% select(-new_contract)) %>%
  left_join(star.data.2008b, by=c("contractid")) %>%
  mutate(year=2008)

## 2009
ma.path.2009a <- paste0("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/input/ma_star_ratings/2009/2009_Part_C_Report_Card_Master_Table_2009_11_30_stars.csv")
star.data.2009a <- read_csv(ma.path.2009a,
                         skip=4,
                         col_names=rating.vars.2009)
star.data.2009a <- as_tibble(sapply(star.data.2009a,plyr::mapvalues,
                                 from=c("1 out of 5 stars","2 out of 5 stars","3 out of 5 stars",
                                        "4 out of 5 stars","5 stars"), 
                                 to=c("1","2","3","4","5")))
star.data.2009a <- star.data.2009a %>%
  mutate_at(vars(-one_of("contractid","org_type","contract_name","org_marketing")),
            as.numeric)

ma.path.2009b <- paste0("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/input/ma_star_ratings/2009/2009_Part_C_Report_Card_Master_Table_2009_11_30_summary.csv")
star.data.2009b <- read_csv(ma.path.2009b,
                         skip=2,
                         col_names=c("contractid","org_type","contract_name","org_marketing","partc_score"))
star.data.2009b <- star.data.2009b %>%
  mutate(new_contract=ifelse(partc_score=="Plan too new to be measured",1,0)) %>%
  mutate(partc_score=plyr::mapvalues(partc_score,
                               from=c("1 out of 5 stars","1.5 out of 5 stars",
                                      "2 out of 5 stars","2.5 out of 5 stars",
                                      "3 out of 5 stars","3.5 out of 5 stars",
                                      "4 out of 5 stars","4.5 out of 5 stars",
                                      "5 stars"), 
                               to=c("1","1.5","2","2.5","3","3.5","4","4.5","5"))) %>%
  mutate(partc_score=as.numeric(partc_score)) %>%
  select(contractid, new_contract, partc_score)

star.data.2009 <- star.data.2009a %>%
  left_join(star.data.2009b, by=c("contractid")) %>%
  mutate(year=2009)

## 2010
ma.path.2010a <- paste0("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/input/ma_star_ratings/2010/2010_Part_C_Report_Card_Master_Table_2009_11_30_domain.csv")
star.data.2010a <- read_csv(ma.path.2010a,
                         skip=4,
                         col_names=rating.vars.2010)
star.data.2010a <- as_tibble(sapply(star.data.2010a,plyr::mapvalues,
                                 from=c("1 out of 5 stars","2 out of 5 stars","3 out of 5 stars",
                                        "4 out of 5 stars","5 stars"), 
                                 to=c("1","2","3","4","5")))
star.data.2010a <- star.data.2010a %>%
  mutate_at(vars(-one_of("contractid","org_type","contract_name","org_marketing")),
            as.numeric)

ma.path.2010b <- paste0("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/input/ma_star_ratings/2010/2010_Part_C_Report_Card_Master_Table_2009_11_30_summary.csv")
star.data.2010b <- read_csv(ma.path.2010b,
                         skip=2,
                         col_names=c("contractid","org_type","contract_name","org_marketing","partc_score"))
star.data.2010b <- star.data.2010b %>%
  mutate(new_contract=ifelse(partc_score=="Plan too new to be measured",1,0)) %>%
  mutate(partc_score=plyr::mapvalues(partc_score,
                               from=c("1 out of 5 stars","1.5 out of 5 stars",
                                      "2 out of 5 stars","2.5 out of 5 stars",
                                      "3 out of 5 stars","3.5 out of 5 stars",
                                      "4 out of 5 stars","4.5 out of 5 stars",
                                      "5 stars"), 
                               to=c("1","1.5","2","2.5","3","3.5","4","4.5","5"))) %>%
  mutate(partc_score=as.numeric(partc_score)) %>%
  select(contractid, new_contract, partc_score)

star.data.2010 <- star.data.2010a %>%
  left_join(star.data.2010b, by=c("contractid")) %>%
  mutate(year=2010)

## [Continue similar edits for 2011–2015…]

# Final lines remain unchanged
star.ratings <- plyr::rbind.fill(star.data.2008, star.data.2009, star.data.2010, star.data.2011,
                                 star.data.2012, star.data.2013, star.data.2014, star.data.2015)
star.ratings <- as_tibble(star.ratings)
star.ratings <- star.ratings %>% 
  mutate(new_contract)

write_rds(star.ratings,"data/output/star_ratings.rds")
