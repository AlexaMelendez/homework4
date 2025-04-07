
# Optional: set base path to avoid repeating long strings
base_path <- "C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/Homework4"

# Call individual scripts -------------------------------------------------
source(paste0(base_path, "/submission1/1_Plan_Data.R"))
source(paste0(base_path, "/submission1/2_Plan_Characteristics.R"))
source(paste0(base_path, "/submission1/3_Service_Areas.R"))
source(paste0(base_path, "/submission1/4_Penetration_Files.R"))
source(paste0(base_path, "/submission1/5_Star_Ratings.R"))
source(paste0(base_path, "/submission1/6_Risk_Rebates.R"))
source(paste0(base_path, "/submission1/7_MA_Benchmark.R"))
source(paste0(base_path, "/submission1/8_FFS_Costs.R"))

# Tidy data ---------------------------------------------------------------
full.ma.data <- read_rds(paste0(base_path, "/data/output/full_ma_data.rds"))
contract.service.area <- read_rds(paste0(base_path, "/data/output/contract_service_area.rds"))
star.ratings <- read_rds(paste0(base_path, "/data/output/star_ratings.rds"))
ma.penetration.data <- read_rds(paste0(base_path, "/data/output/ma_penetration.rds"))
plan.premiums <- read_rds(paste0(base_path, "/data/output/plan_premiums.rds"))
risk.rebate.final <- read_rds(paste0(base_path, "/data/output/risk_rebate.rds"))
benchmark.final <- read_rds(paste0(base_path, "/data/output/ma_benchmark.rds")) %>%
  mutate(ssa = as.double(ssa))
ffs.costs.final <- read_rds(paste0(base_path, "/data/output/ffs_costs.rds"))

final.data <- full.ma.data %>%
  inner_join(contract.service.area %>% select(contractid, fips, year),
             by = c("contractid", "fips", "year")) %>%
  filter(!state %in% c("VI", "PR", "MP", "GU", "AS", "") &
           snp == "No" &
           (planid < 800 | planid >= 900) &
           !is.na(planid) & !is.na(fips))

final.data <- final.data %>%
  left_join(star.ratings %>% select(-contract_name, -org_type, -org_marketing),
            by = c("contractid", "year")) %>%
  left_join(ma.penetration.data %>% ungroup() %>% select(-ssa) %>%
              rename(state_long = state, county_long = county),
            by = c("fips", "year"))

final.data <- final.data %>% ungroup() %>%
  mutate(Star_Rating =
           case_when(
             partd == "No" ~ partc_score,
             partd == "Yes" & is.na(partcd_score) ~ partc_score,
             partd == "Yes" & !is.na(partcd_score) ~ partcd_score,
             TRUE ~ NA_real_
           ))

final.state <- final.data %>%
  group_by(state) %>%
  summarize(state_name = last(state_long, na.rm = TRUE))

final.data <- final.data %>%
  left_join(final.state, by = c("state"))

final.data <- final.data %>%
  left_join(plan.premiums,
            by = c("contractid", "planid", "state_name" = "state", "county", "year")) %>%
  left_join(risk.rebate.final %>% select(-contract_name, -plan_type),
            by = c("contractid", "planid", "year")) %>%
  left_join(benchmark.final,
            by = c("ssa", "year"))

final.data <- final.data %>% ungroup() %>%
  mutate(ma_rate =
           case_when(
             year < 2012 ~ risk_ab,
             year >= 2012 & year < 2015 & Star_Rating == 5 ~ risk_star5,
             year >= 2012 & year < 2015 & Star_Rating == 4.5 ~ risk_star45,
             year >= 2012 & year < 2015 & Star_Rating == 4 ~ risk_star4,
             year >= 2012 & year < 2015 & Star_Rating == 3.5 ~ risk_star35,
             year >= 2012 & year < 2015 & Star_Rating == 3 ~ risk_star3,
             year >= 2012 & year < 2015 & Star_Rating < 3 ~ risk_star25,
             year >= 2012 & year < 2015 & is.na(Star_Rating) ~ risk_star35,
             year >= 2015 & Star_Rating >= 4 ~ risk_bonus5,
             year >= 2015 & Star_Rating < 4 ~ risk_bonus0,
             year >= 2015 & is.na(Star_Rating) ~ risk_bonus35,
             TRUE ~ NA_real_
           ))

final.data <- final.data %>%
  mutate(basic_premium =
           case_when(
             rebate_partc > 0 ~ 0,
             partd == "No" & !is.na(premium) & is.na(premium_partc) ~ premium,
             TRUE ~ premium_partc
           ),
         bid =
           case_when(
             rebate_partc == 0 & basic_premium > 0 ~ (payment_partc + basic_premium) / riskscore_partc,
             rebate_partc > 0 | basic_premium == 0 ~ payment_partc / riskscore_partc,
             TRUE ~ NA_real_
           ))

final.data <- final.data %>%
  left_join(ffs.costs.final %>% select(-state),
            by = c("ssa", "year")) %>%
  mutate(avg_ffscost = case_when(
    parta_enroll == 0 & partb_enroll == 0 ~ 0,
    parta_enroll == 0 & partb_enroll > 0 ~ partb_reimb / partb_enroll,
    parta_enroll > 0 & partb_enroll == 0 ~ parta_reimb / parta_enroll,
    parta_enroll > 0 & partb_enroll > 0 ~ (parta_reimb / parta_enroll) + (partb_reimb / partb_enroll),
    TRUE ~ NA_real_
  ))

# Save final dataset
write_rds(final.data, "C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/Homework4/data/output/final_ma_data.rds")

