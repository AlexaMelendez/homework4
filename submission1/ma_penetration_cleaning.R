library(tidyverse)

# Define month lists by year
monthlist_2008 = c("06","07","08","09","10","11","12")
monthlist_2009 = c("01","02","03","04","05","06","07","08","09","10","11","12")
monthlist_2010 = monthlist_2009
monthlist_2011 = monthlist_2009
monthlist_2012 = monthlist_2009
monthlist_2013 = monthlist_2009
monthlist_2014 = monthlist_2009
monthlist_2015 = monthlist_2009

# Initialize vector to store missing files
missing_files <- c()

# Loop through each year
for (y in 2008:2015) {
  monthlist = get(paste0("monthlist_", y))
  step = 0
  
  for (m in monthlist) {
    step = step + 1
    
    # Build file path
    ma.path = paste0("C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/input/penetration", y, "_", m, ".csv")
    
    # Skip if file doesn't exist
    if (!file.exists(ma.path)) {
      warning(paste("Missing file:", ma.path))
      missing_files <- c(missing_files, ma.path)
      next
    }
    
    # Read in the data
    pene.data = read_csv(ma.path, skip = 1,
                         col_names = c("state", "county", "fips_state", "fips_cnty", "fips",
                                       "ssa_state", "ssa_cnty", "ssa", "eligibles", "enrolled",
                                       "penetration"),
                         col_types = cols(
                           state = col_character(),
                           county = col_character(),
                           fips_state = col_integer(),
                           fips_cnty = col_integer(),
                           fips = col_double(),
                           ssa_state = col_integer(),
                           ssa_cnty = col_integer(),
                           ssa = col_double(),
                           eligibles = col_number(),
                           enrolled = col_number(),
                           penetration = col_number()
                         ), na = "*")
    
    # Add year and month
    pene.data = pene.data %>% mutate(month = m, year = y)
    
    # Append monthly data
    if (step == 1) {
      ma.penetration = pene.data
    } else {
      ma.penetration = rbind(ma.penetration, pene.data)
    }
  }
  
  # Skip to next year if nothing was read
  if (!exists("ma.penetration")) {
    warning(paste("No data loaded for year", y))
    next
  }
  
  # Fill in missing FIPS
  ma.penetration = ma.penetration %>%
    group_by(state, county) %>%
    fill(fips)
  
  # Collapse to yearly data
  ma.penetration = ma.penetration %>%
    group_by(fips, state, county) %>%
    summarize(avg_eligibles = mean(eligibles), sd_eligibles = sd(eligibles),
              min_eligibles = min(eligibles), max_eligibles = max(eligibles),
              first_eligibles = first(eligibles), last_eligibles = last(eligibles),
              avg_enrolled = mean(enrolled), sd_enrolled = sd(enrolled),
              min_enrolled = min(enrolled), max_enrolled = max(enrolled),
              first_enrolled = first(enrolled), last_enrolled = last(enrolled),
              year = last(year), ssa = first(ssa),
              .groups = "drop")
  
  assign(paste0("ma.pene.", y), ma.penetration)
  rm(ma.penetration)
}

# Combine all years
existing_data <- mget(ls(pattern = "^ma\\.pene\\."), ifnotfound = list())
ma.penetration.data <- do.call(rbind, existing_data)

# Save the combined data
write_rds(ma.penetration.data, 
          "C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/data/output/ma_penetration.rds")

# Save list of missing files
writeLines(missing_files, 
           "C:/Users/melen/OneDrive/Documents/Econ_470/Homework 4/missing_files_log.txt")
