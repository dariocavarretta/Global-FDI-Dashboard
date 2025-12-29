library(dplyr)
library(tidyr)
library(zoo)
library(readr)
library(countrycode)

#  loading cleaned data

rm(list = ls())
  

setwd("C:/Users/Dario/Desktop/global dashboard")


fdi_raw <- read_csv("data/fake_fdi.csv")


### CLEAN BASE DATASET

fdi <- fdi_raw %>%
  rename(
    src_country = `Source country`,
    dest_country = `Destination country`,
    value       = `Capital Investment ($USm)`,
    n_projects  = `No.of Projects`,
    jobs        = `Job Creation`,
    status      = `Project status`
  ) %>%
  filter(!is.na(status)) %>%
  mutate(
    time  = as.yearmon(time),
    year  = as.numeric(format(time, "%Y")),
    month = as.numeric(format(time, "%m")),
    ym    = as.yearmon(time),
    
    status_group = case_when(
      status == "Announced" ~ "Announced",
      status %in% c("Opened", "Closed") ~ "Opened",  
      TRUE ~ "Other"                                 
    ),
    status = case_when(
      status == "Announced" ~ "Announced",
      status %in% c("Opened", "Closed") ~ "Opened",  
      TRUE ~ "Other"
    )
  )


### I know that having two status variables is not "clean coding" however at first I had used different definitions fopr the two but bthen decided to merge them back
### for compatibility. As some servers use status_group and some use status, i do not have time to change all of them. :( --> there is no issue with the output 
### however, as it is all consistent

### Closed projects are counted as Opened because they are ina  sense "actual FDI" ie they have been opened before being closed, and this is the clear distinction
### with announced projects --> as such "Opened" captures both projects that are still ongoing and projects that have been opened and closed (so "actual" projects)





saveRDS(fdi, "data/fdi_basic.rds")

#print(fdi[fdi$status == "Closed", ])


######## GENERAL ANALYSIS - TOTAL TRENDS   ###################


### MONTHLY AGGREGATION

# full monthly grid
ym_seq <- seq(from = min(fdi$ym), to = max(fdi$ym), by = 1/12)

full_months <- expand.grid(
  ym = ym_seq,
  status_group = c("Announced", "Opened")
) %>%
  mutate(
    period = format(ym, "%Y-%m"),
    year   = as.numeric(format(ym, "%Y")),
    month  = as.numeric(format(ym, "%m")),
    sort_index = year * 12 + month
  )

fdi_monthly <- fdi %>%
  group_by(ym, status_group) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    jobs  = sum(jobs,  na.rm = TRUE),
    n_projects = sum(n_projects, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(full_months, by = c("ym", "status_group")) %>%
  replace_na(list(value = 0, jobs = 0, n_projects = 0)) %>%
  arrange(sort_index)

saveRDS(fdi_monthly, "data/fdi_monthly.rds")


##############################################
### QUARTERLY AGGREGATION

# full quarter grid

years_vec <- sort(unique(fdi$year))

full_quarters <- expand.grid(
  year = years_vec,
  q    = 1:4,
  status_group = c("Announced","Opened")
) %>%
  mutate(
    period = paste0("Q", q, " ", year),
    sort_index = year * 4 + q
  )

fdi_quarterly <- fdi %>%
  mutate(q = ceiling(month / 3)) %>%
  group_by(year, q, status_group) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    jobs  = sum(jobs,  na.rm = TRUE),
    n_projects = sum(n_projects, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(full_quarters, by = c("year","q","status_group")) %>%
  replace_na(list(value = 0, jobs = 0, n_projects = 0)) %>%
  arrange(sort_index)

saveRDS(fdi_quarterly, "data/fdi_quarterly.rds")


##############################################
### YEARLY AGGREGATION 

fdi_yearly <- fdi %>%
  group_by(year, status_group) %>%
  summarise(
    value      = sum(value, na.rm=TRUE),
    jobs       = sum(jobs, na.rm=TRUE),
    n_projects = sum(n_projects, na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    period = as.character(year),
    sort_index = year
  ) %>%
  arrange(year)

saveRDS(fdi_yearly, "data/fdi_yearly.rds")

#### project size 

# --- CREATE PROJECT SIZE BUCKET DISTRIBUTION FOR ALL 3 STATUS GROUPS ---

# Create buckets and filter to Announced & Actual only
ps_raw <- fdi %>%
  mutate(
    bucket = case_when(
      value < 20 ~ "Small (<20M)",
      value >= 20 & value < 100 ~ "Medium (20–100M)",
      value >= 100 & value < 500 ~ "Large (100–500M)",
      value >= 500 ~ "Mega (>500M)",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(status_group %in% c("Announced", "Opened")) %>%
  group_by(year, status_group, bucket) %>%
  summarise(n = sum(n_projects, na.rm = TRUE), .groups = "drop")

ps_total <- ps_raw %>%
  group_by(year, bucket) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(status_group = "Total")

ps_all <- bind_rows(ps_raw, ps_total)

project_size_dist_yearly <- ps_all %>%
  group_by(year, status_group) %>%
  mutate(
    total = sum(n),
    share = (n / total) * 100
  ) %>%
  ungroup() %>%
  mutate(
    period = as.character(year),
    sort_index = year
  ) %>%
  arrange(year, factor(status_group, levels = c("Announced", "Opened", "Total")), bucket)

# Save
saveRDS(project_size_dist_yearly, "data/project_size_dist_yearly.rds")


####efficieny jobs / cap

efficiency_yearly <- fdi_yearly %>%
  select(year, status_group, value, jobs) %>%
  group_by(year, status_group) %>%
  summarise(
    value_total = sum(value, na.rm = TRUE),
    jobs_total  = sum(jobs, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    efficiency = (jobs_total / value_total) * 1000  # jobs per $1B
  ) %>%
  ungroup() %>%
  mutate(
    period = as.character(year),
    sort_index = year
  )

# Add TOTAL row
efficiency_total <- efficiency_yearly %>%
  group_by(year) %>%
  summarise(
    value_total = sum(value_total),
    jobs_total  = sum(jobs_total),
    efficiency  = (jobs_total / value_total) * 1000,
    status_group = "Total",
    .groups = "drop"
  )

efficiency_yearly <- bind_rows(efficiency_yearly, efficiency_total)

saveRDS(efficiency_yearly, "data/efficiency_yearly.rds")




##### volatility dataset


volatility_yearly <- fdi_yearly %>%
  select(year, status_group, value) %>%
  
  # Pivot to wide so we get Announced / Actual columns
  tidyr::pivot_wider(
    names_from = status_group,
    values_from = value,
    values_fill = 0
  ) %>%
  
  # Create Total
  mutate(
    Total = Announced + Opened
  ) %>%
  
  # Pivot back to long so all 3 categories exist
  tidyr::pivot_longer(
    cols = c("Announced", "Opened", "Total"),
    names_to = "status_group",
    values_to = "value_total"
  ) %>%
  
  arrange(status_group, year) %>%
  group_by(status_group) %>%
  mutate(
    value_lag  = lag(value_total),
    vol_pct    = (value_total - value_lag) / value_lag * 100,
    vol_abs    = value_total - value_lag
  ) %>%
  ungroup() %>%
  mutate(
    period = as.character(year),
    sort_index = year
  )

saveRDS(volatility_yearly, "data/volatility_yearly.rds")



### conversion ratio: 


conversion_yearly <- fdi_yearly %>%
  select(year, status_group, value) %>%
  pivot_wider(
    names_from = status_group,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(
    conversion_ratio = ifelse(Announced > 0, Opened / Announced, NA_real_),
    period = as.character(year),
    sort_index = year
  )

saveRDS(conversion_yearly, "data/conversion_yearly.rds")




################ total - country

fdi <- fdi %>%
  mutate(year = as.numeric(format(time, "%Y")))   # fast + safe

# Yearly inflow
inflow_base <- fdi %>%
  group_by(dest_country, year, status) %>%
  summarise(
    inflow_value = sum(value, na.rm = TRUE),
    inflow_job   = sum(jobs,  na.rm = TRUE),
    inflow_n     = sum(n_projects, na.rm = TRUE),
    .groups = "drop"
  )

outflow_base <- fdi %>%
  group_by(src_country, year, status) %>%
  summarise(
    outflow_value = sum(value, na.rm = TRUE),
    outflow_job   = sum(jobs,  na.rm = TRUE),
    outflow_n     = sum(n_projects, na.rm = TRUE),
    .groups = "drop"
  )

inflow_total <- inflow_base %>%
  filter(status %in% c("Announced", "Opened")) %>%
  group_by(dest_country, year) %>%
  summarise(
    status       = "Total",
    inflow_value = sum(inflow_value, na.rm = TRUE),
    inflow_job   = sum(inflow_job,   na.rm = TRUE),
    inflow_n     = sum(inflow_n,     na.rm = TRUE),
    .groups = "drop"
  )

fdi_country_inflow <- bind_rows(inflow_base, inflow_total)


outflow_total <- outflow_base %>%
  filter(status %in% c("Announced", "Opened")) %>%
  group_by(src_country, year) %>%
  summarise(
    status       = "Total",
    outflow_value = sum(outflow_value, na.rm = TRUE),
    outflow_job   = sum(outflow_job,   na.rm = TRUE),
    outflow_n     = sum(outflow_n,     na.rm = TRUE),
    .groups = "drop"
  )

fdi_country_outflow <- bind_rows(outflow_base, outflow_total)



saveRDS(fdi_country_inflow,  "data/fdi_country_inflow.rds")
saveRDS(fdi_country_outflow, "data/fdi_country_outflow.rds")

###now totals for map
fdi_join <- fdi_country_inflow %>%
  rename(src_country = dest_country)

fdi_country_flow<- fdi_join %>%
  full_join(fdi_country_outflow, by = c("year", "src_country", "status"))%>%
  rename(country = src_country) %>%
  mutate(
    inflow_value  = replace_na(inflow_value, 0),
    inflow_job    = replace_na(inflow_job, 0),
    inflow_n      = replace_na(inflow_n, 0),
    outflow_value = replace_na(outflow_value, 0),
    outflow_job   = replace_na(outflow_job, 0),
    outflow_n     = replace_na(outflow_n, 0)
  )%>%
  group_by(country, year, status) %>%
  summarize(
    flow_value = sum(inflow_value, na.rm = TRUE) +
      sum(outflow_value, na.rm = TRUE),
    flow_job = sum(inflow_job, na.rm = TRUE) +
      sum(outflow_job, na.rm = TRUE),
    flow_n = sum(inflow_n, na.rm = TRUE) +
      sum(outflow_n, na.rm = TRUE),
    netflow_value = sum(inflow_value, na.rm = TRUE) -
      sum(outflow_value, na.rm = TRUE),
    netflow_job = sum(inflow_job, na.rm = TRUE) -
      sum(outflow_job, na.rm = TRUE),
    netflow_n = sum(inflow_n, na.rm = TRUE) -
      sum(outflow_n, na.rm = TRUE)
  )%>%
  mutate(
    iso3 = countrycode(country, origin = "country.name", destination = "iso3c")
  )


saveRDS(fdi_country_flow, "data/fdi_country_flow.rds")




fdi_country_flow_2 <- fdi_join %>%
  full_join(fdi_country_outflow, 
            by = c("year", "src_country", "status")) %>%
  rename(country = src_country) %>%
  mutate(
    inflow_value  = replace_na(inflow_value, 0),
    inflow_job    = replace_na(inflow_job, 0),
    inflow_n      = replace_na(inflow_n, 0),
    outflow_value = replace_na(outflow_value, 0),
    outflow_job   = replace_na(outflow_job, 0),
    outflow_n     = replace_na(outflow_n, 0)
  ) %>%
  group_by(country, year, status) %>%
  summarise(
    # Inflows
    inflow_value = sum(inflow_value),
    inflow_job   = sum(inflow_job),
    inflow_n     = sum(inflow_n),
    
    # Outflows
    outflow_value = sum(outflow_value),
    outflow_job   = sum(outflow_job),
    outflow_n     = sum(outflow_n),
    
    # Total Flows
    flow_value = inflow_value + outflow_value,
    flow_job   = inflow_job   + outflow_job,
    flow_n     = inflow_n     + outflow_n,
    
    # Net Flows
    netflow_value = inflow_value - outflow_value,
    netflow_job   = inflow_job   - outflow_job,
    netflow_n     = inflow_n     - outflow_n,
    
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = countrycode(country, origin = "country.name", destination = "iso3c")
  )



fdi_agg <- fdi_country_flow_2 %>%
  mutate(
    Aggregate_dest_list = purrr::map(
      iso3,
      ~ {
        # If iso3 is NA → return empty (will be filtered out)
        if (is.na(.x)) return(character(0))
        
        groups <- c(
          if (.x %in% c(
            "FRA","DEU","ITA","ESP","POL","NLD","BEL","SWE","AUT","DNK","FIN",
            "PRT","CZE","GRC","HUN","IRL","ROU","SVK","SVN","HRV","BGR",
            "EST","LVA","LTU","CYP","LUX","MLT")) "EU" else NULL,
          
          if (.x %in% c("CAN","MEX","USA")) "NAFTA" else NULL,
          
          if (.x == "GBR") "UK" else NULL,
          
          if (.x %in% c("BRA","RUS","IND","CHN","ZAF")) "BRICS" else NULL,
          
          if (.x %in% c("ARG","PRY","URY","VEN","BOL","BRA")) "MERCOSUR" else NULL,
          
          if (.x %in% c("IDN","MYS","PHL","SGP","THA","VNM",
                        "BRN","KHM","LAO","MMR")) "ASEAN" else NULL,
          
          if (.x %in% c("KAZ","BLR","ARM","AZE","KGZ","TJK",
                        "TKM","UZB","MDA","RUS")) "CIS" else NULL,
          
          if (.x %in% c(
            "DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","TCD","COM","COG","CIV",
            "COD","DJI","EGY","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN","GNB","KEN",
            "LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS","MYT","MAR","MOZ","NAM","NER",
            "NGA","RWA","STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN","TGO","TUN","UGA",
            "TZA","ZMB","ZWE")) "AFR" else NULL
        )
        
        groups  # may be length 0 → safe
      }
    )
  ) %>%
  unnest(Aggregate_dest_list, keep_empty = FALSE) %>%   # drops empty rows
  rename(aggregate = Aggregate_dest_list)




saveRDS(fdi_agg, "data/aggregates_all_flows.rds")

#colnames(agg_flow)


#sankey

#mapping
country_to_aggregate <- fdi_agg %>%
  distinct(country, iso3, aggregate)



# get sector info --> expect many to many relation

fdi_with_agg_dest <- fdi %>%
  left_join(country_to_aggregate,
            by = c("dest_country" = "country"))

fdi_with_agg_src <- fdi %>%
  left_join(country_to_aggregate,
            by = c("src_country" = "country"))



agg_sec_inflow_base <- fdi_with_agg_dest %>%
  filter(!is.na(aggregate)) %>%      # drop unassigned countries
  group_by(aggregate, year, status, Sector, Subsector, Activity) %>%
  summarise(
    inflow_value = sum(value, na.rm = TRUE),
    inflow_job   = sum(jobs, na.rm = TRUE),
    inflow_n     = sum(n_projects, na.rm = TRUE),
    .groups = "drop"
  )



agg_sec_outflow_base <- fdi_with_agg_src %>%
  filter(!is.na(aggregate)) %>% 
  group_by(aggregate, year, status, Sector, Subsector, Activity) %>%
  summarise(
    outflow_value = sum(value, na.rm = TRUE),
    outflow_job   = sum(jobs,  na.rm = TRUE),
    outflow_n     = sum(n_projects, na.rm = TRUE),
    .groups = "drop"
  )



agg_sec_inflow_total <- agg_sec_inflow_base %>%
  filter(status %in% c("Announced","Opened")) %>%
  group_by(aggregate, year, Sector, Subsector, Activity) %>%
  summarise(
    status = "Total",
    inflow_value = sum(inflow_value),
    inflow_job   = sum(inflow_job),
    inflow_n     = sum(inflow_n),
    .groups = "drop"
  )


agg_sec_outflow_total <- agg_sec_outflow_base %>%
  filter(status %in% c("Announced","Opened")) %>%
  group_by(aggregate, year, Sector, Subsector, Activity) %>%
  summarise(
    status = "Total",
    outflow_value = sum(outflow_value),
    outflow_job   = sum(outflow_job),
    outflow_n     = sum(outflow_n),
    .groups = "drop"
  )



agg_sec_flow <- bind_rows(
  bind_rows(agg_sec_inflow_base, agg_sec_inflow_total),
  bind_rows(agg_sec_outflow_base, agg_sec_outflow_total)
) %>%
  mutate(
    across(
      c(inflow_value, inflow_job, inflow_n,
        outflow_value, outflow_job, outflow_n),
      ~ replace_na(.x, 0)
    )
  ) %>%
  group_by(aggregate, year, status, Sector, Subsector, Activity) %>%
  summarise(
    inflow_value  = sum(inflow_value),
    inflow_job    = sum(inflow_job),
    inflow_n      = sum(inflow_n),
    outflow_value = sum(outflow_value),
    outflow_job   = sum(outflow_job),
    outflow_n     = sum(outflow_n),
    
    flow_value    = inflow_value + outflow_value,
    flow_job      = inflow_job   + outflow_job,
    flow_n        = inflow_n     + outflow_n,
    
    netflow_value = inflow_value - outflow_value,
    netflow_job   = inflow_job   - outflow_job,
    netflow_n     = inflow_n     - outflow_n,
    .groups = "drop"
  )






saveRDS(agg_sec_flow, "data/agg_sankey.rds")





############################ sectors

sec_flow_all <- fdi %>%
  mutate(year = as.numeric(format(time, "%Y"))) %>%
  select(year, Sector, Subsector, Activity, status, value, jobs, n_projects) %>%
  group_by(year, Sector, Subsector, Activity, status) %>%
  summarise(
    flow_value = sum(value, na.rm = TRUE),
    flow_job   = sum(jobs, na.rm = TRUE),
    flow_n     = sum(n_projects, na.rm = TRUE),
    .groups = "drop"
  )

# Total = Announced + Opened
sec_total <- sec_flow_all %>%
  filter(status %in% c("Announced", "Opened")) %>%
  group_by(year, Sector, Subsector, Activity) %>%
  summarise(
    status = "Total",
    flow_value = sum(flow_value),
    flow_job   = sum(flow_job),
    flow_n     = sum(flow_n),
    .groups = "drop"
  )

# Combine statuses
sec_flow_all <- bind_rows(sec_flow_all, sec_total)

# Level datasets
sec_sector <- sec_flow_all %>%
  select(year, status, Sector, flow_value, flow_job, flow_n)

sec_subsector <- sec_flow_all %>%
  select(year, status, Subsector, flow_value, flow_job, flow_n)

sec_activity <- sec_flow_all %>%
  select(year, status, Activity, flow_value, flow_job, flow_n)

sec_sec_act <- sec_flow_all %>%
  select(year, status, Sector, Activity, flow_value, flow_job, flow_n)

saveRDS(sec_sector, "data/sector_df.rds")
saveRDS(sec_subsector, "data/subsector_df.rds")
saveRDS(sec_activity, "data/activity_df.rds")
saveRDS(sec_sec_act, "data/sec_act_df.rds")



########################

####country selector


fdi_country_unique <- fdi %>%
  select(dest_country, src_country) %>%
  pivot_longer(cols = everything(), values_to = "country") %>%
  distinct(country) %>%
  filter(!is.na(country), country != "") %>%
  arrange(country) %>%
  mutate(
    iso3 = countrycode(country, origin = "country.name", destination = "iso3c"),
    iso2 = countrycode(country, origin = "country.name", destination = "iso2c")
  ) %>%
  filter(!is.na(iso3), !is.na(iso2))





saveRDS(fdi_country_unique, "data/country_menu.rds")



#### bilateral: no sectors



fdi_no_sec <- fdi %>%
  select(-c(Sector, Subsector, Activity))%>%
  group_by(src_country, dest_country, year, status)%>%
  summarize(value = sum(value, na.rm = TRUE),
            jobs = sum(jobs, na.rm = TRUE),
            n_projects = sum(n_projects, na.rm = TRUE))%>%
  ungroup()



fdi_no_sec <- fdi_no_sec %>%
  mutate(
    iso3_ref = countrycode(dest_country, origin = "country.name", destination = "iso3c"),
    iso3_cp = countrycode(src_country, origin = "country.name", destination = "iso3c"),
    
  )

####

saveRDS(fdi_no_sec, "data/fdi_only_flows.rds")




fdi_no_sec_m <- fdi %>%
  select(-c(Sector, Subsector, Activity))%>%
  group_by(src_country, dest_country, year, month, status)%>%
  summarize(value = sum(value, na.rm = TRUE),
            jobs = sum(jobs, na.rm = TRUE),
            n_projects = sum(n_projects, na.rm = TRUE))%>%
  ungroup()



fdi_no_sec_m <- fdi_no_sec_m %>%
  mutate(
    iso3_ref = countrycode(dest_country, origin = "country.name", destination = "iso3c"),
    iso3_cp = countrycode(src_country, origin = "country.name", destination = "iso3c"),
    
  )

saveRDS(fdi_no_sec_m, "data/fdi_only_flows_month.rds")








