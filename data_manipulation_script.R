# Load libraries

library(tidyverse)
library(jsonlite)

# Import Data adding in columns for events
room_key_data <- read_tsv("data.tsv", progress = FALSE) %>% 
  rownames_to_column() %>% 
  rename(key = rowname) %>% 
  mutate(lead_event = !(lead == "null"), 
         booking_event = !(booking == "null"))

# Create referral data set and referral key set
referral_key <- room_key_data %>% 
  filter(referral != "null") %>% 
  select(key, lead_event, booking_event)

referral <- room_key_data %>% 
  filter(referral != "null") %>%
  .$referral %>% 
  map_df(fromJSON) %>% 
  bind_cols(referral_key) %>% 
  select(key, everything()) %>% 
  rename(visitor_id = `visitor-id`, 
         event_id_referral = `event-id`, 
         ts_referral = ts,
         location_tid = `location-tid`, 
         partner_code = `partner-code`, 
         check_in = `check-in`, 
         check_out = `check-out`)

# Create lead data set and key set
lead_key <- room_key_data %>% 
  filter(lead != "null") %>% 
  select(key, booking_event)

lead <- room_key_data %>% 
  filter(lead != "null") %>%
  .$lead %>% 
  map_df(fromJSON) %>% 
  bind_cols(lead_key) %>% 
  select(key, everything()) %>% 
  rename(visitor_id = `visitor-id`, 
         event_id_lead = `event-id`, 
         ts_lead = ts, 
         rate_partner = `rate-partner`)

# Create booking data set and key set
booking_key <- room_key_data %>% 
  filter(booking != "null") %>% 
  select(key)

booking <- room_key_data %>% 
  filter(booking != "null") %>%
  .$booking %>%  
  map_df(fromJSON) %>% 
  bind_cols(booking_key) %>% 
  select(key, everything())

# Filter referral info for when leads are TRUE
referral_info <- referral %>% 
  filter(lead_event == TRUE) %>% 
  select(-event_id_referral, -type, -lead_event, -booking_event)

# Join referral information to the lead data set and do some additional 
# mutations
lead_ref <- lead %>% 
  select(-event_id_lead, - type) %>% 
  left_join(referral_info, by = c("key", "visitor_id")) %>% 
  rename(rate_partner_lead = rate_partner, 
         partner_code_referral = partner_code) %>% 
  mutate(check_in = lubridate::ymd(check_in), 
         check_out = lubridate::ymd(check_out), 
         check_in_diff = as.numeric(check_in - lubridate::ymd("2017-10-15")), 
         check_out_diff = as.numeric(check_out - lubridate::ymd("2017-10-15")))

# Create output file for modified lead data set

write_csv(lead_ref, "lead_ref_data.csv")