################################################################################ .
#                                                                              #
#                       LOCALITY PROFILES: HOUSEHOLDS                          #
#                                                                              #
################################################################################ .
###
### The purpose of this code is to produce graphs which will be used for locality
### profiles produced in RMarkdown.
###
### When rerunning for a new year of data, manually change dates at lines 39 and 41
###
### First Created: 05/09/2019
### UPDATES
### Oct 2021  Aidan Morrison
### Sep 2022  Diane Wilson  Updated to use functions within Global Script and updated palette
### March 2025 Andrew Saul
###
### Contents:
### Section 1 - Packages, working directory etc
### Section 2 - Households Data
### Section 3 - Council Tax Band Data
### Section 4 - Objects for Summary Table


##################### Section 1 - Packages, working directory etc ########################

# load in required packages
library(readxl)
library(reshape2)

# Set paths
household_xlsx_path <- paste0(data_path, "Households/", "Data ", ext_year, "/household_estimates.xlsx")
counciltax_xlsx_path <- paste0(data_path, "Households/", "Data ", ext_year, "/council_tax.xlsx")
# Read in Global Script for RMarkdown (For testing only)
# source("Master RMarkdown Document & Render Code/Global Script.R")

# Set locality (for testing only)
# LOCALITY <- "Whalsay and Skerries"
# LOCALITY <- "Ayr North and Former Coalfield Communities"


##################### Section 2 - Households Data #############################

## 2a) Data imports & cleaning ----

# extract sheets with year names
household_sheet_titles <- 
  excel_sheets(household_xlsx_path) %>% 
  str_subset(., "^\\d+$")

# Extract data for each year
house_raw_dat <- 
  map(household_sheet_titles, ~read_excel(household_xlsx_path, 
                                sheet = .x,
                                skip = 3) %>% 
        mutate(year = as.numeric(.x))
      ) %>% 
  bind_rows() %>% 
  clean_names() %>% 
  relocate(year)

# Global Script Function to read in Localities Lookup
lookup_dz <- 
  read_in_localities(dz_level = TRUE) %>%
  select(datazone2011, hscp_locality) %>%
  filter(hscp_locality %in% locality_list)


# filter housing data for locality of interest and save locality column
house_dat <- house_raw_dat %>% 
  left_join(lookup_dz, join_by(data_zone_code == datazone2011)) %>% 
  filter(data_zone_code %in% lookup_dz$datazone2011)

# aggregate data
house_dat1 <- house_dat %>%
  dplyr::group_by(year, hscp_locality) %>%
  dplyr::summarise(
    total_dwellings = sum(total_number_of_dwellings),
    occupied_dwellings = sum(occupied_dwellings),
    vacant_dwellings = sum(vacant_dwellings),
    second_homes = sum(second_homes),
    tax_exempt = sum(occupied_dwellings_exempt_from_paying_council_tax),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(3:7, list(perc = ~ 100 * .x / total_dwellings)))

loc_text_value <- function(df = house_dat1, 
                                loc = locality_list, 
                                years = household_sheet_titles, 
                                col_name) {
  max_year <- max(years) # latest sheet year
  map(loc,
      ~filter(df, hscp_locality == .x,
              year == max_year) %>% 
        pull({{col_name}}) %>% 
        format_number_for_text()) %>% 
    set_names(loc)
  
}


## 2b) Text objects stored as lists, each containing the values from the two localities----
# numbers
n_houses <- loc_text_value(col_name = total_dwellings)
n_occupied <- loc_text_value(col_name = occupied_dwellings)
n_vacant <- loc_text_value(col_name = vacant_dwellings)
n_single_discount <- loc_text_value(col_name = tax_discount)
n_exempt <- loc_text_value(col_name = tax_exempt)
n_second_homes <- loc_text_value(col_name = second_homes)

# percentages
perc_occupied <- loc_text_value(col_name = occupied_dwellings_perc)
perc_vacant <- loc_text_value(col_name = vacant_dwellings_perc)
perc_single_discount <- loc_text_value(col_name = tax_discount_perc)
perc_exempt <- loc_text_value(col_name = tax_exempt_perc)
perc_second_homes <- loc_text_value(col_name = second_homes_perc)


## 2c) Plots and Tables ----

# Total dwellings over time
houses_ts <- list()
for (loc in locality_list){
houses_ts[[loc]] <- ggplot(house_dat1 %>% filter(hscp_locality == loc), 
                    aes(x = year, y = total_dwellings, group = 1)) +
  geom_line(linewidth = 1, colour = "#3F3685") +
  theme_profiles() +
  geom_point(color = "#3F3685") +
  geom_text(aes(label = format(total_dwellings, big.mark = ",")),
    vjust = 2, color = "#4a4a4a", size = 3.5
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1.1 * max(house_dat1$total_dwellings))) +
  labs(
    x = "Year", y = "Number of Dwellings",
    title = paste0("Number of Dwellings by Year in ", str_wrap(`loc`, 40), " ", max(household_sheet_titles)),
    caption = "Source: Council Tax billing system (via NRS)"
  ) +
  theme(plot.title = element_text(size = 12))
}

# Table
house_table <- 
  map(locality_list,
      ~filter(house_dat1, hscp_locality == .x) %>%
        select(year, total_dwellings, occupied_dwellings, vacant_dwellings,
               tax_discount, tax_exempt, second_homes) %>%
  mutate(across(2:7, ~ format(.x, big.mark = ",")))
  ) %>% 
  set_names(locality_list)


######################## Section 3 - Council Tax Band Data ############################

## 3a) Data imports & cleaning ----

# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings

ctb_sheet_titles <- 
  excel_sheets(counciltax_xlsx_path) %>% 
  str_subset(., "^\\d+$")

# select latest year ctb
house_raw_dat2 <- read_excel(counciltax_xlsx_path,
  sheet = as.character(max(ctb_sheet_titles)), skip = 4
) %>%
  clean_names()

# Filter and aggregate
house_dat2 <- house_raw_dat2 %>%
  left_join(lookup_dz, join_by(data_zone_code == datazone2011)) %>% 
  filter(data_zone_code %in% lookup_dz$datazone2011) %>%
  select(hscp_locality, 5:14) %>%
  group_by(hscp_locality) %>% 
  summarise(across(everything(), sum))


## 3b) Plots & tables ----

ctb <- house_dat2 %>%
  select(hscp_locality, council_tax_band_a:council_tax_band_h) %>%
  pivot_longer(council_tax_band_a:council_tax_band_h) %>% 
  mutate(name = str_extract(name, ".$") %>% str_to_upper(.))

pal_ctb <- phsstyles::phs_colours(c(
  "phs-magenta", "phs-magenta-80", "phs-magenta-50", "phs-magenta-10",
  "phs-purple-30", "phs-purple-50", "phs-purple-80", "phs-purple"
))

ctb_plot <- list()
ctb_table <- list()

for(loc in locality_list){

ctb_plot[[loc]] <- 
  filter(ctb, hscp_locality == loc) %>% 
  ggplot(aes(
    x = value,
    y = 1,
    fill = factor(name, levels = rev(name))
  )) +
  geom_col(position = "fill", colour = "black", linewidth = 0.5, orientation = "y") +
  theme_classic() +
  labs(x = "Proportion of Households", y = "", caption = "Source: Scottish Assessors’ Association (via NRS)") +
  scale_fill_manual(
    name = "Council Tax Band",
   # labels = paste("Band", LETTERS[8:1]),
    values = pal_ctb,
    drop = FALSE,
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

ctb_table[[loc]] <- 
  filter(ctb, hscp_locality == loc) %>% 
  mutate(percent = paste0(format_number_for_text(100 * value / sum(value)), "%")) %>%
  select(-value) %>%
  pivot_wider(names_from = name, values_from = percent) %>%
  mutate(`Tax Band` = "Percent of households") %>%
  select(-hscp_locality)

}

## Objects for locality
calculate_perc_ctb <- function(df = house_dat2, 
                           loc = locality_list, 
                           col_names) {
  map(loc,
      ~filter(df, hscp_locality == .x) %>% 
        select(total_number_of_dwellings, all_of(col_names)) %>%
         pivot_longer(all_of(col_names)) %>% 
        group_by(total_number_of_dwellings) %>% 
         summarise(value = sum(value)) %>% 
        mutate(perc = 100*value/total_number_of_dwellings) %>% 
        pull(perc) %>% 
        format_number_for_text()
    #     format_number_for_text(.data$ctb_range_sum/total_number_of_dwellings * 100)) %>% 
    # set_names(loc)
    # 
  ) %>% 
    set_names(locality_list)
}

perc_houses_AC <-
  calculate_perc_ctb(col_names = c("council_tax_band_a", "council_tax_band_b", "council_tax_band_c"))
 
# format_number_for_text(sum(
#   house_dat2$council_tax_band_a,
#   house_dat2$council_tax_band_b,
#   house_dat2$council_tax_band_c
# ) / house_dat2$total_number_of_dwellings * 100)

perc_houses_FH <- 
  calculate_perc_ctb(col_names = c("council_tax_band_f", "council_tax_band_g", "council_tax_band_h"))
  
#   format_number_for_text(sum(
#   house_dat2$council_tax_band_f,
#   house_dat2$council_tax_band_g,
#   house_dat2$council_tax_band_h
# ) / house_dat2$total_number_of_dwellings * 100)


########################## Section 4 - Objects for Summary Table ########################

## Relevant lookups for creating the table objects

# Global Script Function to read in Localities Lookup
lookup2 <- read_in_localities(dz_level = FALSE)

# Determine HSCP and HB based on Loc
HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)

# Determine other localities based on LOCALITY object
other_locs <- lookup2 %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup2, HSCP)

rm(lookup2)


# 1. Other localities

# Global Script Function to read in Localities Lookup
other_locs_dz <- read_in_localities(dz_level = TRUE) %>%
  arrange() %>%
  dplyr::select(datazone2011, hscp_locality) %>%
  inner_join(other_locs, by = c("hscp_locality" = "hscp_locality"))

house_dat_otherlocs <- house_raw_dat %>%
  inner_join(other_locs_dz, by = c("data_zone_code" = "datazone2011")) %>%
  filter(year == max(year)) %>%
  group_by(hscp_locality) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  dplyr::ungroup() %>%
  mutate(tax_discount_perc = round_half_up(tax_discount / total_dwellings * 100, 1))

other_locs_n_houses <- house_dat_otherlocs %>%
  mutate(tot_dwellings_chr = formatC(total_dwellings, format = "d", big.mark = ",")) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, tot_dwellings_chr) %>%
  pivot_wider(names_from = hscp_locality, values_from = tot_dwellings_chr)

other_locs_perc_discount <- house_dat_otherlocs %>%
  select(hscp_locality, tax_discount_perc) %>%
  arrange(hscp_locality) %>%
  pivot_wider(names_from = hscp_locality, values_from = tax_discount_perc)


house_dat2_otherlocs <- house_raw_dat2 %>%
  inner_join(other_locs_dz, by = c("data_zone_code" = "datazone2011")) %>%
  group_by(hscp_locality) %>%
  summarise(
    total_number_of_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  mutate(
    perc_houses_AC = round_half_up((band_a + band_b + band_c) / total_number_of_dwellings * 100, 1),
    perc_houses_FH = round_half_up((band_f + band_g + band_h) / total_number_of_dwellings * 100, 1)
  )

other_locs_perc_housesAC <- house_dat2_otherlocs %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, perc_houses_AC) %>%
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_AC)

other_locs_perc_housesFH <- house_dat2_otherlocs %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, perc_houses_FH) %>%
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_FH)

rm(house_dat2_otherlocs, house_dat_otherlocs, other_locs_dz)


# 2. HSCP

# Global Script Function to read in Localities Lookup
hscp_dz <- read_in_localities(dz_level = TRUE) %>%
  select(datazone2011, hscp2019name) %>%
  filter(hscp2019name == HSCP)


house_dat_hscp <- house_raw_dat %>%
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  filter(year == max(year)) %>%
  group_by(year) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  ungroup() %>%
  mutate(perc_discount = round_half_up(tax_discount / total_dwellings * 100, 1))

hscp_n_houses <- format_number_for_text(house_dat_hscp$total_dwellings)
hscp_perc_discount <- house_dat_hscp$perc_discount


house_dat2_hscp <- house_raw_dat2 %>%
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  group_by(hscp2019name) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  ungroup() %>%
  mutate(
    perc_houses_AC = round_half_up((band_a + band_b + band_c) / total_dwellings * 100, 1),
    perc_houses_FH = round_half_up((band_f + band_g + band_h) / total_dwellings * 100, 1)
  )

hscp_perc_housesAC <- house_dat2_hscp$perc_houses_AC
hscp_perc_housesFH <- house_dat2_hscp$perc_houses_FH

rm(hscp_dz, house_dat_hscp, house_dat2_hscp)


# 3. Scotland
scot_n_houses <- format_number_for_text(sum(filter(house_raw_dat, year == max(year))$total_number_of_dwellings, na.rm = TRUE))
scot_perc_discount <- format_number_for_text(sum(filter(house_raw_dat, year == max(year))$dwellings_with_a_single_adult_council_tax_discount, na.rm = TRUE) / sum(filter(house_raw_dat, year == max(year))$total_number_of_dwellings, na.rm = TRUE) * 100)

scot_perc_housesAC <- format_number_for_text(sum(house_raw_dat2$council_tax_band_a,
  house_raw_dat2$council_tax_band_b,
  house_raw_dat2$council_tax_band_c,
  na.rm = TRUE
) / sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) * 100)
scot_perc_housesFH <- format_number_for_text(sum(house_raw_dat2$council_tax_band_f,
  house_raw_dat2$council_tax_band_g,
  house_raw_dat2$council_tax_band_h,
  na.rm = TRUE
) / sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) * 100)
