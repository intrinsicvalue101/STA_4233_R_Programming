# INSTALL LIBRARIES (COMMENT OUT)
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("fastDummies")
# install.packages("ggcorrplot")

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(fastDummies)
library(ggcorrplot)

# ==============================================================================
# PROJECT: Supply Chain Lead Time & Performance Analysis
# DATE:    2026-05-03
# PURPOSE: Clean raw shipment data, impute missing values via group medians, 
#          and analyze manufacturing/transit performance trends.
# ==============================================================================

# ------------------------------------------------------------------------------
# PHASE 1: DATA IMPORTATION 
# ------------------------------------------------------------------------------

# Load raw datasets from Excel
df_raw_shipments <- read_excel("raw_data.xlsx", col_names=T, sheet = 1)
df_fiscal_calendar <- read_excel("raw_data.xlsx", col_names=T, sheet = 2) 

# 1. Initial calculation to detect data entry errors (e.g., negative lead times)
df_cleaning_step1 <- df_raw_shipments %>%
  mutate(
    `In-transit Lead Time` = as.numeric(difftime(`Receipt Date`, `Ship Date`, units = "days")), # In-transit lead time = receipt date - ship date
    `Manufacturing Lead Time` = as.numeric(difftime(`Ship Date`, `PO Download Date`, units = "days")), # Manufacturing lead time = ship date - download date
  )

negative_values_check1 <- df_cleaning_step1 %>%
  filter(`In-transit Lead Time` < 0 | `Manufacturing Lead Time` < 0)

# User created function to correct date chronology
# Ensures logical flow (PO < Ship < Receipt) by sorting dates row-wise.
# This addresses instances where timestamps were swapped in the source system.
sort_dates_chronologically <- function(df, col_names)
{
  df <- df %>% 
    unite("combined_dates", all_of(col_names), sep = ",") %>% # unites columns into combined_dates col
    rowwise() %>% # because order varies row-by-row --> apply changes one row at a time 
    mutate(sorted_dates = paste(sort(strsplit(combined_dates, ",")[[1]]), collapse = ",")) %>% # sorted date vector gets placed into sorted_dates col
    ungroup() %>%
    select(-combined_dates) # clean up the temporary column
}




# ------------------------------------------------------------------------------
# PHASE 2: DATA CLEANING & IMPUTATION
# ------------------------------------------------------------------------------

# 2. Apply chronological sorting and recalculate lead times
df_cleaning_step2 <- df_cleaning_step1 %>%  
  sort_dates_chronologically(col_names = c("PO Download Date", "Ship Date", "Receipt Date")) %>% # fix out of order dates
   separate("sorted_dates", c("PO Download Date", "Ship Date", "Receipt Date"), sep = ",") %>%
  mutate(
    `PO Download Date` = ymd(`PO Download Date`), 
    `Ship Date`        = ymd(`Ship Date`),
    `Receipt Date`     = ymd(`Receipt Date`)) %>%
   mutate(
     `In-transit Lead Time` = as.numeric(difftime(`Receipt Date`, `Ship Date`, units = "days")), # calculate lead times again with dates correctly ordered
     `Manufacturing Lead Time` = as.numeric(difftime(`Ship Date`, `PO Download Date`, units = "days")),
  )

negative_values_check2 <- df_cleaning_step2 %>%
  filter(`In-transit Lead Time` < 0 | `Manufacturing Lead Time` < 0)

# 3. Map shipments to fiscal periods based on the physical Receipt Date
df_cleaning_step3 <- df_cleaning_step2 %>%
  as.data.frame() %>%
  left_join(
    df_fiscal_calendar[, c("Start_Date", "End_date", "Quarter", "Year")], 
    by = join_by(between(`Receipt Date`, Start_Date, End_date))) %>% 
  select(!c(Start_Date, End_date))

colSums(is.na(df_cleaning_step3))
sum(is.na(df_cleaning_step3))

# 4. Calculate imputation values using group-level medians (robust against outliers)
median_lookup_transit <- df_cleaning_step3 %>%
  group_by(LOB, Origin, `Ship Mode`) %>% 
  summarise(median_transit = as.integer(round(median(`In-transit Lead Time`, na.rm = TRUE), 2)), 
            .groups = "drop")

median_lookup_manufacturing <- df_cleaning_step3 %>%
  group_by(LOB, Origin, `Ship Mode`) %>%
  summarise(median_manuf = as.integer(round(median(`Manufacturing Lead Time`, na.rm = TRUE), 2)), 
            .groups = "drop")

# 5. Impute missing lead times and derive missing dates
df_final_clean <- df_cleaning_step3 %>%
  left_join(median_lookup_transit, by = c("Origin", "LOB", "Ship Mode")) %>%
  mutate(`In-transit Lead Time` = coalesce(`In-transit Lead Time`, median_transit)) %>% # replace all missing lead time values with median
  left_join(median_lookup_manufacturing, by = c("Origin", "LOB", "Ship Mode")) %>%
  mutate(`Manufacturing Lead Time` = coalesce(`Manufacturing Lead Time`, median_manuf)) %>% 
  select(-median_transit, -median_manuf) %>%
  mutate(`Ship Date` = as.Date(coalesce(`Ship Date`, `PO Download Date` + days(`Manufacturing Lead Time`)))) %>% # calculate new ship/receipt dates based off of imputed lead time info
  mutate(`Receipt Date` = as.Date(coalesce(`Receipt Date`, `Ship Date` + days(`In-transit Lead Time`)))) 

colSums(is.na(df_final_clean))
sum(is.na(df_final_clean))

# Patch metadata (Qtr/Year) for rows where dates were originally missing but now imputed
df_metadata_patch <- df_final_clean %>%
  filter(is.na(Quarter)) %>%
  select(-Quarter, -Year) %>% 
  mutate(`Ship Date` = as.Date(`Ship Date`)) %>%
  left_join(df_fiscal_calendar %>%
              mutate(Start_Date = as.Date(Start_Date), End_date = as.Date(End_date)), 
            by = join_by(between(`Receipt Date`, Start_Date, End_date))) %>% 
  select(-Start_Date, -End_date, -`S.No`) %>%
  distinct() # Ensure keys are unique so rows_patch can identify specific rows to update

df_final_clean <- df_final_clean %>%
  rows_patch(df_metadata_patch, by = c("Origin", "LOB", "Ship Mode", "PO Download Date", "Ship Date", "Receipt Date")) %>%
  select(-c("PO Download Date", "Ship Date", "Receipt Date")) # Remove columns not needed anymore

colSums(is.na(df_final_clean))

df_final_clean <- df_final_clean %>%
  drop_na(Quarter, Year) # Remove rows where qtr or year don't exist within the fiscal calendar




# ------------------------------------------------------------------------------
# PHASE 3: EXPLORATORY DATA ANALYSIS 
# ------------------------------------------------------------------------------

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
# SUBSECTION 1: CENTRAL TENDENCY ANALYSIS
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

summary(df_final_clean)

# Mean and Median Manufacturing Lead Time by Site Origin
manufacturing_avg_by_odm <- df_final_clean %>%
  group_by(Origin) %>% 
  summarise(mean_manufacturing = round(mean(`Manufacturing Lead Time`, na.rm = TRUE), 1),
            median_manufacturing = round(median(`Manufacturing Lead Time`, na.rm = TRUE), 1), 
            total_shipments = n(), .groups = "drop") %>%
  arrange(median_manufacturing)

# Mean and Median In-transit Lead Time by Site Origin
transit_avg_by_odm <- df_final_clean %>%
  group_by(Origin) %>% 
  summarise(mean_transit = round(mean(`In-transit Lead Time`, na.rm = TRUE), 1),
    median_transit = round(median(`In-transit Lead Time`, na.rm = TRUE), 1),
    total_shipments = n(), .groups = "drop") %>%
  arrange(median_transit)

# Visualize Manufacturing Lead Times by Site Origin
plot_median_manufacturing_by_odm <- manufacturing_avg_by_odm %>% 
  ggplot(aes(x = Origin, y = median_manufacturing, fill = Origin)) +
  geom_col() +
  geom_label(aes(label = median_manufacturing), color = "black", fill = "white", size = 20 / .pt) +
  theme_bw() +
  labs(fill = NULL, y = "Median Manufacturing Time (Days)", x = "ODM Site") +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 20),
        legend.position = c(.8, .8), legend.text = element_text(size = 20)) + 
  scale_fill_brewer(palette = "Set2") 

ggsave(filename = "median_manufacturing_times_by_site.png", width = 10, height = 8, dpi = 300, bg = "white")

# Visualize Transit Lead Times by Site Origin
plot_median_transit_by_odm <- transit_avg_by_odm %>%
  ggplot(aes(x = Origin, y = median_transit, fill = Origin)) +
  geom_col() +
  geom_label(aes(label = median_transit), color = "black", fill = "white", size = 20 / .pt) +
  labs(fill = NULL, y = "Median Transit Lead Time (Days)", x = "ODM Site") +
  theme_bw() +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 20),
        legend.position = c(.8, .5), legend.text = element_text(size = 20)) +
  scale_fill_brewer(palette = "Set2") 

ggsave(filename = "median_transit_times_by_site.png", width = 10, height = 8, dpi = 300, bg = "white")

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
# SUBSECTION 2: VARIABILITY & RISK ANALYSIS
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# IQR metrics to evaluate site consistency/predictability
manufacturing_IQR_by_odm <- df_final_clean %>%
  group_by(Origin) %>%
  summarise(manufacturing_IQR = round(IQR(`Manufacturing Lead Time`), 1),
            total_shipments = n(), .groups = "drop") %>%
  arrange(manufacturing_IQR)

delivery_IQR_by_odm <- df_final_clean %>%
  group_by(Origin) %>%
  summarise(transit_IQR = round(IQR(`In-transit Lead Time`), 1),
            total_shipments = n(), .groups = "drop") %>%
  arrange(transit_IQR)

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
# SUBSECTION 3: OUTLIER ANALYSIS
# Identification of extreme lead time events based on 1.5 * IQR
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# Calculate Lower (Q1) and Upper Bounds (Q3) by Site Origin
delivery_quantiles_by_odm <- df_final_clean %>%
  select(Origin, `In-transit Lead Time`) %>%
  group_by(Origin) %>%
  summarize(q1_lead_time = quantile(`In-transit Lead Time`, 0.25, na.rm = TRUE, names = FALSE),
            q3_lead_time = quantile(`In-transit Lead Time`, 0.75, na.rm = TRUE, names = FALSE)) 

# Create Table containing both IQR and Q1/Q3 variables
delivery_leadtime_and_quantiles <- df_final_clean %>% # we need in-transit lead time, site origin, IQR, q1, q2
  select(Origin, `In-transit Lead Time`) %>% 
  left_join(delivery_IQR_by_odm, by = "Origin") %>%
  left_join(delivery_quantiles_by_odm, by = "Origin")

# Get Outlier Percentages for In-Transit Lead Times by Site Origin
transit_outlier_counts <- delivery_leadtime_and_quantiles %>%
  group_by(Origin) %>%
  mutate(lower_bound = q1_lead_time - (1.5 * transit_IQR),
         upper_bound = q3_lead_time + (1.5 * transit_IQR)) %>%
  filter(`In-transit Lead Time` < lower_bound | `In-transit Lead Time` > upper_bound) %>%
  summarize(
    outlier_count = n(), 
    total_shipments = first(total_shipments), 
    outlier_proportion_pct = round((outlier_count / total_shipments) * 100, 1)
  ) %>%
  arrange(outlier_proportion_pct)

plot_transit_outlier_counts <- transit_outlier_counts %>%
  ggplot(aes(x = Origin, y = outlier_proportion_pct, fill = Origin)) +
  geom_col() +
  geom_label(aes(label = paste(outlier_proportion_pct, "%")), color = "black", fill = "white", size = 20 / .pt) +
  theme_bw() +
  labs(fill = NULL, y = "Percent of In-Transit Lead Time Outliers (%)", x = "ODM Site") +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 20),
        legend.position = c(.8, .8), legend.text = element_text(size = 20)) + 
  scale_fill_brewer(palette = "Set2") 

ggsave(filename = "transit_outliers_by_site.png", width = 10, height = 8, dpi = 300, bg = "white")

# Calculate Lower (Q1) and Upper Bounds (Q3) by Site Origin
manufacturing_quantiles_by_odm <- df_final_clean %>%
  select(Origin, `Manufacturing Lead Time`) %>%
  group_by(Origin) %>%
  summarize(q1_lead_time = quantile(`Manufacturing Lead Time`, 0.25, na.rm = TRUE, names = FALSE),
            q3_lead_time = quantile(`Manufacturing Lead Time`, 0.75, na.rm = TRUE, names = FALSE))

# Create Table containing both IQR and Q1/Q3 variables
manufacturing_leadtime_and_quantiles <- df_final_clean %>% # we need in transit lead time, site origin, IQR, q1, q2
  select(Origin, `Manufacturing Lead Time`) %>% 
  left_join(manufacturing_IQR_by_odm, by = "Origin") %>%
  left_join(manufacturing_quantiles_by_odm, by = "Origin")

# Get Outlier Percentages for Manufacturing Lead Times by Site Origin
manufacturing_outlier_counts <- manufacturing_leadtime_and_quantiles %>%
  group_by(Origin) %>%
  mutate(lower_bound = q1_lead_time - (1.5 * manufacturing_IQR),
         upper_bound = q3_lead_time + (1.5 * manufacturing_IQR)) %>%
  filter(`Manufacturing Lead Time` < lower_bound | `Manufacturing Lead Time` > upper_bound) %>%
  summarize(
    outlier_count = n(), 
    total_shipments = first(total_shipments), 
    outlier_proportion_pct = round((outlier_count / total_shipments) * 100, 1)
  ) %>%
  arrange(outlier_proportion_pct)

plot_manufacturing_outlier_counts <- manufacturing_outlier_counts %>%
  ggplot(aes(x = Origin, y = outlier_proportion_pct, fill = Origin)) +
  geom_col() +
  geom_label(aes(label = paste(outlier_proportion_pct, "%")), color = "black", fill = "white", size = 20 / .pt) +
  theme_bw() +
  labs(fill = NULL, y = "Percent of Manufacturing Lead Time Outliers (%)", x = "ODM Site") +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 20),
        legend.position = c(.8, .8), legend.text = element_text(size = 20)) + 
  scale_fill_brewer(palette = "Set2") 

ggsave(filename = "manuf_outliers_by_site.png", width = 10, height = 8, dpi = 300, bg = "white")

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
# SUBSECTION 4: TEMPORAL TRENDS
# Monitoring shifts in site performance across fiscal quarters
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# Compare the variance (spread) of lead times in earlier quarters versus the peak pandemic quarters.
quarterly_transit_IQR <- df_final_clean  %>% 
  group_by(Quarter, Year) %>%
  summarize(IQR = round(IQR(`In-transit Lead Time`),1), .groups = "drop") %>%
  mutate(Timepoint = paste(Quarter, Year)) %>%
  group_by(Year, Quarter) %>%
  mutate(Timepoint = factor(Timepoint, levels = unique(Timepoint)))

quarterly_manufacturing_IQR <- df_final_clean %>% 
  group_by(Quarter, Year) %>%
  summarize(IQR = round(IQR(`Manufacturing Lead Time`),1), .groups = "drop") %>%
  mutate(Timepoint = paste(Quarter, Year)) %>%
  arrange(Year, Quarter) %>%
  mutate(Timepoint = factor(Timepoint, levels = unique(Timepoint)))

plot_quarterly_manufacturing_IQR <- ggplot(quarterly_manufacturing_IQR, aes(x = Timepoint, y = IQR, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  theme_bw() +
  geom_label(
    aes(label = IQR), 
    color = "black",
    fill = "white", 
    size = 20 / .pt,
    vjust = -.5
  ) +
  labs(x = "Timestamp", y = "IQR") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 20)) +
  theme(
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    legend.position = "none",
    strip.text = element_text(size = 20, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set1") 

ggsave(
  filename = "quarterly_manufacturing_IQR.png",
  width = 10,           
  height = 8,           
  dpi = 300,            
  bg = "white"          
)

plot_quarterly_transit_IQR <- ggplot(quarterly_transit_IQR, aes(x = Timepoint, y = IQR, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  theme_bw() +
  geom_label(
    aes(label = IQR), 
    color = "black",
    fill = "white", 
    size = 20 / .pt,
    vjust = -.5
  ) +
  labs(x = "Timestamp", y = "IQR") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 60)) +
  theme(
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    legend.position = "none",
    strip.text = element_text(size = 20, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set1") 

ggsave(
  filename = "quarterly_transit_IQR.png",
  width = 10,           
  height = 8,           
  dpi = 300,            
  bg = "white"          
)

# Track quarterly shifts in median performance and consistency 
quarterly_performance <- df_final_clean  %>% 
  group_by(Quarter, Year, Origin) %>%
  summarize(median_transit = round(median(`In-transit Lead Time`), 1),
            median_manufacturing = round(median(`Manufacturing Lead Time`), 1),
            transit_IQR = round(IQR(`In-transit Lead Time`), 1), 
            manufacturing_IQR = round(IQR(`Manufacturing Lead Time`), 1), .groups = "drop") %>%
  mutate(Timepoint = paste(Quarter, Year)) %>%
  group_by(Year, Quarter) %>%
  mutate(Timepoint = factor(Timepoint, levels = unique(Timepoint)))

# Visualizing Manufacturing Lead Time Trends across fiscal quarters by site
plot_median_manufacturing_trends <- ggplot(quarterly_performance, aes(x = Timepoint, y = median_manufacturing, group = 1, color = Origin)) +
  geom_line(linewidth = 2) + geom_point(size = 4) +
  theme_bw() +
  geom_label(aes(label = median_manufacturing), color = "black", fill = "white", size = 20 / .pt, vjust = -.25) +
  facet_wrap(~Origin, scale = "free") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  labs(x = "Fiscal Quarter", y = "Median Manufacturing Lead Time (Days)") +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 15),
        strip.text = element_text(size = 20, face = "bold"), legend.position = "none",
        plot.margin = margin(t = 5, r = 2, b = 2, l = 2)) 

ggsave(filename = "quarterly_manufacturing_times_by_origin.png", width = 10, height = 8, dpi = 300, bg = "white")

# Visualizing transit Lead Time Trends across fiscal quarters by site
plot_median_transit_trends <- ggplot(quarterly_performance, aes(x = Timepoint, y = median_transit, group = 1, color = Origin)) +
  geom_line(linewidth = 2) + geom_point(size = 4) +
  theme_bw() +
  geom_label(aes(label = median_transit), color = "black", fill = "white", size = 20 / .pt, vjust = -.5) +
  facet_wrap(~Origin, scale = "free") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.25))) +
  labs(x = "Fiscal Quarter", y = "Median Transit Lead Time (Days)") +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 15),
        strip.text = element_text(size = 20, face = "bold"), legend.position = "none",
        plot.margin = margin(t = 5, r = 2, b = 2, l = 2)) 

ggsave(filename = "quarterly_transit_times_by_origin.png", width = 10, height = 8, dpi = 300, bg = "white")

# Track Orders Delivered Per Quarter
quarterly_delivery_volume <- df_final_clean %>%
  mutate(Timepoint = paste(Quarter, Year)) %>% 
  group_by(Timepoint, Year, Quarter) %>%
  summarize(total_orders = n(), .groups = "drop") %>%
  group_by(Year, Quarter) %>%
  mutate(Timepoint = factor(Timepoint, levels = unique(Timepoint)))

plot_quarterly_delivery_volume <- ggplot(quarterly_delivery_volume, aes(x = Timepoint, y = total_orders, group = 1)) +
  geom_line(linewidth = 2, color = "#8856a7") + 
  geom_point(size = 4, color = "#8856a7") +
  theme_bw() +
  coord_cartesian(ylim = c(0,4500)) +
  geom_label(aes(label = total_orders), color = "black", fill = "white", size = 20 / .pt, vjust = -.5) +
  labs(x = "Fiscal Quarter", y = "Number of Orders Delivered") +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 20))

ggsave(filename = "quarterly_orders_delivered.png", width = 10, height = 8, dpi = 300, bg = "white")

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
# SUBSECTION 4: CATEGORICAL ANALYSIS
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# Density analysis: Comparing transit efficiency across different Ship Modes
density_by_shipmode <- ggplot(df_final_clean, aes(x = `In-transit Lead Time`, fill = `Ship Mode`)) +
  geom_density(alpha = 0.5, bw = 1.5) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  scale_x_continuous(breaks = seq(0, 55, by = 5), limits = c(-2, 55)) + # Used a range to only capture where bulk of data lies + create more visually appealing graph
  theme_bw() +
  labs(fill = NULL, 
       x = "Transit Lead Time (Days)", 
       y = "Density") +
  theme(
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    legend.position = c(0.8,.5),
    legend.text = element_text(size = 20)
    )

ggsave(filename = "transit_shipmode_lead_times_density_graph.png", width = 10, height = 8, dpi = 300, bg = "white")

# Median and IQR of In Transit Lead Time by Ship Mode
transit_performance_by_shipmode <- df_final_clean %>%
  group_by(`Ship Mode`) %>%
  summarize(median_transit = round(median(`In-transit Lead Time`, na.rm = TRUE), 1),
            IQR_transit = round(IQR(`In-transit Lead Time`, na.rm = TRUE), 1),
            total_orders = n(), .groups = "drop") %>%
  arrange(median_transit)

# Manufacturing Time by Line of Business (LOB) 
plot_manufacturing_by_LOB <- df_final_clean %>%
  ggplot(aes(x = `Manufacturing Lead Time`, fill = LOB)) +
  geom_histogram(position = "identity", binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(xlim = c(0,30)) + 
  facet_wrap(~LOB, ncol = 1, scale = "free") + 
  labs(x = "Manufacturing Lead Time (Days)", y = "Order Count") +
  theme_bw() +
  theme(axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size = 20),
        strip.text = element_text(size = 20, face = "bold"), legend.position = "none")

ggsave(filename = "manufacturing_LOB_graph.png", width = 10, height = 8, dpi = 300, bg = "white")

# Summary Metrics for Manufacturing prioritization by LOB
manufacturing_performance_by_LOB <- df_final_clean %>%
  group_by(`LOB`) %>%
  summarize(median_manuf = round(median(`Manufacturing Lead Time`, na.rm = TRUE), 1),
            IQR_manuf = round(IQR(`Manufacturing Lead Time`, na.rm = TRUE), 1),
            total_orders = n(), .groups = "drop") %>%
  arrange(median_manuf)

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
# SUBSECTION 5: CORRELATION ANALYSIS
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# Correlation Matrix: Relationship between categorical factors and Lead Time
df_encoded <- df_final_clean %>%
  select(c("LOB", "Origin", "Ship Mode", "Quarter", "In-transit Lead Time")) %>% 
  dummy_cols(select_columns = c("LOB", "Origin", "Ship Mode", "Quarter"), remove_selected_columns = TRUE)

cor_matrix <- cor(df_encoded, use = "complete.obs") 

lead_time_correlations <- cor_matrix[, "In-transit Lead Time"] # Extract only the correlations related to 'In-transit Lead Time'

cor_df <- data.frame( # Convert to a dataframe for easier sorting
  Variable = names(lead_time_correlations),
  Correlation = round(as.numeric(lead_time_correlations), 4)
)

# Output sorted correlation results
sorted_table <- cor_df %>% 
  arrange(desc(Correlation))

print(sorted_table)


