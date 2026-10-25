# Global Supply Chain Data Exploration & Data Cleaning Pipeline

## Project Overview

Disruptions resulting from global events create unprecedented challenges in managing supply chain demand and inventory across multi-regional networks. For technology companies operating across large geographic regions, unpredictable logistics lead times severely impact customer satisfaction, delivery commitments, and inventory holding costs.

This project addresses the operational challenges faced by a major technology enterprise (**ABC Company**) operating across North America and Latin America that relies heavily on global Original Design Manufacturers (ODMs) in Asia and Europe. The primary objective is to develop a complete data exploration, cleaning, and preprocessing pipeline in **R** (`main.R`) to prepare raw operational supply chain datasets for advanced analytics and modeling.

Specifically, the pipeline standardizes, cleans, and analyzes the relationships between different manufacturing origin sites, shipment transport modes, lines of business (LOB), and overall logistics lead times to help mitigate supply chain risk and establish accurate customer delivery expectations.

---

## Operational Model & Lead-Time Metrics

The company operates on a complex logistics model involving multiple ODMs, shipping modes, and regional distribution warehouses. This analysis focuses on incoming supply movements from **four primary manufacturing sites (Sites A, B, C, and D)**, which collectively account for 90% of total shipment volume.

### Supply Chain Lifecycle Flow

```
+---------------------+      +--------------------+      +--------------------+      +---------------------+
| Purchase Order (PO) |      |   ODM/Site Starts  |      |  Product Shipped   |      |  Product Received   |
| Placed & Downloaded | ---> |  Building Product  | ---> |   from ODM Site    | ---> |    at Warehouse     |
| (PO Download Date)  |      |                    |      |    (Ship Date)     |      |   (Receipt Date)    |
+---------------------+      +--------------------+      +--------------------+      +---------------------+
|<------------------ Manufacturing Lead Time ----------->|<---------------- In-Transit Lead Time ------------>|
|<------------------------------------------ Logistics Lead Time ------------------------------------------>|
```

### Key Metrics Defined

* **In-Transit Lead Time**: Warehouse Receipt Date - ODM Ship Date
* **Manufacturing Lead Time**: ODM Ship Date - PO Download Date
* **Logistics Lead Time**: Warehouse Receipt Date - PO Download Date

### Transport Modes
Logistics carriers utilize four distinct transport modes to move goods from sites to regional warehouses:
1. **AIR**
2. **FAST BOAT**
3. **GROUND**
4. **OCEAN**

---

## Technical Pipeline Implementation (`main.R`)

The master processing script `main.R` executes the sequential analytical workflow across three primary phases:

### Phase 1: Data Ingestion & Chronological Date Correction
* **Dynamic Excel Ingestion**: Reads `Raw-Data` and `Calendar` worksheets directly from `raw_data.xlsx` using `readxl::read_excel()`.
* **Chronological Sorting Function (`sort_dates_chronologically`)**: Applies a custom user-defined function to address system-level date swapping errors. By performing row-wise string splitting, sorting, and recombining, it guarantees logical sequence ordering: $\text{PO Download Date} \le \text{Ship Date} \le \text{Receipt Date}$.
* **Initial Lead Time Calculation**: Derives initial numeric lead times (`In-transit Lead Time` and `Manufacturing Lead Time`) using `difftime(..., units = "days")`.

### Phase 2: Fiscal Mapping, Group Imputation & Patching
* **Dynamic Fiscal Period Joining**: Uses `dplyr::left_join` with `between()` to map each record's `Receipt Date` to its corresponding `Quarter` and `Year` range defined in the fiscal calendar sheet.
* **Group Median Imputation**: Calculates robust group medians grouped by `(LOB, Origin, Ship Mode)` combination. Missing lead-time values are replaced via `coalesce()` with these group-level median lookups.
* **Date Re-Derivation & Metadata Patching**: missing `Ship Date` and `Receipt Date` values are re-derived using imputed lead times. `rows_patch()` updates missing fiscal quarter and year metadata for imputed records, followed by filtering unmapped rows using `drop_na(Quarter, Year)`.

### Phase 3: Exploratory Data Analysis & Asset Generation
The script computes central tendencies, volatility metrics, outlier counts, and trend visualizations using `ggplot2`:
* **Central Tendency Analysis**: Computes mean/median manufacturing and transit lead times per ODM site (`median_manufacturing_times_by_site.png`, `median_transit_times_by_site.png`).
* **Interquartile Range (IQR) Risk Analysis**: Evaluates variability in lead times across sites and quarterly timepoints (`quarterly_manufacturing_IQR.png`, `quarterly_transit_IQR.png`).
* **Outlier Identification ($1.5 \times \text{IQR}$ Rule)**: Quantifies site risk by identifying extreme lead-time events beyond upper/lower bounds ($Q_1/Q_3 \pm 1.5 \cdot \text{IQR}$) (`transit_outliers_by_site.png`, `manuf_outliers_by_site.png`).
* **Temporal Trend Tracking**: Plots quarterly shifts in site performance (`quarterly_manufacturing_times_by_origin.png`, `quarterly_transit_times_by_origin.png`) and order volume (`quarterly_orders_delivered.png`).
* **Categorical Performance Evaluation**: Computes density distributions across transport modes (`transit_shipmode_lead_times_density_graph.png`) and manufacturing histograms by LOB (`manufacturing_LOB_graph.png`).
* **One-Hot Encoding & Correlation Matrix**: Uses `fastDummies::dummy_cols()` to encode `LOB`, `Origin`, `Ship Mode`, and `Quarter` into binary dummy columns. Constructs a Pearson correlation matrix (`cor()`) and prints sorted correlation coefficients relative to `In-transit Lead Time`.

---

## Directory Structure

```.
├── main.R                   # Master execution script (Ingestion, Cleaning, EDA, Plots)
├── raw_data.xlsx            # Source Excel data containing 'Raw-Data' & 'Calendar' sheets
└── Final_Project_Report.pdf # Final technical report detailing findings and methodologies
```

---

## Output Visualizations Generated (`main.R`)

Running `main.R` automatically exports the following high-resolution PNG plots:

* `median_manufacturing_times_by_site.png` — Median manufacturing lead time by ODM site.
* `median_transit_times_by_site.png` — Median transit lead time by ODM site.
* `transit_outliers_by_site.png` — Proportion of transit lead-time outliers ($1.5 \times \text{IQR}$) by site.
* `manuf_outliers_by_site.png` — Proportion of manufacturing lead-time outliers ($1.5 \times \text{IQR}$) by site.
* `quarterly_manufacturing_IQR.png` — Overall manufacturing lead-time variance across fiscal quarters.
* `quarterly_transit_IQR.png` — Overall transit lead-time variance across fiscal quarters.
* `quarterly_manufacturing_times_by_origin.png` — Site-by-site median manufacturing lead-time quarterly trends.
* `quarterly_transit_times_by_origin.png` — Site-by-site median transit lead-time quarterly trends.
* `quarterly_orders_delivered.png` — Total order delivery volume by fiscal quarter.
* `transit_shipmode_lead_times_density_graph.png` — Lead-time density distributions across transport modes.
* `manufacturing_LOB_graph.png` — Manufacturing lead-time histogram distributions by Line of Business.

---

## Setup & Running the Code

### Prerequisites
Ensure you have **R** installed along with the required R packages:

```R
install.packages(c("readxl", "dplyr", "tidyverse", "ggplot2", "lubridate", "fastDummies", "ggcorrplot"))
```

### Running the Analysis
1. Place `main.R` and `raw_data.xlsx` in the same working directory.
2. Open your terminal or R console and run:

```R
source("main.R")
```