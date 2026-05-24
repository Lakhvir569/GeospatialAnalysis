# Detroit Crime & Grocery Access — Geospatial Analysis

**Type:** Solo Project

## Overview
Used R and geospatial libraries to map 2021 crime incidents and grocery store locations across Detroit neighborhoods, analyzing how robbery rates change within 1-, 2-, and 3-mile buffer zones around stores to help inform resource allocation and public safety planning.

## Tasks

### Task 1 — Crime Mapping
- Overlaid neighborhood and zip code shapefiles with 2021 crime incident data to create color-coded risk maps
- Identified high-risk zones (1,200+ incidents) and low-risk zones (<100 incidents) across Detroit neighborhoods
- Found that most neighborhoods fall in a moderate-risk category, pointing to broad-based intervention needs

### Task 2 — Grocery Store Buffer Analysis
- Plotted 1-, 2-, and 3-mile buffer zones around each Detroit grocery store using the `sf` library
- Calculated robbery incident counts within each buffer zone
- Exported descriptive statistics to Excel for further analysis

## Results
| Buffer Zone | Mean Robberies |
|---|---|
| 1 mile | 5.57 |
| 2 miles | 13.49 |
| 3 miles | 27.04 |

A 3-mile radius covers nearly all Detroit neighborhoods with grocery access. A 2-mile radius was identified as the optimal planning threshold for most neighborhoods.

## Tech Stack
`R` `sf` `tmap` `Excel` `Geospatial Analysis` `Shapefiles`

## Files
- `Geospatial Analysis Report.pdf` — Full project write-up with maps and analysis
- `GeospatialAnalysis.R` — Full R source code
- `GroceryRobberyStatsPerMile.xlsx` — Robbery statistics by buffer zone exported to Excel
