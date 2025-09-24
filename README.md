# NetCDF Zonal Statistics Calculator (Shiny App)

This Shiny web application allows users to **aggregate NetCDF gridded data over hydrological zones** using an HRU (Hydrological Response Unit) shapefile or predefined grid-cell weights. It is designed to compute zonal statistics (e.g., average values) across time and space and output them as a CSV file, along with rich visualization.

---

## 📦 Features

* Upload NetCDF file (`.nc`) with time-varying gridded data.
* Upload optional grid weights file (`.txt`) for custom aggregation.
* Upload HRU shapefile (`.zip`) to dynamically compute weights and aggregate values.
* Automatically detects:

  * Spatial-only variables (e.g., `lat`, `lon`)
  * Dimension names
  * HRU ID field from shapefile
* Provides interactive UI for selecting variables, dimensions, and HRU fields.
* Computes:

  * Time series statistics (95% uncertainty interval + median)
  * Static spatial summaries (e.g., elevation, land cover)
* Results visualized:

  * Interactive Plotly plots for time series
  * Interactive data tables for static variables
* Download aggregated results as a `.csv` file.
* Responsive log panel to track processing steps.
* User-friendly with loading indicators and progress bars throughout.

---

## 🖥️ How to Run Locally

### 🔹 Simple Method (No Git Required)

1. Visit:

   👉 [https://github.com/rarabzad/NetCDF-Zonal-Statistics-App](https://github.com/rarabzad/NetCDF-Zonal-Statistics-App)

2. Click the green **`Code`** button and choose **`Download ZIP`**.

3. Extract the ZIP and open the folder containing `app.R`.

4. Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) if you haven’t already.

5. In RStudio:

   * Open `app.R`
   * Click **Run App** in the top-right

   Or from the console:

   ```r
   shiny::runApp("path/to/your/app")
   ```

> 💡 Missing packages will need to be installed manually using `install.packages()` if not already installed.

---

### 🔹 Git Method

```bash
git clone https://github.com/rarabzad/NetCDF-Zonal-Statistics-App.git
cd NetCDF-Zonal-Statistics-App
```

Then open `app.R` in RStudio and click **Run App** or run:

```r
shiny::runApp()
```

---

## 📁 Required Inputs

### 1. NetCDF File (`.nc`)

* Should contain time-varying variables across a spatial grid.
* The app automatically detects variables with spatial dimensions only (no time dimension).

You will be prompted to select:

* **Variables** (e.g., `tmin`, `tmax`, `precip`)
* **Grid Dimensions** (e.g., `rlat`, `rlon`)

### 2. Optional Grid Weights File (`.txt`)

* A precomputed text file containing weights for each HRU–grid cell pair.
* Skipped if HRU shapefile is provided instead.

### 3. HRU Shapefile (`.zip`)

* Upload a `.zip` archive containing:

  * `.shp`, `.dbf`, `.shx`, and `.prj` files
* The shapefile must include polygon features representing HRUs.
* The app will ask you to select an HRU ID field (e.g., `ZoneID` or `SubId`).

> 📝 Files inside the ZIP must not be inside nested folders.

---

## 📤 Output

### Downloaded CSV

| Column Name | Description                                 |
| ----------- | ------------------------------------------- |
| Time        | Timestamps parsed from NetCDF               |
| Variable(s) | Aggregated values (median, quantiles, etc.) |
| HRU Zones   | Aggregated by HRU shapefile or weight file  |

---

## 📊 Visual Output

* **Interactive Time Series**
  For each variable:

  * Solid blue line = **Median** across HRUs
  * Shaded gray ribbon = **2.5%–97.5% quantile range** (95% uncertainty interval)

* **Static Variables Table**
  HRU-level values shown in an interactive sortable table.

* **Log Output**
  Tracks progress such as:

  * File uploads
  * Variable selections
  * Processing steps
  * Errors or warnings

---

## 🎛️ Interface Overview

| Component            | Description                                             |
| -------------------- | ------------------------------------------------------- |
| NetCDF Upload        | Upload `.nc` file with gridded data                     |
| Grid Weights Upload  | Upload `.txt` file with precomputed HRU weights         |
| HRU Shapefile Upload | Upload zipped shapefile to generate weights dynamically |
| Select Variables     | Choose spatial-only variables to aggregate              |
| Select Dimensions    | Select spatial dimensions of the NetCDF file            |
| Select HRU Field     | Choose field from shapefile to define HRU IDs           |
| Run Aggregation      | Starts processing and generates results                 |
| Download Aggregated  | Download `.csv` result file with all statistics         |
| Log Output           | Real-time updates and status                            |

---

## ⏳ Performance Notes

* Max upload size: **500 MB**
* Large files may take **2–10 minutes** to process depending on:

  * File size
  * Number of HRUs
  * Complexity of NetCDF variables

> 🛑 Make sure coordinate systems between NetCDF and shapefile align (e.g., both in WGS84)

---

## 🧠 How It Works Internally

1. **File Uploads & UI Prep**

   * Parses NetCDF and shapefile inputs
   * Detects spatial variables
   * Initializes UI for selections

2. **Aggregation Logic**

   * Calls the external function `rdrs_spatial_aggregator()`:

     * Computes spatial weights (if HRU shapefile is used)
     * Aggregates NetCDF values by HRU
   * Applies quantile statistics

3. **Visualization**

   * Uses `ggplot2` + `plotly` for time series
   * Uses `DT::datatable` for static tables

4. **Download**

   * Combines results into one `.csv`
   * Wrapped in a `withProgress()` dialog for responsiveness

---

## 🙋 Need Help?

For issues, suggestions, or improvements:

* [Open an Issue](https://github.com/rarabzad/NetCDFZonalStatistics)
* [Live demo](https://raven-netcdfzonalstatistics.share.connect.posit.cloud)
