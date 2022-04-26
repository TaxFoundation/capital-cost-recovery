# Capital Cost Recovery across the OECD

The Tax Foundation’s publication [Capital Cost Recovery across the OECD](https://taxfoundation.org/publications/capital-cost-recovery-across-the-oecd/) shows how capital allowances compare across OECD countries and how they have developed since 2000. The dataset we compiled for the years 1979 to 2021 is made available as a resource for research.

## Latest Release
The current version of this project is 1.2.0 (2022-04-26).

## Installation Directions
You can analyze the data on your personal computer by using R.

### Cloning the GitHub repository
To install capital-cost-recovery, run the following in the folder you wish to store the files:

```git clone https://github.com/TaxFoundation/capital-cost-recovery```

```cd capital-cost-recovery```

More details on cloning a repository can be found [here](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository).

### Installing R
If you haven't already done so, download and install [RStudio](https://www.rstudio.com/products/rstudio/).

All necessary functions to run capital-cost-recovery can be called from capital-cost-recovery/capital_cost_recovery.R. See below for an explanation of the source data and the folders and files in the repository.


## The Dataset

### Scope
The source dataset compiled for this publication includes the 2020 depreciation schedules of all 38 OECD countries for the following three assets: industrial buildings, machinery, and intangibles.

In addition, the dataset includes historic depreciation schedules for the time period 1979 to 2020. However, years prior to 2000 do not include data for all OECD countries (e.g., the year 1979 only includes data for Canada, Finland, and Germany). The dataset also includes some historic depreciation schedules data for non-OECD countries.

It also includes the best available inventory valuation methods in OECD countries up until 2021, statutory corporate tax rates, effective average tax rates (EATRs), and effective marginal tax rates (EMTRs) for certain years and countries. However, these variables are not used in this report.


### Depreciation Methods

There are various depreciation methods. The dataset captures them as follows:

- **SL - Straight-Line Method:** An asset is depreciated by an equal allowance (in nominal terms) each year; e.g., an annual allowance of 10% over 10 years.

* **DB - Declining-Balance Method:** An asset is depreciated based on its _remaining_ book value; e.g., in the case of a $100 investment, in the first year the allowance is 25% of $100, in the second year it's 25% of ($100-$25), and so forth.

* **DB or SL - Declining-Balance Method with a Switch to the Straight-Line Method:** An asset is depreciated with the declining-balance method for a (number of) year(s) and then the depreciation method switches to straight-line. In some countries businesses switch from DB to SL when depreciating an asset as soon as the allowance is higher under SL than it would be under DB.

* **SL2 - Straight-Line Method with Changing Rates:** An asset is depreciated under the straight-line method. However, after a (number of) year(s) the depreciation rate changes; e.g., an asset is depreciated at 20% for three years and then at 10% for four years. In our dataset, countries that switch their depreciation rates twice are treated as if they switched them only once (see Italy).

* **initialDB - Declining-Balance Method with an Initial Allowance:** An asset is depreciated under the declining-balance method but there is an additional allowance in the first year.


We assume that businesses will choose the tax-optimizing depreciation method (higher allowances in earlier years) and list that method in the dataset while ignoring less optimal alternatives businesses might have.

The document *Oxford_CBT_Tax_Database_Data_Description.md* provides more details on how this dataset is structured and what the variables are.



## Explanation of Files in Repository

### /main directory

Location of the R code, the source documentation, a detailed description of the dataset, and this README.

The R code reads in and downloads the necessary data, cleans the data, corrects data, merges datasets, and produces intermediate and final output datasets and tables.

The source documentation cites all the sources used.

The description of the dataset explains the variables it contains and gives some background information.


### /source-data

Location of **input** files to .R code file including:

- `cost_recovery_data.csv` Dataset that includes variables that reflect the depreciation schedules of OECD countries and some non-OECD countries for the years 2000-2020 (and for some countries for the years 1979-1999 as well; the new OECD country Colombia is missing for the years 2000-2012 due to data unavailability).

- `country_codes.csv` Dataset that includes all 249 sovereign states and dependent territories that have been assigned a country code by the International Organization for Standardization (ISO). Includes official country names in various languages, ISO country codes, continents, and further geographical information.

- `gdp_historical.xlsx` U.S. Department of Agriculture's dataset of historical real Gross Domestic Product (GDP) and growth rates of GDP for 176 countries and various regions (in billions of 2015 dollars) for the years 1999 to 2019.

- `gdp_projected.xlsx` U.S. Department of Agriculture's dataset of projected real Gross Domestic Product (GDP) and growth rates of GDP for 176 countries and various regions (in billions of 2015 dollars) for the years 2012 to 2032.


### /final-data
Location of **final data** files of .R code file including

- `npv_all_years.csv` Dataset that includes the net present value calculations of capital allowances for each of the three asset types by country (all OECD and EU countries) and by year (for all years for which we have data). It also includes the average of the three asset types (weighted by an asset's estimated share in the economy), the average weighted by GDP, and GDP data.


### /final-outputs
Location of **output tables** that are included in the publication or needed to produce the charts that are part of the publication.

- `asset_averages.csv` Table showing the average net present value of capital allowances in the OECD by asset type for the year 2021.

- `cit_rates_timeseries.csv` Table showing the average and weighted average (weighted by GDP) combined corporate income tax rate in the OECD for each year from 2000 to 2021.

- `eu_cctb.csv` Table showing the average net present value of capital allowances in EU countries for the year 2021. Also includes the average net present value of capital allowances under the EU's Common Corporate Tax Base (CCTB) proposal.

- `npv_europe.csv` Table showing the average net present value of capital allowances in European countries for the year 2021. Also includes a column that ranks the countries by the net present value of their capital allowances.

- `npv_ranks_2021.csv` Table showing the net present value of capital allowances by OECD country and asset type for the year 2021. It also includes the average of the net present values of capital allowances weighted by each asset's estimated share in the economy and a ranking by each asset type and by the weighted average.

- `npv_weighted_timeseries.csv` Table showing the average and weighted average (weighted by GDP) net present value of capital allowances in the OECD for each year from 2000 to 2021.


## Contact Information
This repository is maintained by [Daniel Bunn](mailto:dbunn@taxfoundation.org) at the [Tax Foundation](https://taxfoundation.org).


## Suggested Citation
Please cite the source of your analysis as "Tax Foundation, "Capital Cost Recovery across the OECD," author's calculations." If you would like to link to the GitHub repository, please use https://github.com/PSLmodels/capital-cost-recovery/tree/master. If you would like to link to the associated Tax Foundation publication, please use https://taxfoundation.org/publications/capital-cost-recovery-across-the-oecd/. 
