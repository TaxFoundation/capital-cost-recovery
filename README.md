# Capital Cost Recovery across the OECD, 2020

[UPDATE]

-> double check whether the calculations for Canada are right! They have "initial DB" and its not quite clear to me what should go in rate1 and rate2 - either 2017 and before or 2018 and 2019 need to be changed! (compare with Finland)

Regarding underlying depreciation schedules: Kind of allowance represents the most tax-efficient possibility; other possibilities are ignored.

Mexico adjusts its capital allowances for inflation!

## Corrections Made to Previous Years

Corrections are generally made going back to the year 2010, unless otherwise noted.

* **Austria**
    * Extended the depreciation length for buildings from 33 years to 40 years (from 3% to 2.5% annual depreciation), see WKO, "Die Abschreibung von Betriebsgebäuden," https://www.wko.at/service/steuern/Die-Abschreibung-von-Betriebsgebaeuden.html. This was corrected for the years 2016, 2017, and 2018.

* **Belgium** 
    * Depreciates machinery as follows: DB at 28.57 percent for 3 years, then SL at 14.29 percent for 2 years, and finally the remaining 7.86 percent in the last year. Combining the SL depreciation years, it's around 2.6 years. The dataset previously listed it at 2 years, this was changed to 2.6 years for the years 1998 to 2019.
    * There is an additional investment deduction for intangibles that was previously not taken into account. 

* **Bulgaria**
    * Switched its best inventory valuation method from LIFO to Weighted Average in 2011. This switch was previously not reflected in the dataset (now corrected).

* **Croatia**
    * Switched its depreciation schedule for industrial buildings to SL 10 percent (for 10 years) in 2005. This was corrected. Similar corrections were made to the depreciation schedules for machinery and intangibles.

* **Canada**
    * !!Cannot find any evidence for buildings being depreciated at 2 percent starting in the second year. Double check again. Is it because in Canada, only half of the annual depreciation is considered in the first year (half-year rule)?

* **France**
    * Depreciates machinery as follows: DB at 32.14 percent for 4 years, then SL at 7.07 percent for 3 years. The dataset previously listed it at 28.57 percent for 5 years and then 6.3 percent at 3 years. This was corrected for the years for the years 2001 to 2019.
    Under the declining-balance, straight-line rates are multiplied by a factor depending on the useful life of an asset. 

* **Greece**
    *  Greece switched from LIFO to Average in 2015, which was previously not captured in the dataset. This was corrected for the years starting in 2015.

* **Hungary**
    * Generally, there is no prescribed amortization rate for intangibles in Hungary; the historical value, the residual value, and the useful life should be considered, see PwC, "Worldwide Tax Summaries - Corporate-Deductions," last reviewed Jan. 15, 2020, https://taxsummaries.pwc.com/ID/Hungary-Corporate-Deductions. The dataset previously listed it at 5 year (20%) straight-line depreciation. However, as explained above, if there is no defined amortization rate, we assume a rate of 10 percent (10%). This was corrected.

* **Ireland**
    * Ireland allows intangible assets to be written off over a fixed write-down period of 15 years (7%). The dataset previously listed it at 20 year (5%) straight-line depreciation. This was corrected.

* **Japan**
    * Japan's inventory valuation method previously listed in the dataset for the years prior to 2018 was LIFO. However, Japan switched to "Weighted-Average" as the best available method in 2009. This was corrected for the years 2009 to 2017.

* **Lithuania**
    * The dataset previuosly listed Lithuania as "DB or SL" for all asset types while it should've been "DB." This was corrected.

* **Luxembourg**
    * For the asset machinery, the dataset previously listed it as "DB or SL" with 30% DB for 4 years and 10% for 10 years for the years 2017 and 2018. However, it should've been 30% DB for 4 years and *8%* for 3 years. This was corrected.

* **Netherlands**
    * For the asset machinery, the dataset previously listed it as "SL" with 50 percent (2 years) since 2009. However, this 50 percent rate was only in place from 2009 to 2011 and has been changed multiple times. This was corrected for all years starting in 2012.

* **New Zealand**
    * The daset previously incorrectly listed the depreciation method as "DB or SL" for buildings and machinery for the years 2017 and 2018, while it should've been "DB." This was corrected.

* **Slovakia**
    * In 2015, Slovakia changed its depreciation method for buildings from a special accelerated method to SL. This change had previously not been reflected in the dataset but is now corrected. 

* **Turkey**
    * The inventory valuation method was previously listed as "LIFO." This was incorrect as LIFO is not permitted. Now corrected for 2010-2019.
    * It is allowed to change from DB to SL for each asset (all asset types). As we assume that businesses will choose the tax-optimizing method and thus maximize depreciation rates, it is optimal to change from DB to SL as soon as the rate is greater under SL than it is under DB. This was previously not taken into account for buildings but is now corrected for the years back to 2010.

## Depreciation Methods
* **SL**: straight line
* **DB**: declining balance
* **DB or SL**: declining balance with a switch to straight line
* **SL2**: straight line depreciation with changing rates (SL3 is treated like SL2 - see Italy)
* **initialDB**: Declining Balance with initial allowance

^ Add examples for each method!! 

## Things that should be changed (note to myself)
In the variable description, I think rows like the following should be adjusted (it should be "SL or DB" and not just "SL"):
taxdeprbuildsl	Numeric, not in %	Rate of depreciation applicable if depreciation according to straight line (SL), if SL2, the second applicable rate


The Tax Foundation’s publication [Corporate Tax Rates around the World](https://taxfoundation.org/publications/corporate-tax-rates-around-the-world/) shows how statutory corporate income tax rates have developed since 1980, with data for over 200 jurisdictions for the year 2019. The dataset we compiled for the years 1980 to 2019 is made available as a resource for research.

## The Dataset

### Scope
The dataset compiled for this publication includes the 2019 statutory corporate income tax rates of 218 sovereign states and dependent territories around the world. Tax rates were researched only for jurisdictions that are among the almost 250 sovereign states and dependent territories that have been assigned a country code by the International Organization for Standardization (ISO). As a result, zones or territories that are independent taxing jurisdictions but do not have their own country code are not included in the dataset.

In addition, the dataset includes historic statutory corporate income tax rates for the time period 1980 to 2018. However, these years cover tax rates of fewer than 218 jurisdictions due to missing data points.

To be able to calculate average statutory corporate income tax rates weighted by GDP, the dataset includes GDP data for 176 jurisdictions. When used to calculate average statutory corporate income tax rates, either weighted by GDP or unweighted, only these 176 jurisdictions are included (to ensure the comparability of the unweighted and weighted averages).

### Definition of Selected Corporate Income Tax Rate
The dataset captures standard top statutory corporate income tax rates levied on domestic businesses. This means:
- The dataset does not reflect special tax regimes, including but not limited to patent boxes, offshore regimes, or special rates for specific industries. 
- A number of countries levy lower rates for businesses below a certain revenue threshold. The dataset does not capture these lower rates.
- A few countries levy gross revenue taxes on businesses instead of corporate income taxes. Since the tax rates of a corporate income tax and a gross revenue tax are not comparable, these countries are excluded from the dataset.

## Explanation of Files in Repository

### /main directory

Location of the R code, the source documentation, and this README.

The R code reads in and downloads the necessary data, cleans the data, adds missing data manually, merges datasets, and produces intermediate and final output datasets and tables.

The source documentation cites all the sources used.

### /source-data

Location of **input** files to .R code file including:

- `country_codes.csv` Dataset that includes all 249 sovereign states and dependent territories that have been assigned a country code by the International Organization for Standardization (ISO). Includes official country names in various languages, ISO country codes, continents, and further geographical information.

- `data_rates_1980_2018.csv` Tax Foundation's dataset of statutory corporate income tax rates for the years 1980 to 2018. This dataset has been built in stages since 2015.

- `gdp_historical.xlsx` U.S. Department of Agriculture's dataset of historical real Gross Domestic Product (GDP) and growth rates of GDP for 176 countries and various regions (in billions of 2010 dollars) for the years 1980 to 2017.

- `gdp_projected.xlsx` U.S. Department of Agriculture's dataset of projected real Gross Domestic Product (GDP) and growth rates of GDP for 176 countries and various regions (in billions of 2010 dollars) for the years 2010 to 2030.

- `kpmg_dataset.xlsx` KPMG's dataset of statutory corporate income tax rates for 171 jurisdictions for the years 2003 to 2019.

### /intermediate-outputs

Location of **intermediate output** files of .R code file including:

- `gdp_iso.csv` GDP data paired with ISO country codes for the years 1980 to 2019.

- `rates_final.csv` Statutory corporate income tax rates for the years 1980 to 2019. Includes rates of all countries for which data was available in 2019 (data from OECD, KPMG, and researched individually).

- `rates_preliminary.csv` Statutory corporate income tax rates for the years 1980 to 2019. Includes rates of countries for which OECD and KPMG data was available for the year 2019. Does not include countries for which the rate was researched and added individually.

### /final-data
Location of **final output** files of .R code file including

- `final_data_long.csv` Statutory corporate income tax rates and GDP levels of all countries paired with ISO country codes, continents, and country groups for the years 1980 to 2019. Includes all countries that have an ISO country code, including the ones for which corporate income tax rates and/or GDP data was not available. In long format.

- `final_data_2019.csv` Statutory corporate income tax rates and GDP levels of countries paired with ISO country codes, continents, and country groups for the year 2019. Only includes countries for which both the corporate income tax rates and GDP data were available.

- `final_data_2019_gdp_incomplete.csv` Statutory corporate income tax rates and GDP levels of countries paired with ISO country codes, continents, and country groups for the year 2019. Includes all countries for which we have data for the corporate income tax rate, including countries for which we do not have GDP data.

### /final-outputs
Location of **output tables** that are included in the publication.

- `bottom_rates.csv` Table of the 21 countries with the lowest corporate income tax rates in the world in 2019 (excluding countries without a corporate tax).

- `distribution_1980.csv` Table showing the distribution of corporate income tax rates in 1980.

- `distribution_1990.csv` Table showing the distribution of corporate income tax rates in 1990.

- `distribution_2000.csv` Table showing the distribution of corporate income tax rates in 2000.

- `distribution_2010.csv` Table showing the distribution of corporate income tax rates in 2010.

- `distribution_2019.csv` Table showing the distribution of corporate income tax rates in 2019.

- `rate_changes.csv` Table showing by how much the corporate income tax rates changed between 2000 and 2019 by country.

- `rate_time_series.csv` Table showing the weighted and unweighted worldwide average of corporate income tax rates by year between 1980 and 2019.

- `rates_regional.csv` Table showing the weighted and unweighted averages of corporate income tax rates by continent and country groups for the year 2019.

- `regional_all_data.csv` Table showing the weighted and unweighted averages of corporate income tax rates by continent and country groups for the years 1980, 1990, 2000, 2010, and 2019.

- `top_rates.csv` Table of the 21 countries with the highest corporate income tax rates in the world in 2019.

- `zero_rates.csv` Table of countries without a corporate income tax in 2019.