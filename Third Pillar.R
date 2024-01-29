###################################################################################################################################
###################################################################################################################################
### GETTING STARTED ###
###################################################################################################################################
###################################################################################################################################

# Open Read Me "Third Pillar_Read Me.docx"

# Clear Global Environment
rm(list= ls())

# Install Packages (remove # if need to install packages) 
#install.packages("readxl")
#install.packages("xlsx")
#install.packages("writexl")
#install.packages("haven")
#install.packages("wbstats")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("r2weight")
#install.packages("devtools")
#install_github("swager/randomForestCI")
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("rnaturalearth")

# Load Libraries
library(readxl)
library(xlsx)
library(writexl)
library(haven)
library(wbstats)
library(dplyr)
library(tidyr)
library(lmtest)
library(robustbase)
library(ranger)
library(vtable)
library(SuperLearner)
library("r2weight")
library(devtools) 
library(randomForestCI)
library(randomForest) 
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(grid)
library(sf)
library(rnaturalearth)
library(tibble)
library(gridExtra)

# Set working directory to folder where you have "Input" and "Output" folders
setwd("C:/Users/KKAY/Rode Kruis-Vlaanderen/Fritz Schiltz - Third sector/Paper 2 - Measuring the plural sector/WBER/DATA/Analysis/Third Pillar")

###################################################################################################################################
###################################################################################################################################
### IMPORT AND CLEAN DATA ###
###################################################################################################################################
###################################################################################################################################

###################################################################################################################################
### POLITY DATA ###
###################################################################################################################################

# Import Polity data
# Source: https://www.systemicpeace.org/inscrdata.html - Polity5 Annual Time-Series, 1946-2018
# This link downloads the excel file: https://www.systemicpeace.org/inscr/p5v2018.xls (Excel file = "pv2018.xls")
polity5 <- read_excel("input\\p5v2018.xls", col_names = TRUE)

# Impute United States data from 2016 to 2020 (the continued to have a durable government 
  # (i.e., there was no regime change or transition period))
polity5$durable[16802] <- 151
polity5$durable[16803] <- 152
polity5$durable[16804] <- 153
polity5$durable[16805] <- 154
polity5$durable[16806] <- 155

# Select Year 2018 only
polity5$year <- as.numeric(polity5$year)
polity5 <- polity5[polity5$year=="2018", ]
polity5$Country <- polity5$country
polity5$countrycode <- polity5$scode

###################################################################################################################################
# Additional analysis for Annex (using last five years instead of only 2018) (remove # to replicate Table A2)
###################################################################################################################################

# polity5$year <- as.numeric(polity5$year)
# polity5 <- polity5[polity5$year %in% c("2014", "2015", "2016", "2017", "2018"), ]
# polity5$Country <- polity5$country
# polity5$countrycode <- polity5$scode
# polity5 <- polity5 %>%
  # group_by(Country, countrycode) %>%
  # summarize(
    # polity = mean(polity, na.rm = TRUE),
    # polity2 = mean(polity2, na.rm = TRUE),
    # democ = mean(democ, na.rm = TRUE),
    # fragment = mean(fragment, na.rm = TRUE),
    # durable = mean(durable, na.rm = TRUE),
    # polcomp = mean(polcomp, na.rm = TRUE))


###################################################################################################################################
# Continue Polity Data Analysis
###################################################################################################################################

# Select country (country name), country code, fragment (polity fragmentation),democ (institutionalized democracy), 
  #polity (combined polity score), polity 2 (reversed combined polity score), durable (regime durability), polcomp (political competition) 
# See detailed variable explanations here: https://www.systemicpeace.org/inscr/p5manualv2018.pdf
polity5 <- polity5[, c("Country", "countrycode", "polity", "polity2","democ", "fragment", "durable", "polcomp")]

###################################################################################################################################
### WORLD DEVELOPMENT INDICATORS ###
###################################################################################################################################

# Databases can be pulled automatically from the World Bank DataBank to get the latest data (use codes): https://databank.worldbank.org/metadataglossary/all/series

# The following steps are for the database pulled automatically from the World Bank DataBank

# Specify the indicators and countries of interest
#indicators <- c("NY.GDP.MKTP.CD", "NY.GNS.ICTR.ZS", "NY.GNP.MKTP.KD", "NE.CON.GOVT.KD", "NY.GDP.PCAP.KD")
#countries <- "all"

# Retrieve data from the World Bank
#gdp_data <- wb_data(indicator = indicators[1], country = countries)
#gross_saving_data <- wb_data(indicator = indicators[2], country = countries)
#gni_data <- wb_data(indicator = indicators[3], country = countries)
#gov_expenditure_data <- wb_data(indicator = indicators[4], country = countries)
#gdp_per_capita_data <- wb_data(indicator = indicators[5], country = countries)

# Select relevant variables
#gdp_data <- gdp_data[, c("iso3c", "country", "date", "NY.GDP.MKTP.CD")]
#gross_saving_data <- gross_saving_data[, c("iso3c", "country","date", "NY.GNS.ICTR.ZS")]
#gni_data <- gni_data[, c("iso3c","country", "date", "NY.GNP.MKTP.KD")]
#gov_expenditure_data <- gov_expenditure_data[, c("iso3c","country", "date", "NE.CON.GOVT.KD")]
#gdp_per_capita_data <- gdp_per_capita_data[, c("iso3c","country", "date", "NY.GDP.PCAP.KD")]

# Rename columns
#colnames(gdp_data) <- c("countrycode", "Country", "year", "gdp")
#colnames(gross_saving_data) <- c("countrycode","Country", "year", "grossSaving")
#colnames(gni_data) <- c("countrycode","Country", "year", "gni")
#colnames(gov_expenditure_data) <- c("countrycode","Country", "year", "govExpenditure")
#colnames(gdp_per_capita_data) <- c("countrycode","Country", "year", "gdpPerCapita")

# Merge datasets
# Merge the datasets
#wbdata <- inner_join(gdp_data, gross_saving_data, by = c("countrycode","Country", "year"))
#wbdata <- inner_join(wbdata, gni_data, by = c("countrycode","Country", "year")) 
#wbdata <- inner_join(wbdata, gov_expenditure_data, by = c("countrycode","Country", "year"))
#wbdata <- inner_join(wbdata, gdp_per_capita_data, by = c("countrycode","Country", "year"))

# Select Year 2021 only
#wbdata$year <- as.numeric(wbdata$year)
#wbdata<- wbdata[wbdata$year=="2021", ]

# The following steps are to replicate the findings in the paper using the gdppercapita input file "gdpPerCapita.xlsx"

# Input gdp per capita data
wbdata <- read_excel("input\\gdpPerCapita.xlsx")

# Select data from 2021 only
wbdata<- wbdata[wbdata$year=="2021", ]

###################################################################################################################################
# Additional analysis for Annex (using last five years instead of only 2018) (remove # to replicate Table A2)
###################################################################################################################################

# wbdata <- wbdata[wbdata$year %in% 2017:2021, ]
# wbdata <- wbdata %>%
  # group_by(countrycode) %>%
  # summarize(
    # gdpPerCapita = mean(gdpPerCapita, na.rm = TRUE))

###################################################################################################################################
# Continue World Bank Inidiators Analysis
###################################################################################################################################

# Add Country column with matching country names for each countrycode

# Define a list of country code and country name pairs
country_pairs <- list(
  "ABW" = "Aruba",
  "AFE" = "Africa East",
  "AFG" = "Afghanistan",
  "AFW" = "Africa West",
  "AGO" = "Angola",
  "ALB" = "Albania",
  "AND" = "Andorra",
  "ARB" = "Arab World",
  "ARE" = "United Arab Emirates",
  "ARG" = "Argentina",
  "ARM" = "Armenia",
  "ASM" = "American Samoa",
  "ATG" = "Antigua and Barbuda",
  "AUS" = "Australia",
  "AUT" = "Austria",
  "AZE" = "Azerbaijan",
  "BDI" = "Burundi",
  "BEL" = "Belgium",
  "BEN" = "Benin",
  "BFA" = "Burkina Faso",
  "BGD" = "Bangladesh",
  "BGR" = "Bulgaria",
  "BHR" = "Bahrain",
  "BHS" = "The Bahamas",
  "BIH" = "Bosnia and Herzegovina",
  "BLR" = "Belarus",
  "BLZ" = "Belize",
  "BMU" = "Bermuda",
  "BOL" = "Bolivia",
  "BRA" = "Brazil",
  "BRB" = "Barbados",
  "BRN" = "Brunei Darussalam",
  "BTN" = "Bhutan",
  "BWA" = "Botswana",
  "CAF" = "Central African Republic",
  "CAN" = "Canada",
  "CEB" = "Central Europe and the Baltics",
  "CHE" = "Switzerland",
  "CHI" = "Channel Islands",
  "CHL" = "Chile",
  "CHN" = "China",
  "CIV" = "Cote d'Ivoire",
  "CMR" = "Cameroon",
  "COD" = "Congo, Dem. Rep.",
  "COG" = "Congo, Rep.",
  "COL" = "Colombia",
  "COM" = "Comoros",
  "CPV" = "Cabo Verde",
  "CRI" = "Costa Rica",
  "CSS" = "Caribbean small states",
  "CUB" = "Cuba",
  "CUW" = "Curacao",
  "CYM" = "Cayman Islands",
  "CYP" = "Cyprus",
  "CZE" = "Czech Republic",
  "DEU" = "Germany",
  "DJI" = "Djibouti",
  "DMA" = "Dominica",
  "DNK" = "Denmark",
  "DOM" = "Dominican Republic",
  "DZA" = "Algeria",
  "EAP" = "East Asia & Pacific",
  "EAR" = "Early-demographic dividend",
  "EAS" = "East Asia & Pacific (excluding high income)",
  "ECA" = "Europe & Central Asia",
  "ECS" = "Europe & Central Asia (excluding high income)",
  "ECU" = "Ecuador",
  "EGY" = "Egypt, Arab Rep.",
  "EMU" = "Euro area",
  "ERI" = "Eritrea",
  "ESP" = "Spain",
  "EST" = "Estonia",
  "ETH" = "Ethiopia",
  "EUU" = "European Union",
  "FCS" = "Fragile and conflict affected situations",
  "FIN" = "Finland",
  "FJI" = "Fiji",
  "FRA" = "France",
  "FRO" = "Faroe Islands",
  "FSM" = "Micronesia, Fed. Sts.",
  "GAB" = "Gabon",
  "GBR" = "United Kingdom",
  "GEO" = "Georgia",
  "GHA" = "Ghana",
  "GIB" = "Gibraltar",
  "GIN" = "Guinea",
  "GMB" = "Gambia",
  "GNB" = "Guinea-Bissau",
  "GNQ" = "Equatorial Guinea",
  "GRC" = "Greece",
  "GRD" = "Grenada",
  "GRL" = "Greenland",
  "GTM" = "Guatemala",
  "GUM" = "Guam",
  "GUY" = "Guyana",
  "HIC" = "High income",
  "HKG" = "Hong Kong SAR, China",
  "HND" = "Honduras",
  "HPC" = "Heavily indebted poor countries (HIPC)",
  "HRV" = "Croatia",
  "HTI" = "Haiti",
  "HUN" = "Hungary",
  "IBD" = "IBRD",
  "IBT" = "IBRD only",
  "IDA" = "IDA",
  "IDB" = "IDA & IBRD total",
  "IDN" = "Indonesia",
  "IDX" = "IDA blend",
  "IMN" = "Isle of Man",
  "IND" = "India",
  "INX" = "Not classified",
  "IRL" = "Ireland",
  "IRN" = "Iran, Islamic Rep.",
  "IRQ" = "Iraq",
  "ISL" = "Iceland",
  "ISR" = "Israel",
  "ITA" = "Italy",
  "JAM" = "Jamaica",
  "JOR" = "Jordan",
  "JPN" = "Japan",
  "KAZ" = "Kazakhstan",
  "KEN" = "Kenya",
  "KGZ" = "Kyrgyz Republic",
  "KHM" = "Cambodia",
  "KIR" = "Kiribati",
  "KNA" = "St. Kitts and Nevis",
  "KOR" = "Korea, Rep.",
  "KWT" = "Kuwait",
  "LAC" = "Latin America & the Caribbean (excluding high income)",
  "LAO" = "Lao PDR",
  "LBN" = "Lebanon",
  "LBR" = "Liberia",
  "LBY" = "Libya",
  "LCA" = "St. Lucia",
  "LCN" = "Latin America & Caribbean",
  "LDC" = "Least developed countries: UN classification",
  "LIC" = "Low income",
  "LIE" = "Liechtenstein",
  "LKA" = "Sri Lanka",
  "LMC" = "Lower middle income",
  "LMY" = "Low & middle income",
  "LSO" = "Lesotho",
  "LTE" = "Late-demographic dividend",
  "LTU" = "Lithuania",
  "LUX" = "Luxembourg",
  "LVA" = "Latvia",
  "MAC" = "Macao SAR, China",
  "MAF" = "St. Martin (French part)",
  "MAR" = "Morocco",
  "MCO" = "Monaco",
  "MDA" = "Moldova",
  "MDG" = "Madagascar",
  "MDV" = "Maldives",
  "MEA" = "Middle East & North Africa",
  "MEX" = "Mexico",
  "MHL" = "Marshall Islands",
  "MIC" = "Middle income",
  "MKD" = "North Macedonia",
  "MLI" = "Mali",
  "MLT" = "Malta",
  "MMR" = "Myanmar",
  "MNA" = "Middle East & North Africa (excluding high income)",
  "MNE" = "Montenegro",
  "MNG" = "Mongolia",
  "MNP" = "Northern Mariana Islands",
  "MOZ" = "Mozambique",
  "MRT" = "Mauritania",
  "MUS" = "Mauritius",
  "MWI" = "Malawi",
  "MYS" = "Malaysia",
  "NAC" = "North America",
  "NAM" = "Namibia",
  "NCL" = "New Caledonia",
  "NER" = "Niger",
  "NGA" = "Nigeria",
  "NIC" = "Nicaragua",
  "NLD" = "Netherlands",
  "NOR" = "Norway",
  "NPL" = "Nepal",
  "NRU" = "Nauru",
  "NZL" = "New Zealand",
  "OED" = "OECD members",
  "OMN" = "Oman",
  "OSS" = "Other small states",
  "PAK" = "Pakistan",
  "PAN" = "Panama",
  "PER" = "Peru",
  "PHL" = "Philippines",
  "PLW" = "Palau",
  "PNG" = "Papua New Guinea",
  "POL" = "Poland",
  "PRE" = "Pre-demographic dividend",
  "PRI" = "Puerto Rico",
  "PRK" = "Korea, Dem. People's Rep.",
  "PRT" = "Portugal",
  "PRY" = "Paraguay",
  "PSE" = "West Bank and Gaza",
  "PSS" = "Pacific island small states",
  "PST" = "Post-demographic dividend",
  "PYF" = "French Polynesia",
  "QAT" = "Qatar",
  "ROU" = "Romania",
  "RUS" = "Russian Federation",
  "RWA" = "Rwanda",
  "SAS" = "South Asia",
  "SAU" = "Saudi Arabia",
  "SDN" = "Sudan",
  "SEN" = "Senegal",
  "SGP" = "Singapore",
  "SLB" = "Solomon Islands",
  "SLE" = "Sierra Leone",
  "SLV" = "El Salvador",
  "SMR" = "San Marino",
  "SOM" = "Somalia",
  "SRB" = "Serbia",
  "SSA" = "Sub-Saharan Africa",
  "SSD" = "South Sudan",
  "SSF" = "Sub-Saharan Africa (excluding high income)",
  "SST" = "Small states",
  "STP" = "Sao Tome and Principe",
  "SUR" = "Suriname",
  "SVK" = "Slovak Republic",
  "SVN" = "Slovenia",
  "SWE" = "Sweden",
  "SWZ" = "Eswatini",
  "SXM" = "Sint Maarten (Dutch part)",
  "SYC" = "Seychelles",
  "SYR" = "Syrian Arab Republic",
  "TCA" = "Turks and Caicos Islands",
  "TCD" = "Chad",
  "TEA" = "East Asia & Pacific (IDA & IBRD countries)",
  "TEC" = "Europe & Central Asia (IDA & IBRD countries)",
  "TGO" = "Togo",
  "THA" = "Thailand",
  "TJK" = "Tajikistan",
  "TKM" = "Turkmenistan",
  "TLA" = "Latin America & the Caribbean (IDA & IBRD countries)",
  "TLS" = "Timor-Leste",
  "TMN" = "Middle East & North Africa (IDA & IBRD countries)",
  "TON" = "Tonga",
  "TSA" = "South Asia (IDA & IBRD)",
  "TSS" = "Sub-Saharan Africa (IDA & IBRD countries)",
  "TTO" = "Trinidad and Tobago",
  "TUN" = "Tunisia",
  "TUR" = "Turkey",
  "TUV" = "Tuvalu",
  "TZA" = "Tanzania",
  "UGA" = "Uganda",
  "UKR" = "Ukraine",
  "UMC" = "Upper middle income",
  "URY" = "Uruguay",
  "USA" = "United States",
  "UZB" = "Uzbekistan",
  "VCT" = "St. Vincent and the Grenadines",
  "VEN" = "Venezuela",
  "VGB" = "British Virgin Islands", 
  "VIR" = "U.S. Virgin Islands",
  "VNM" = "Vietnam", 
  "VUT" = "Vanuatu", 
  "WLD" = "World",
  "WSM" = "Samoa",
  "XKX" = "Kosovo",
  "YEM" = "Yemen",
  "ZAF" = "South Africa",
  "ZMB" = "Zambia",
  "ZWE" = "Zimbabwe")

wbdata$Country <- country_pairs[wbdata$countrycode]

wbdata$Country <- as.character(wbdata$Country)

###################################################################################################################################
### INTERNATIONAL LABOUR MARKET DATA ###
###################################################################################################################################

### EMPLOYMENT SECTORS ###

# Import Employment Data 
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "EMP_TEMP_SEX_ECO_NB_A_EN.xlsx")
# Source: ILO STAT Data, "Employment by sex and economic activity (thousands)" 
# ILO Code: EMP_TEMP_SEX_ECO_NB_A_EN
# Link to updated dataset: https://www.ilo.org/ilostat-files/Documents/Excel/Indicator/EMP_TEMP_SEX_ECO_NB_A_EN.xlsx

emp <- read_excel("input\\EMP_TEMP_SEX_ECO_NB_A_EN.xlsx")

# Add correct/shortened column names 
colnames(emp) <- emp[5,]
colnames(emp)[10] <- "Mining and quarrying"
colnames(emp)[11] <- "Trade"
colnames(emp)[12] <- "Public Administration"
colnames(emp)[1] <- "Country"
colnames(emp)[14] <- "Notes"

# Remove first 5 rows
emp <- emp[-(1:5),]

# Make columns numeric
emp[, 4:ncol(emp)] <- lapply(emp[, 4:ncol(emp)], as.numeric)

# Agriculture Data - AG

# Create new dataframe

ag <- emp[c("Country", "Time", "Sex", "Agriculture")]

# Select years (2017-2021) and sex (Total)
ag <- ag %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
ag <- ag %>%
  pivot_wider(names_from = Time, values_from = Agriculture) %>%
  select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
ag$Agriculture_abs <- rowMeans(ag[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Manufacturing Data - MAN

# Create new dataframe

man <- emp[c("Country", "Time", "Sex", "Manufacturing")]

# Select years (2017-2021) and sex (Total)
man <- man %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
man <- man %>%
  pivot_wider(names_from = Time, values_from = Manufacturing) %>%
  select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
man$Manufacturing_abs <- rowMeans(man[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Construction Data - CON

# Create new dataframe

con <- emp[c("Country", "Time", "Sex", "Construction")]

# Select years (2017-2021) and sex (Total)
con <- con %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
con <- con %>%
  pivot_wider(names_from = Time, values_from = Construction) %>%
  select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
con$Construction_abs <- rowMeans(con[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Trade Data - TRADE

# Create new dataframe

trade <- emp[c("Country", "Time", "Sex", "Trade")]

# Select years (2017-2021) and sex (Total)
trade <- trade %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
trade <- trade %>%
  pivot_wider(names_from = Time, values_from = Trade) %>%
  select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
trade$Trade_abs <- rowMeans(trade[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Public Administration Data - PUB

# Create new dataframe

pub <- emp[c("Country", "Time", "Sex", "Public Administration")]

# Select years (2017-2021) and sex (Total)
pub <- pub %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total")

# Transform dataframe so years become the columns 
pub <- pub %>%
  pivot_wider(names_from = Time, values_from = `Public Administration`) %>%
  select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
pub$Public_abs <- rowMeans(pub[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

# Other - OTHER (Mining and Quarrying & Not classified)

# Create new dataframe

other <- emp[c("Country", "Time", "Sex", "Mining and quarrying", "Not classified")]

# Create variable other 
other$Other <- rowSums(other[, c("Mining and quarrying", "Not classified")], na.rm = TRUE)

# Select years (2017-2021) and sex (Total) and keep new Other variable only 
other <- other %>%
  filter(Time %in% c("2017", "2018", "2019", "2020", "2021"), Sex == "Total") %>%
  select(-c("Mining and quarrying", "Not classified"))

# Transform dataframe so years become the columns 
other <- other %>%
  pivot_wider(names_from = Time, values_from = `Other`) %>%
  select("Country", "Sex", "2017", "2018", "2019", "2020", "2021")

# Create new variable "Agriculture_abs"
other$Other_abs <- rowMeans(other[, c("2017","2018", "2019", "2020", "2021")], na.rm = TRUE)

### ALL EMPLOYMENT SECTORS TOGETHER ###

# Add all dataframes together into one
sectors <- full_join(ag, man, by = "Country") %>%
  full_join(con, by = "Country") %>%
  full_join(trade, by = "Country") %>%
  full_join(pub, by = "Country") %>%
  full_join(other, by = "Country")
sectors <- sectors[c("Country", "Agriculture_abs", "Manufacturing_abs", 
                     "Construction_abs", "Trade_abs", "Public_abs", "Other_abs")]

# Create Total variable 
sectors$Total <- rowSums(sectors[, c("Agriculture_abs", "Manufacturing_abs", 
                                     "Construction_abs", "Trade_abs", "Public_abs", "Other_abs")], na.rm = TRUE)

# Create percentages
sectors$AG <- (sectors$Agriculture_abs / sectors$Total) * 100
sectors$MAN <- (sectors$Manufacturing_abs / sectors$Total) * 100
sectors$CON <- (sectors$Construction_abs / sectors$Total) * 100
sectors$TRADE <- (sectors$Trade_abs / sectors$Total) * 100
sectors$PUB <- (sectors$Public_abs / sectors$Total) * 100
sectors$OTHER <- (sectors$Other_abs / sectors$Total) * 100

# Create dataframe to add to full database
sectors_database <- sectors %>%
  select("Country", "AG", "MAN", "CON", "TRADE", "PUB", "OTHER")

###################################################################################################################################
### POPULATION DATA ###
###################################################################################################################################

# Import Population Data
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "population.xlsx")
# Source: ILO STAT Data, "Working-age population by sex and age (thousands) - Annual"
# ILO Code: "POP_XWAP_SEX_AGE_NB_A"
# Link to updated dataset:https://ilostat.ilo.org/topics/population-and-labour-force/ 

pop <- read_excel("input\\population.xlsx")

# Add correct column names
colnames(pop) <- pop[5,]
pop <- pop %>%
  rename(Country = `Reference area`)

# Remove first 5 rows
pop <- pop[-(1:5),]

# Make columns numeric
columns_to_convert_pop <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                        "45-49", "50-54", "55-59", "60-64")
for (column in columns_to_convert_pop) {
  pop[[column]] <- as.numeric(pop[[column]])
}

# Create 15-64 column
pop$'15-64' <- rowSums(pop[,c("15-19", "20-24","25-29","30-34", "35-39", "40-44",
                              "45-49", "50-54","55-59", "60-64")])
# Keep Sex = Total 
SexFilterPop <- "Total"
pop <- subset(pop, Sex == SexFilterPop)

# Keep years 2019, 2020, and 2021
YearFilterPop <- c("2019", "2020", "2021")
pop <- subset(pop, Time %in% YearFilterPop)

# Remove all unnecessary columns 
columns_to_remove_pop <- c("Source", "Sex", "Total", "0-4", "5-9", "10-14", "15-19",
                       "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                       "50-54", "55-59", "60-64", "65+")
pop <- pop %>% select(-one_of(columns_to_remove_pop))

# Transform dataframe so Time becomes the columns
pop$Time <- as.numeric(pop$Time)
pop <- pop %>%
  pivot_wider(names_from = Time, values_from = `15-64`)

# Add correct row names
pop <- as.data.frame(pop) 
rownames(pop) <- pop[, 1]

# Create mean population value across 2019,2020, and 2021
pop$population <- rowMeans(pop[, c("2019", "2020", "2021")], na.rm = TRUE)

# Create final dataframe with only country and mean population 
population <- pop[c("Country", "population")]

###################################################################################################################################
### PARTICIPATION DATA ###
###################################################################################################################################

# Import Participation Data
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "EAP_DWAP_SEX_AGE_RT_A_EN.xlsx")
# Source: ILO STAT Data, "Labour force participation rate by sex and age (%)"
# ILO Code: EAP_DWAP_SEX_AGE_RT_A_EN
# Link to updated dataset: https://ilostat.ilo.org/topics/population-and-labour-force/ 

part <- read_excel("Input\\EAP_DWAP_SEX_AGE_RT_A_EN.xlsx")

# Add correct column names
colnames(part) <- part[5,]
colnames(part)[9] <- "Notes"
part <- part %>%
  rename(Country = `Reference area`)

# Remove first 5 rows
part <- part[-(1:5),]

# Make 15-64 column numeric
part$`15-64` <- as.numeric(part$`15-64`)

# Keep Sex = Total 
SexFilterPart <- "Total"
part <- subset(part, Sex == SexFilterPart)

# Keep years 2017-2021
part$Time <- as.numeric(part$Time)
YearFilterPart <- c("2017", "2018", "2019", "2020", "2021")
part <- subset(part, Time %in% YearFilterPart)

# Remove all unnecessary columns 
columns_to_remove_part <- c("Source", "Sex", "15+", "15-24", "25+", "Notes")
part <- part %>% select(-one_of(columns_to_remove_part))

# Transform dataframe so Time becomes the columns
part <- part %>%
  pivot_wider(names_from = Time, values_from = `15-64`) %>%
  select("Country", "2017", "2018", "2019", "2020", "2021")

# Add correct row names
part <- as.data.frame(part) 
rownames(part) <- part[, 1]

# Create mean population value across 2019,2020, and 2021
part$participation <- rowMeans(part[, c("2017", "2018", "2019", "2020", "2021")], na.rm = TRUE)

# Create final dataframe with only country and mean population 
participation <- part[c("Country", "participation")]

###################################################################################################################################
### UNEMPLOYMENT DATA ###
###################################################################################################################################

# Import Participation Data
# In journal article, data was obtained May 8 2022 from ILO STAT (Excel file = "SDG_0852_SEX_AGE_RT_A_EN.xlsx")
# Source: ILO STAT Data, "Unemployment Rate (%)"
# ILO Code: SDG_0852_SEX_AGE_RT_A
# Link to download updated dataset: https://www.ilo.org/ilostat-files/Documents/Excel/Indicator/SDG_0852_SEX_AGE_RT_A_EN.xlsx

unemp <- read_excel("Input\\SDG_0852_SEX_AGE_RT_A_EN.xlsx")

# Add correct column names
colnames(unemp) <- unemp[5,]
colnames(unemp)[8] <- "Notes"
unemp <- unemp %>%
  rename(Country = `Reference area`)

# Remove first 5 rows
unemp <- unemp[-(1:5),]

# Make 15-64 column numeric
unemp$`15+` <- as.numeric(unemp$`15+`)

# Keep Sex = Total 
SexFilterUnemp <- "Total"
unemp <- subset(unemp, Sex %in% SexFilterUnemp)

# Keep years 2017-2021
unemp$Time <- as.numeric(unemp$Time)
YearFilterUnemp <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", 
                     "2019", "2020", "2021")
unemp <- subset(unemp, Time %in% YearFilterUnemp)

# Remove all unnecessary columns 
columns_to_remove_unemp <- c("Source", "Sex", "15-24", "25+", "Notes")
unemp <- unemp %>% select(-one_of(columns_to_remove_unemp))

# Transform dataframe so Time becomes the columns
unemp <- unemp %>%
  pivot_wider(names_from = Time, values_from = `15+`) %>%
  select("Country", "2012", "2013", "2014", "2015", "2016", "2017", 
         "2018", "2019", "2020", "2021")

# Add correct row names
unemp <- as.data.frame(unemp) 
rownames(unemp) <- unemp[, 1]

# Create mean population value across 2012 - 2021
unemp$unemployment <- rowMeans(unemp[, c("2012", "2013", "2014", "2015", 
                                         "2016", "2017", "2018", "2019", "2020",
                                         "2021")], na.rm = TRUE)

###################################################################################################################################
### TE/EAP (Total Employment/Economically Active Population) RATIO ###
###################################################################################################################################

# Merge part, pop, and unemp dataframes together
labour <- pop %>%
  left_join(part, by = "Country") %>%
  left_join(unemp, by = "Country")

# Select reference area, mean population (2019-2021),mean participation (2017-2021), mean unemployment (2012-2021)
labour <- labour %>%
  select("Country", "population", "participation", "unemployment")

# Calculate EAP 
labour$EAP <- ifelse(is.na(labour$participation) | is.na(labour$population), NA, 
                     (labour$participation/100) * labour$population)

# Calculate TE
labour$TE <- ifelse(is.na(labour$participation) | is.na(labour$population), NA, 
                    ((100-labour$unemployment)/100) * labour$EAP)

# Calculate TE/EAP
labour$TE.EAP.ratio <- ifelse(is.na(labour$TE) | is.na(labour$EAP), NA, 
                       (labour$TE / labour$EAP))

###################################################################################################################################
### THIRD PILLAR SIZE INTERNATIONAL ###
###################################################################################################################################

# Import JHU INTL Data
# SOURCE: Lester M. Salamon, S. Wojciech Sokolowski, Megan A. Haddock, and Associates, Explaining Civil Society Development: A Social Origins Approach, (Baltimore: Johns Hopkins University Press, 2017).
# PDF Available in Folder "JHU_2017_Civil Society Studies Comparative Data Tables"
# Manually copied into Excel

jhuintl <- read_excel("Input\\jhuintl.xlsx")

# Add correct column names
colnames(jhuintl) <- jhuintl[4,]

# Remove first 4 rows
jhuintl <- jhuintl[-(1:4),]

# Add correct row names 
jhuintl <- as.data.frame(jhuintl)
rownames(jhuintl) <- jhuintl[,1]

# Make columns numeric
jhuintl <- jhuintl %>%
  mutate(
    Year = as.numeric(Year),
    `Paid workers` = as.numeric(`Paid workers`),
    `JHU-Volunteers` = as.numeric(`JHU-Volunteers`),
    `JHU-Total` = as.numeric(`JHU-Total`)
  )

# Multiply values by 100 
jhuintl <- jhuintl %>%
  mutate(
    `Paid workers` = `Paid workers` * 100,
    `JHU-Volunteers` = `JHU-Volunteers` * 100,
    `JHU-Total` = `JHU-Total` * 100
  )

# Add TE.EAP.ratio column from labour dataframe to jhuintl dataframe 
jhuintl <- jhuintl %>%
  left_join(labour, by = "Country")
jhuintl <- jhuintl %>%
  select("Country", "Year", "Paid workers", "JHU-Volunteers", "JHU-Total", "TE.EAP.ratio")

# Calculate corrected third pillar size for paid workers, JHU-volunteers, JHU-Total using TE.EAP.ratio
jhuintl <- jhuintl %>%
  mutate(Plural_intl = `JHU-Total` / `TE.EAP.ratio`)
jhuintl <- jhuintl %>%
  mutate(`Plural - paid_intl` = `Paid workers` / `TE.EAP.ratio`)
jhuintl <- jhuintl %>%
  mutate(`Plural - volunteer_intl` = `JHU-Volunteers` / `TE.EAP.ratio`)

###################################################################################################################################
### THIRD PILLAR SIZE EUROPEAN ###
###################################################################################################################################

# Import JHU EU Dataset
# SOURCE: The size and composition of the european third sector. Salamon, LM, Sokolowski W. 2018. In "The third sector as a renewable resource for Europe" by Enjolras et al. 
# Manually copied into Excel 

jhueu <- read_excel("Input\\jhueu.xlsx")

# Remove last few rows (i.e., notes from the table)
jhueu <- jhueu[-c(37:44), ]

# Remove columns without percentage values 
jhueu <- jhueu[, -c(2:10)]

# Add correct column names
colnames(jhueu) <- c(
  "1" = "Country",
  "2" = "NPI paid",
  "3" = "NPI volunteer",
  "4" = "NPI total",
  "5" = "Coops paid",
  "6" = "Coops volunteers",
  "7" = "Coops total",
  "8" = "Social ent",
  "9" = "Direct vol",
  "10" = "Total TSE"
)

# Remove first 2 rows
jhueu <- jhueu[-(1:2),]

# Make columns numeric
jhueu[, -1] <- lapply(jhueu[, -1], as.numeric)

# Add correct row names
jhueu <- as.data.frame(jhueu)
rownames(jhueu) <- jhueu[,1]

# Calculate third pillar size for Plural, Plural-paid, and Plural-volunteer
jhueu <- jhueu %>%
  mutate(Plural_eu = `Total TSE` - `Direct vol`)
jhueu <- jhueu %>%
  mutate(`Plural - paid_eu` = coalesce(`NPI paid`, 0) + coalesce(`Coops paid`, 0) + coalesce(`Social ent`, 0))
jhueu <- jhueu %>%
  mutate(`Plural - volunteer_eu` = coalesce(`NPI volunteer`, 0) + coalesce(`Coops volunteers`, 0))

###################################################################################################################################
### THIRD PILLAR SIZE COMBINED (EUROPEAN PRIORITIZED) ###
###################################################################################################################################

# Combine jhueu and jhuintl dataframes into a new dataframe 
thirdpillar <- full_join(jhueu, jhuintl, by = "Country")

# Keep only summarized variables (Plural, Plural-paid, Plural-volunteer) from jhueu and jhuintl
thirdpillar <- thirdpillar %>%
  select(`Country`, 
         `Plural_intl`, `Plural - paid_intl`, `Plural - volunteer_intl`, 
         `Plural_eu`, `Plural - paid_eu`, `Plural - volunteer_eu`)

# Finalize third pillar size for each country 
thirdpillar$Plural <- ifelse(is.na(thirdpillar$Plural_eu), thirdpillar$Plural_intl, thirdpillar$Plural_eu)
thirdpillar$`Plural - paid` <- ifelse(is.na(thirdpillar$`Plural - paid_eu`), thirdpillar$`Plural - paid_intl`, thirdpillar$`Plural - paid_eu`)
thirdpillar$`Plural - volunteer` <- ifelse(is.na(thirdpillar$`Plural - volunteer_eu`), thirdpillar$`Plural - volunteer_intl`, thirdpillar$`Plural - volunteer_eu`)

# Re-order so final third pillar size comes first 
thirdpillar <- thirdpillar %>%
  select(`Country`,
         `Plural`, `Plural - paid`, `Plural - volunteer`,
         `Plural_intl`, `Plural - paid_intl`, `Plural - volunteer_intl`, 
         `Plural_eu`, `Plural - paid_eu`, `Plural - volunteer_eu`)

###################################################################################################################################
### MERGE ALL DATA TOGETHER TO CREATE THIRD PILLAR DATABASE###
###################################################################################################################################

# Merge population dataframe, participation dataframe, sectors_database dataframe and thirdpillar dataframe 
database <- full_join(population, participation, by = join_by(Country))
database <- full_join(database, sectors_database, by = join_by(Country))
database <- full_join(database, thirdpillar, by = join_by(Country))

# Write excel file for database
write_xlsx(database, "output\\Database_thirdpillar.xlsx")

###################################################################################################################################
###################################################################################################################################
### MAIN ANALYSIS ###
###################################################################################################################################
###################################################################################################################################

# List all objects in Glob Env that are relevant for analysis 
objects_to_keep <- (c("database", "polity5", "wbdata"))

# Use the rm function to remove all other objects
rm(list = setdiff(ls(), objects_to_keep), envir = .GlobalEnv)

# Create new dataframe
all_countries <- database

# Fix country names and codes to match polity5 data
# Be careful about these country names and codes. 
  # It is possible that more countries will be added to the list since this script was written. 
  # You will need to merge the dataframes carefully and ensure that countries with complete data are not written differently in separate rows.
all_countries$Country[all_countries$Country == "Bosnia and Herzegovina"] <- "Bosnia"
all_countries$Country[all_countries$Country == "Czechia"] <- "Czech Republic"
all_countries$Country[all_countries$Country == "Côte d'Ivoire"] <- "Cote D'Ivoire"
all_countries$Country[all_countries$Country == "Iran, Islamic Republic of"] <- "Iran"
all_countries$Country[all_countries$Country == "Korea, Republic of"] <- "Korea"
all_countries$Country[all_countries$Country == "Lao People's Democratic Republic"] <- "Laos"
all_countries$Country[all_countries$Country == "Moldova, Republic of"] <- "Moldova"
all_countries$Country[all_countries$Country == "Myanmar"] <- "Myanmar (Burma)"
all_countries$Country[all_countries$Country == "North Macedonia"] <- "Macedonia"
all_countries$Country[all_countries$Country == "Russian Federation"] <- "Russia"
all_countries$Country[all_countries$Country == "Slovakia"] <- "Slovak Republic"
all_countries$Country[all_countries$Country == "Taiwan, China"] <- "Taiwan"
all_countries$Country[all_countries$Country == "United Arab Emirates"] <- "UAE"
all_countries$Country[all_countries$Country == "Venezuela, Bolivarian Republic of"] <- "Venezuela"
all_countries$Country[all_countries$Country == "Viet Nam"] <- "Vietnam"

# Create new dataframe 
countryLevelWbPolity <- polity5

# Merge all_countries with polity 5
all_countries = full_join(all_countries, countryLevelWbPolity, by = join_by(Country))
all_countries <- all_countries %>%
  select(Country, countrycode, everything())

# Add correct country codes to all_countries after adding polity data
# Be careful about these country names and codes. 
  # It is possible that more countries will be added to the list since this script was written. 
  # You will need to merge the dataframes carefully and ensure that countries with complete data are not written differently in separate rows.
all_countries$countrycode[all_countries$Country == "Korea"] <- "KOR"
all_countries$countrycode[all_countries$Country == "Tonga"] <- "TON"
all_countries$countrycode[all_countries$Country == "Iceland"] <- "ISL"
all_countries$countrycode[all_countries$Country == "Belize"] <- "BLZ"
all_countries$countrycode[all_countries$Country == "Barbados"] <- "BRB"
all_countries$countrycode[all_countries$Country == "Maldives"] <- "MDV"
all_countries$countrycode[all_countries$Country == "Saint Lucia"] <- "LCA"
all_countries$countrycode[all_countries$Country == "Samoa"] <- "WSM"
all_countries$countrycode[all_countries$Country == "Vanuatu"] <- "VUT"
all_countries$countrycode[all_countries$Country == "New Caledonia"] <- "NCL"
all_countries$countrycode[all_countries$Country == "Brunei Darussalam"] <- "BRN"
all_countries$countrycode[all_countries$Country == "Switzerland"] <- "CHE"

all_countries <- all_countries %>%
  mutate(countrycode = ifelse(Country == "Algeria", "DZA", countrycode),
         countrycode = ifelse(Country == "Australia", "AUS", countrycode),
         countrycode = ifelse(Country == "Brukina Faso", "BFA", countrycode),
         countrycode = ifelse(Country == "Bangladesh", "BGD", countrycode),
         countrycode = ifelse(Country == "Bosnia and Herzegovina", "BIH", countrycode),
         countrycode = ifelse(Country == "Bosnia", "BIH", countrycode),
         countrycode = ifelse(Country == "Botswana", "BWA", countrycode),
         countrycode = ifelse(Country == "Bulgaria", "BGR", countrycode),
         countrycode = ifelse(Country == "Cambodia", "KHM", countrycode),
         countrycode = ifelse(Country == "Cape Verde", "CPV", countrycode),
         countrycode = ifelse(Country == "Chad", "TCD", countrycode),
         countrycode = ifelse(Country == "Costa Rica", "CRI", countrycode),
         countrycode = ifelse(Country == "Croatia", "HRV", countrycode),
         countrycode = ifelse(Country == "Czechia", "CZE", countrycode),
         countrycode = ifelse(Country == "Denmark", "DNK", countrycode),
         countrycode = ifelse(Country == "France", "FRA", countrycode),
         countrycode = ifelse(Country == "Gambia", "GMB", countrycode),
         countrycode = ifelse(Country == "Georgia", "GEO", countrycode),
         countrycode = ifelse(Country == "Germany", "DEU", countrycode),         
         countrycode = ifelse(Country == "Guatemala", "GTM", countrycode),
         countrycode = ifelse(Country == "Honduras", "HND", countrycode),         
         countrycode = ifelse(Country == "Indonesia", "IDN", countrycode),         
         countrycode = ifelse(Country == "Ireland", "IRL", countrycode),         
         countrycode = ifelse(Country == "Cote d'Ivoire", "CIV", countrycode),         
         countrycode = ifelse(Country == "Kyrgyzstan", "KGZ", countrycode),         
         countrycode = ifelse(Country == "Latvia", "LVA", countrycode),         
         countrycode = ifelse(Country == "Lebanon", "LBN", countrycode),         
         countrycode = ifelse(Country == "Lesotho", "LSO", countrycode),         
         countrycode = ifelse(Country == "Lithuania", "LTU", countrycode),         
         countrycode = ifelse(Country == "Mauritania", "MRT", countrycode),         
         countrycode = ifelse(Country == "Moldova, Republic of", "MDA", countrycode),         
         countrycode = ifelse(Country == "Montenegro", "MNE", countrycode),         
         countrycode = ifelse(Country == "Mongolia", "MNG", countrycode),         
         countrycode = ifelse(Country == "Myanmar", "MMR", countrycode),         
         countrycode = ifelse(Country == "Nepal", "NPL", countrycode),
         countrycode = ifelse(Country == "New Zealand", "NZL", countrycode), 
         countrycode = ifelse(Country == "Nigeria", "NGA", countrycode), 
         countrycode = ifelse(Country == "Niger", "NER", countrycode), 
         countrycode = ifelse(Country == "Netherlands", "NLD", countrycode), 
         countrycode = ifelse(Country == "Oman", "OMN", countrycode), 
         countrycode = ifelse(Country == "Paraguay", "PRY", countrycode), 
         countrycode = ifelse(Country == "Philippines", "PHL", countrycode), 
         countrycode = ifelse(Country == "Portugal", "PRT", countrycode), 
         countrycode = ifelse(Country == "Romania", "ROU", countrycode), 
         countrycode = ifelse(Country == "South Africa", "ZAF", countrycode), 
         countrycode = ifelse(Country == "El Salvador", "SLV", countrycode), 
         countrycode = ifelse(Country == "Serbia", "SRB", countrycode), 
         countrycode = ifelse(Country == "Sierra Leone", "SLE", countrycode), 
         countrycode = ifelse(Country == "Slovakia", "SVK", countrycode), 
         countrycode = ifelse(Country == "Spain", "ESP", countrycode), 
         countrycode = ifelse(Country == "Sri Lanka", "LKA", countrycode), 
         countrycode = ifelse(Country == "Sweden", "SWE", countrycode), 
         countrycode = ifelse(Country == "Thailand", "THA", countrycode), 
         countrycode = ifelse(Country == "Togo", "TGO", countrycode), 
         countrycode = ifelse(Country == "United Arab Emirates", "ARE", countrycode), 
         countrycode = ifelse(Country == "United Kingdom", "GBR", countrycode), 
         countrycode = ifelse(Country == "Uruguay", "URY", countrycode),
         countrycode = ifelse(Country == "Venezuela, Bolivarian Republic of", "VEN", countrycode), 
         countrycode = ifelse(Country == "Viet Nam", "VNM", countrycode), 
         countrycode = ifelse(Country == "Zambia", "ZMB", countrycode), 
         countrycode = ifelse(Country == "Zimbabwe", "ZWE", countrycode), 
         countrycode = ifelse(Country == "Austria", "AUT", countrycode), 
         countrycode = ifelse(Country == "Slovenia", "SVN", countrycode), 
         countrycode = ifelse(Country == "Switzerland", "CHE", countrycode)
  )

# Merge all_countries with wbdata
all_countries = full_join(all_countries, wbdata, by = (c("Country", "countrycode")))
all_countries <- all_countries %>%
  select(Country, countrycode, everything())

# Change all country names to be match
# Be careful about these country names and codes. 
  # It is possible that more countries will be added to the list since this script was written. 
  # You will need to merge the dataframes carefully and ensure that countries with complete data are not written differently in separate rows.

all_countries$Country[all_countries$Country == "Bosnia and Herzegovina"] <- "Bosnia"
all_countries$Country[all_countries$Country == "Czechia"] <- "Czech Republic"
all_countries$Country[all_countries$Country == "Côte d'Ivoire"] <- "Cote D'Ivoire"
all_countries$Country[all_countries$Country == "Cote d'Ivoire"] <- "Cote D'Ivoire"
all_countries$Country[all_countries$Country == "Iran, Islamic Republic of"] <- "Iran"
all_countries$Country[all_countries$Country == "Korea, Republic of"] <- "Korea"
all_countries$Country[all_countries$Country == "Lao People's Democratic Republic"] <- "Laos"
all_countries$Country[all_countries$Country == "Moldova, Republic of"] <- "Moldova"
all_countries$Country[all_countries$Country == "Myanmar"] <- "Myanmar (Burma)"
all_countries$Country[all_countries$Country == "North Macedonia"] <- "Macedonia"
all_countries$Country[all_countries$Country == "Russian Federation"] <- "Russia"
all_countries$Country[all_countries$Country == "Slovakia"] <- "Slovak Republic"
all_countries$Country[all_countries$Country == "Taiwan, China"] <- "Taiwan"
all_countries$Country[all_countries$Country == "United Arab Emirates"] <- "UAE"
all_countries$Country[all_countries$Country == "Venezuela, Bolivarian Republic of"] <- "Venezuela"
all_countries$Country[all_countries$Country == "Viet Nam"] <- "Vietnam"
all_countries$Country[all_countries$Country == "Cabo Verde"] <- "Cape Verde"
all_countries$Country[all_countries$Country == "Egypt, Arab Rep."] <- "Egypt"
all_countries$Country[all_countries$Country == "Gambia, The"] <- "Gambia"
all_countries$Country[all_countries$Country == "Iran, Islamic Rep."] <- "Iran"
all_countries$Country[all_countries$Country == "Korea South"] <- "Korea"
all_countries$Country[all_countries$Country == "Korea, Rep."] <- "Korea"
all_countries$Country[all_countries$Country == "Lao PDR"] <- "Laos"
all_countries$Country[all_countries$Country == "Kyrgyz Republic"] <- "Kyrgyzstan"
all_countries$Country[all_countries$Country == "Turkiye"] <- "Turkey"
all_countries$Country[all_countries$Country == "Venezuela, RB"] <- "Venezuela"

# Remove columns that are not needed in analysis and re-order columns
all_countries <- all_countries %>%
  select(Country, population, participation, 
         AG, MAN, CON, TRADE, PUB, OTHER, 
         Plural, `Plural - paid`, `Plural - volunteer`, 
         polity, democ, durable, gdpPerCapita)

# Aggregate data so there is one row per country (instead of >1)
all_countries <- all_countries %>%
  group_by(Country) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup()

# Convert NaN to NA
all_countries <- lapply(all_countries, function(x) ifelse(is.nan(x), NA, x))
all_countries <- as.data.frame(all_countries)

# Make the names of countries a factor
names_all_countries <- all_countries[,1]

# Make Countries the row names 
rownames(all_countries) <- names_all_countries

# Remove Country column
all_countries[,1] <- NULL

###################################################################################################################################
### Footnote 16 Analysis ###
###################################################################################################################################

# Remove # to continue analysis with removal of these countries

# Remove rows for countries that were imputed in the EU JHU dataframe
  # countries_to_remove <- c('Bulgaria', 'Croatia', 'Cyprus', 'Estonia', 'Greece', 'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Slovenia')

# Use subset to select rows that are NOT in the list of countries to remove
  # all_countries <- subset(all_countries, !(row.names(all_countries) %in% countries_to_remove))

###################################################################################################################################

# Create input dataframe
all_countries_input = all_countries[,c(1:8,12:length(all_countries))]
all_countries_input <- all_countries[complete.cases(all_countries_input),]
all_countries_input <- all_countries_input[,c(1:8,12:length(all_countries_input))]

# Create model dataframe
all_countries_model <- all_countries[complete.cases(all_countries),]

# PLURAL  - everything with plural
all_countries_model_plural <- all_countries_model[,c(1:8,12:length(all_countries_model),9)]

# PAID - everything with paid
all_countries_model_paid <- all_countries_model[,c(1:8,12:length(all_countries_model),10)]

# VOLUNTEERS  - everything with volunteers
all_countries_model_volunteers <- all_countries_model[,c(1:8,12:length(all_countries_model),11)]

###################################################################################################################################

### Summary statistics tables ###

sumtable(all_countries_model, digits=5)
sumtable(all_countries_model_plural, digits = 5)
sumtable(all_countries_model_paid, digits = 5)
sumtable(all_countries_model_volunteers, digits = 5)

# Set seed for replicability
set.seed(123)

###################################################################################################################################

### Create random forest model ###

# Set number of folds in random forsest model
cvFolds = 10

# Select type of model
model = "SL.randomForest"
  
###################################################################################################################################

### PLURAL ###

# actual (all_countries_model_plural$plural) vs. predicted (sl$sl.predict) third pillar size
# Model that makes the predictions of third pillar size for the countries, running it 10 times (for 10 folds) 
sl = CV.SuperLearner(Y = all_countries_model_plural$Plural, X = all_countries_model_plural[,c(1:(length(all_countries_model_plural)-1))], 
                     family = gaussian(), SL.library = model,
                     control = list(saveFitLibrary = TRUE),
                     cvControl = list(V = cvFolds, shuffle = FALSE))
modelOls <- lm(all_countries_model_plural$Plural ~ sl$SL.predict)
summary(modelOls)
  # Multiple R-squared: 0.7386 #for this analysis with 120 input and 51 model countries

# CVrisk is cross-validated risk estimate for each fold that is run (10)
# Goes through each risk estimate for each fold and then the average of these is reported in the paper (as Mean squared error)
risks = c()
for (i in 1:length(sl$AllSL)){
  risks[i] = sl$AllSL[[i]]$cvRisk  
}

###################################################################################################################################

### PAID ###

#actual (all_countries_model_plural$paid) vs. predicted (slpaid$sl.predict) third pillar size
slPaid = CV.SuperLearner(Y = all_countries_model_paid$Plural...paid, X = all_countries_model_paid[,c(1:(length(all_countries_model_paid)-1))], 
                         family = gaussian(), SL.library = model,
                         control = list(saveFitLibrary = TRUE),
                         cvControl = list(V = cvFolds, shuffle = FALSE))
modelPaid <- lm(all_countries_model_paid$Plural...paid ~ slPaid$SL.predict)
summary(modelPaid)

# CVrisk is cross-validated risk estimate for each fold that is run (10)
# Goes through each risk estiamte for each fold and then the average of these is reported in the paper (we called it Mean squared error)
risksPaid = c()
for (i in 1:length(slPaid$AllSL)){
  risksPaid[i] = slPaid$AllSL[[i]]$cvRisk  
}

###################################################################################################################################

### Calculating R-squared ###

# calculates an R squared for each fold so that we can then calculated a standard deviation for the R squareds (R squared summary)
# This is because the superlearner function does not calculate standard deviations, so we needed to calculate them ourselves 

rsquareds = c()

for (i in 1:length(sl$AllSL)) {
  fold = sl$AllSL[[i]]
  predictionsInTheFold = data.frame(fold$SL.predict)
  predictionsInTheFold$Country <- rownames(predictionsInTheFold)
  
  dfMatching <- data.frame(all_countries)
  dfMatching$Country <- rownames(dfMatching)  
  predictionsMatched = merge(dfMatching, predictionsInTheFold, by = "Country", all = TRUE) 
  
  modelFold <- lm(predictionsMatched$Plural ~ predictionsMatched$fold.SL.predict)
  
  rsquareds[i] = summary(modelFold)$r.squared
}

# Paid R-squared
rsquaredsPaid = c()
for (i in 1:length(slPaid$AllSL)) {
  fold = slPaid$AllSL[[i]]
  predictionsInTheFold = data.frame(fold$SL.predict)
  predictionsInTheFold$Country <- rownames(predictionsInTheFold)
  
  dfMatching <- data.frame(all_countries)
  dfMatching$Country <- rownames(dfMatching)  
  predictionsMatched = merge(dfMatching, predictionsInTheFold, by = "Country", all = TRUE) 
  
  modelFold <- lm(predictionsMatched$Plural ~ predictionsMatched$fold.SL.predict)
  
  rsquaredsPaid[i] = summary(modelFold)$r.squared
}

### R-squared summary
summary(rsquareds)
summary(rsquaredsPaid)
### Mean squared error summary
summary(risks)
summary(risksPaid)

###################################################################################################################################

### Predictions ###

# Takes what was trained before and makes predictions on the countries that we have all the data for but not the third pillar size data 

# Random Forest Model using paid third pillar for 50 countries as output, and all predictor variables as input. 
# Predicted values for all 117 countries based on all_countries_input. 
predictions = SL.randomForest(Y = all_countries_model_paid$Plural...paid, X = all_countries_model_paid[1:(length(all_countries_model_paid)-1)],
                              newX = all_countries_input, obsWeights = rep(1,dim(all_countries_model_paid)[1]), family = gaussian(), seed = 123)
predictionsPaid = predictions$pred

# Random Forest Model using volunteer third pillar for 50 countries as output, all predictor variables as input. 
# Predicted values for all 117 countries based on all_countries_input. 
predictions = SL.randomForest(Y = all_countries_model_volunteers$Plural...volunteer, X = all_countries_model_volunteers[1:(length(all_countries_model_volunteers)-1)],
                              newX = all_countries_input, obsWeights = rep(1,dim(all_countries_model_volunteers)[1]), family = gaussian(), seed = 123)
predictionsVolunteers = predictions$pred

###################################################################################################################################

### Confidence Intervals ###

# Make errors
rf_fit <- randomForest(Plural ~ ., data=all_countries_model_plural,  replace=TRUE, keep.inbag=TRUE, seed = 123) # Build the model

# Plot the error as the number of trees increases
plot(rf_fit)

# Plot the important variables
varImpPlot(rf_fit,col="blue",pch= 2)

# Calculate the Variance
# This function is user-made and allows you to create the confidence interval around the estimates (Wager et al., 2014)
# A blog on how to do this more specifically: https://blog.revolutionanalytics.com/2016/03/confidence-intervals-for-random-forest.html
predictions <- randomForestInfJack(rf_fit, all_countries_input, calibrate = TRUE)

###################################################################################################################################

### Combine into dataset ###

df <- data.frame(predictions)
df <- mutate(df, p5 = y.hat-1.96*sqrt(var.hat))
df <- mutate(df, p95 = y.hat+1.96*sqrt(var.hat))
df <- mutate(df, predictionsPaid = predictionsPaid)
df <- mutate(df, predictionsVolunteers = predictionsVolunteers)

# Renaming
df$Country <- rownames(all_countries_input)
all_countries_model$Country = rownames(all_countries_model)
dfNew = merge(df, all_countries_model, by = "Country", all = TRUE)  

dfNew = dfNew[c(1,16,2,4:7)]
colnames(dfNew)[2] = 'Actual'
colnames(dfNew)[3] = 'Average'
colnames(dfNew)[4] = '5th pct'
colnames(dfNew)[5] = '95th pct'
colnames(dfNew)[6] = 'Paid staff'
colnames(dfNew)[7] = 'Volunteers'

###################################################################################################################################

# Mean and standard deviation of predicted third pillar size (TPT) values
mean(dfNew$Average)
sd(dfNew$Average)

###################################################################################################################################

# Rename average predicted third pillar size to total third pillar size
dfNew$Total = dfNew$Average

# Keep actual third pillar size if third pillar size is there
dfNew$Total[!is.na(dfNew$Actual)] = dfNew$Actual[!is.na(dfNew$Actual)]

# Mean and standard deviation for third pillar size (mix of actual and predicted)
mean(dfNew$Total)
sd(dfNew$Total)

###################################################################################################################################

# Save dataset with actual and predicted third pillar sizes, 5th and 95th percentiles, paid, 
  # volunteers, total (total is actual if actual is there, else it is the predicted third pillar size)
write.xlsx(dfNew, "output\\Annex.xlsx")

# Rename to dataframe called third pillar
thirdpillar <- dfNew

###################################################################################################################################
###################################################################################################################################
### FIGURES AND TABLES IN PAPER ###
###################################################################################################################################
###################################################################################################################################

###################################################################################################################################
### Figure 1 ###
###################################################################################################################################

# Import world giving index data for high-income countries and low-income countries
# Data is extracted from the CAF World Giving Index 2022, manually copied from Figure 1
give <- read_excel("input\\CAF_Fig 1.xlsx")

# Calculate Total of Volunteering and Donating money
give <- give %>%
  mutate(Total = Volunteering + `Donating money`) %>%
  arrange(Year)

# Import google trends data
# Nine search terms were used on Google Trends, with filters "Worldwide", "2004-present", "All categories", and "Web Search":
  # civil society
  # community
  # non profits
  # non-governmental organizations
  # plural sector
  # social sector
  # third pillar
  # third sector
  # voluntary
# This link is for Google Trends page (use each term in a separate search): https://trends.google.com/trends/ 
# All .csv files are in Input folder already labelled as their search term 

# Import each of the search term csv files
trend1 <- read.csv("input\\civil society.csv", skip = 2)
trend2 <- read.csv("input\\community.csv", skip = 2)
trend3 <- read.csv("input\\non profits.csv", skip = 2)
trend4 <- read.csv("input\\non-governmental organizations.csv", skip = 2)
trend5 <- read.csv("input\\plural sector.csv", skip = 2)
trend6 <- read.csv("input\\social sector.csv", skip = 2)
trend7 <- read.csv("input\\third pillar.csv", skip = 2)
trend8 <- read.csv("input\\third sector.csv", skip = 2)
trend9 <- read.csv("input\\voluntary.csv", skip = 2)

# Group all dataframes together
trend <- Reduce(function(x, y) merge(x, y, by = "Month", all = TRUE), 
                list(trend1, trend2, trend3, trend4, trend5, trend6, trend7, trend8, trend9))

# Rename third pillar variable
trend <- trend %>%
  rename("third pillar" := `third.pillar...Worldwide.`,
         "plural sector" := `plural.sector...Worldwide.`,
         "community":= `community...Worldwide.`,
         "third sector":= `third.sector...Worldwide.`,
         "non profits":= `non.profits...Worldwide.`,
         "NGOs":= `non.governmental.organizations...Worldwide.`,
         "voluntary":= `voluntary...Worldwide.`,
         "civil society":= `civil.society...Worldwide.`,
         "social sector":= `social.sector...Worldwide.`)

# Separate Year and Month into two columns
trend <- separate(trend, Month, into = c("Year", "Month"), sep = "-")

# Remove Month column
trend <- trend[,-2]

# Make all variables numeric
trend[] <- lapply(trend, as.numeric)

# Aggregate google trends data so there is one value per year (using the sum)
trend <- aggregate(. ~ Year, data = trend, FUN = sum)

# Make Row Names same as Year
rownames(trend) <- (trend[,1])

# Sum all the counts together (not including year column)
trend$sum <- rowSums(trend[,-1])

# Keep data with years 2009 to 2021 to replicate figure and overlay it with CAF figure
trend <- trend %>%
  filter(between(as.integer(substring(Year, 1, 4)), 2009, 2021))

# Index the values so that 2009 = 100 
sum_2009 <- subset(trend, Year == "2009")$sum
trend$`sum index` <- round((trend$`sum` / sum_2009) * 100)

# Create figure 
  # Make bar chart for 'give' data
bar_chart <- ggplot(give, aes(x = Year)) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) + 
  geom_bar(aes(y = `Total`, fill = "Donating money"), stat = "identity", width = 0.8) +
  geom_bar(aes(y = `Volunteering`, fill = "Volunteering"), stat = "identity", width = 0.8) +
  geom_text(aes(label = paste0(`Total`,"%"), y = `Total`), vjust = -0.2, position = position_dodge(0.9), size = 4.5) +
  geom_text(aes(label = paste0(Volunteering, "%"), y = Volunteering), vjust = 8, position = position_dodge(0.9), size = 4, colour = "white") +
  geom_text(aes(label = paste0(`Donating money`, "%"), y = `Donating money`), vjust = 1, position = position_dodge(0.9), size = 4) +
  scale_fill_manual(values = c("Donating money" = "lightgrey", "Volunteering" = "darkgrey")) +
  labs(fill = "", y = "Participation (%)") +
  scale_x_continuous(breaks = seq(min(give$Year), max(give$Year), by = 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 60, by = 5), minor_breaks = NULL) + 
  theme_minimal() +
  theme(axis.title.x = element_text(hjust = 0.5, vjust = -2),
        axis.title.y = element_text(hjust = 0.5, vjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(margin = margin(t = -4), vjust = 1),
        axis.text.y = element_text(margin = margin(r = 2), hjust = 1),
        plot.margin = margin(7, 7, 7, 7))
bar_chart

  # Overlay line plot with bar chart
combined_plot <- bar_chart +
  geom_line(data = trend, aes(x = Year, y = `sum index` * (60/100), color = "Third pillar"), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . * (100/60), name = "Interest in the third pillar (2009 = 100)", breaks = seq(0, 100, by = 25))) +
  scale_color_manual(values = c("Third pillar" = "red")) +
  labs(color = "Interest") +
  theme(
    axis.line.y.right = element_line(color = "black", size = 0.5),
    axis.line.y.left = element_line(color = "black", size = 0.5),
    axis.ticks.y = element_line(color = "black"), 
    axis.ticks.y.right = element_line(color = "black"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y.right = element_text(size = 14),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y.right = element_text(size = 12, colour = "black"),
    legend.position = "top", 
    plot.margin = margin(20,20,20,20)
  ) +
  guides(fill = guide_legend(title = ""), color = guide_legend(title = ""))

combined_plot

# Export plot
ggsave("output\\Figure 1.pdf",combined_plot, width = 40, height = 20, units = "cm")

###################################################################################################################################
### Figure 2 ###
###################################################################################################################################

# Plotting actual vs. predicted third pillar size values for total and for paid 
dfGraphs <- full_join(dfNew,all_countries_model, by = join_by(Country))

plot(dfGraphs$Actual,dfGraphs$Average,xlab="Actual",ylab="Predicted")
text(x=dfGraphs$Actual,y=dfGraphs$Average,labels= )

pluralPlot = ggplot(dfGraphs, aes(`Average`,`Actual`)) +
  ylab("Third pillar size - total (actual)") + xlab("Third pillar size - total (predicted)") +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  expand_limits(x = c(0,20), y = c(0,20)) +
  geom_text_repel(aes(label = dfGraphs$Country))

paidPlot = ggplot(dfGraphs, aes(`Paid staff`, `Plural...paid`)) +
  ylab("Third pillar size - paid staff (actual)") + xlab("Third pillar size - paid staff (predicted)") +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  expand_limits(x = c(0,20), y = c(0,20)) +
  geom_text_repel(aes(label = dfGraphs$Country))

plot = grid.arrange(pluralPlot, paidPlot, ncol=2)

ggsave("output\\Figure 2.pdf",plot, width = 40, height = 20, units = "cm")

###################################################################################################################################
### Figure 3 ###
###################################################################################################################################

# Load world map
world_map <- st_as_sf(ne_countries())

# Select variables needed for world map
world_map <- world_map %>%
  select(name)

# Correct country names in world_map
world_map$name[world_map$name == "Bosnia and Herz."] <- "Bosnia"
world_map$name[world_map$name == "Côte d'Ivoire"] <- "Cote D'Ivoire"
world_map$name[world_map$name == "Czechia"] <- "Czech Republic"
world_map$name[world_map$name == "Dominican Rep."] <- "Dominican Republic"
world_map$name[world_map$name == "Myanmar"] <- "Myanmar (Burma)"
world_map$name[world_map$name == "North Macedonia"] <- "Macedonia"
world_map$name[world_map$name == "Slovakia"] <- "Slovak Republic"
world_map$name[world_map$name == "South Korea"] <- "Korea"
world_map$name[world_map$name == "United Arab Emirates"] <- "UAE"
world_map$name[world_map$name == "United States of America"] <- "United States"

world_data <- left_join(world_map, thirdpillar, by = c("name" = "Country"))

# Create the world map plot with a border around the entire map
world_map_plot <- ggplot() +
  geom_sf(data = world_data, aes(fill = Total), color = "black") +  # Border for countries
  scale_fill_gradient(low = "lightpink", high = "red", na.value = "lightgray", 
                      limits = c(1,20), 
                      breaks = c(1,20), 
                      labels = c(1,20),
                      guide = guide_colorbar(ticks = FALSE)) +
  labs(fill = "") +
  theme_void() +
  theme(
    legend.text = element_text(size = 8, face = "bold", colour = "black"),
    legend.title = element_text(size = 10),
    legend.position = "bottom", 
    legend.key = element_rect(fill = "darkgrey", colour = "darkgrey", linewidth = 0.5),
    plot.margin = margin(20,20,20,20)
    )

print(world_map_plot)

# Export world map to pdf
ggsave("output\\Figure 3.pdf", plot = world_map_plot, width = 40, height = 20, units = "cm")

###################################################################################################################################
### Figure 4 ### 
###################################################################################################################################

# Calculate mean third pillar size and mean GDP 
meanTP <- mean(dfNew$Total)
meanGDP <- mean(dfGraphs$gdpPerCapita, na.rm=TRUE)

# Create plot of GDP by third pillar size (total)
paidPluralGDP = ggplot(df, aes(dfGraphs$gdpPerCapita,dfNew$Total)) +
  ylab("Third pillar size - total (predicted)") + 
  xlab("GDP per capita, 2015 constant $US") +
  geom_point() +
  expand_limits(x = c(100,100000), y = c(0,20)) +
  geom_hline(yintercept = mean(dfNew$Average), color="red") +
  geom_vline(xintercept = mean(dfGraphs$gdpPerCapita, na.rm=TRUE), color="red") +
  annotate("text", x=24000, y=16.4, label = paste("Global average: ", round(meanGDP, 2)), angle=90, size=4, color="red") +
  annotate("text", x=55000, y=5.6, label = paste("Global average: ", round(meanTP, 2)), angle=0, size=4, color="red") +
  geom_text_repel(aes(label = dfNew$Country), size=5)
paidPluralGDP 

# Export to pdf file
ggsave("output\\Figure 4.pdf",paidPluralGDP, width = 40, height = 20, units = "cm")

###################################################################################################################################
### Table 1 ###
###################################################################################################################################

# Create summary statistics table as dataframe
table1 <- sumtable(all_countries_model, digits=5, out = "csv")
rownames(table1) <- table1$Variable
table1$`Pctl. 25` <- NULL
table1$`Pctl. 75` <- NULL

# Reorder rows
desired_order <- c(
  "population", "participation", "AG", "MAN", "CON", "TRADE", "PUB", "OTHER", "gdpPerCapita",
  "polity", "democ", "durable", "Plural", "Plural...paid", "Plural...volunteer")
table1 <- table1[desired_order,]

# Export dataframe as excel file
write.xlsx(table1, "output\\Table 1.xlsx")

###################################################################################################################################
### Table 2 ###
###################################################################################################################################

# Calculate the summary statistics for R2 and Mean squared error
rsquareds_mean <- mean(rsquareds)
rsquareds_sd <- sd(rsquareds)
rsquareds_min <- min(rsquareds)
rsquareds_max <- max(rsquareds)

rsquaredsPaid_mean <- mean(rsquaredsPaid)
rsquaredsPaid_sd <- sd(rsquaredsPaid)
rsquaredsPaid_min <- min(rsquaredsPaid)
rsquaredsPaid_max <- max(rsquaredsPaid)

risks_mean <- mean(risks)
risks_sd <- sd(risks)
risks_min <- min(risks)
risks_max <- max(risks)

risksPaid_mean <- mean(risksPaid)
risksPaid_sd <- sd(risksPaid)
risksPaid_min <- min(risksPaid)
risksPaid_max <- max(risksPaid)

# Create a dataframe "table2"
table2 <- data.frame(
  Statistic = c("rsquareds", "rsquaredsPaid", "risks", "risksPaid"),
  Mean = c(rsquareds_mean, rsquaredsPaid_mean, risks_mean, risksPaid_mean),
  SD = c(rsquareds_sd, rsquaredsPaid_sd, risks_sd, risksPaid_sd),
  Minimum = c(rsquareds_min, rsquaredsPaid_min, risks_min, risksPaid_min),
  Maximum = c(rsquareds_max, rsquaredsPaid_max, risks_max, risksPaid_max)
)

# Export dataframe as excel file
write.xlsx(table2, "output\\Table 2.xlsx")

###################################################################################################################################
### Table 3 ### 
###################################################################################################################################

# Import grouping of countries based on ILO Stat 
# Updated country groupings are found here: https://ilostat.ilo.org/resources/concepts-and-definitions/classification-country-groupings/ 
country_groups <- read_excel("input\\country groups.xlsx")

country_groups$Country[country_groups$Country == "Bosnia and Herzegovina"] <- "Bosnia"
country_groups$Country[country_groups$Country == "Czechia"] <- "Czech Republic"
country_groups$Country[country_groups$Country == "Côte d'Ivoire"] <- "Cote D'Ivoire"
country_groups$Country[country_groups$Country == "Iran, Islamic Republic of"] <- "Iran"
country_groups$Country[country_groups$Country == "Korea, Republic of"] <- "Korea"
country_groups$Country[country_groups$Country == "Lao People's Democratic Republic"] <- "Laos"
country_groups$Country[country_groups$Country == "North Macedonia"] <- "Macedonia"
country_groups$Country[country_groups$Country == "Moldova, Republic of"] <- "Moldova"
country_groups$Country[country_groups$Country == "Myanmar"] <- "Myanmar (Burma)"
country_groups$Country[country_groups$Country == "Russian Federation"] <- "Russia"
country_groups$Country[country_groups$Country == "Slovakia"] <- "Slovak Republic"
country_groups$Country[country_groups$Country == "Viet Nam"] <- "Vietnam"
country_groups$Country[country_groups$Country == "United Arab Emirates"] <- "UAE"

# Merge third pillar dataset with country_groups                         
thirdpillar <- full_join(thirdpillar, country_groups, by = join_by(Country))

# Remove countries without third pillar size data
thirdpillar <- thirdpillar %>%
  slice(1:120)

# Make table into a dataframe 
table3_sub <- thirdpillar %>%
  group_by(`ILO Region`, `ILO Subregion - Broad`) %>%
  summarise(
    N = n(),
    Mean = mean(Total, na.rm = TRUE),
    SD = sd(Total, na.rm = TRUE),
    Min = min(Total, na.rm = TRUE),
    Max = max(Total, na.rm = TRUE)
  )
table3_main <- thirdpillar %>%
  group_by(`ILO Region`) %>%
  summarise(
    N = n(),
    Mean = mean(Total, na.rm = TRUE),
    SD = sd(Total, na.rm = TRUE),
    Min = min(Total, na.rm = TRUE),
    Max = max(Total, na.rm = TRUE)
  ) 
table3_global <- thirdpillar %>%
  summarise(
    N = n(),
    Mean = mean(Total, na.rm = TRUE),
    SD = sd(Total, na.rm = TRUE),
    Min = min(Total, na.rm = TRUE),
    Max = max(Total, na.rm = TRUE)
  )
table3_global$`ILO Region` <- "Global"
table3_global <- table3_global %>%
  select(`ILO Region`, N, Mean, SD, Min, Max)

# Combine all tables to make Table 3
table3 <- bind_rows(table3_sub, table3_main, table3_global)
table3 <- as.data.frame(table3)

# Re-order logically

# Define the custom order for reordering the rows
custom_order <- c("Africa_NA", "Africa_Northern Africa", "Africa_Sub-Saharan Africa", 
                  "Americas_NA", "Americas_Northern America", "Americas_Latin America and the Caribbean", 
                  "Arab States_NA", "Arab States_Arab States", 
                  "Asia and the Pacific_NA", "Asia and the Pacific_Eastern Asia", 
                  "Asia and the Pacific_Southern Asia", "Asia and the Pacific_South-Eastern Asia and the Pacific", 
                  "Europe and Central Asia_NA", "Europe and Central Asia_Central and Western Asia", 
                  "Europe and Central Asia_Eastern Europe", "Europe and Central and Western Asia", 
                  "Europe and Central Asia_Northern, Southern and Western Europe", "Global")

# Reorder the rows in the table3 dataframe based on the custom order
table3 <- table3 %>%
  arrange(factor(paste(`ILO Region`, `ILO Subregion - Broad`, sep = "_"), levels = custom_order))

# Export dataframe as excel file
write.xlsx(table3, "output\\Table 3.xlsx")

###################################################################################################################################
###################################################################################################################################
### ADDITIONAL ANALYSES (IN TEXT STATISTICAL REPORTING)
###################################################################################################################################
###################################################################################################################################

###################################################################################################################################
### Footnote 11 Regression ###
###################################################################################################################################

# Linear model plural
lm_plural <- lm(Plural ~ population + participation + 
                  AG + MAN + CON + TRADE + PUB + OTHER + 
                  polity + democ + durable + gdpPerCapita, 
                data = all_countries_model_plural)

summary_lm_plural <- summary(lm_plural)
lm_plural_r2 <- summary_lm_plural$adj.r.squared
lm_plural_r2

# Linear model paid
lm_paid <- lm(Plural...paid ~ population + participation + 
                AG + MAN + CON + TRADE + PUB + OTHER + 
                polity + democ + durable + gdpPerCapita, 
              data = all_countries_model_paid)

summary_lm_paid <- summary(lm_paid)
lm_paid_r2 <- summary_lm_paid$adj.r.squared
lm_paid_r2

###################################################################################################################################
### Third pillar size (TPT) globally ###
###################################################################################################################################

# Report TPT in paper 
TPTGlobal <- mean(dfNew$Total)
TPTGlobal

###################################################################################################################################
### Third pillar size (TPT) for top 3 and bottom 3 countries ###
###################################################################################################################################

# Top and bottom 3 countries and their associated percentages
top3_TPT <- dfNew[order(-dfNew$Total, na.last = NA), ][1:3, c("Country", "Total")]
bottom3_TPT <- dfNew[order(dfNew$Total, na.last = NA), ][1:3, c("Country", "Total")]

# Print the top and bottom 3 countries (report in paper)
top3_TPT
bottom3_TPT

###################################################################################################################################
### Third pillar size (TPT) for each income group ###
###################################################################################################################################

# Calculate TPT for each income group (in paper, not as table)
income_groups <- thirdpillar %>%
  group_by(`World Bank Income Group`) %>%
  summarise(
    N = n(),
    Mean = mean(Total, na.rm = TRUE),
    SD = sd(Total, na.rm = TRUE),
    Min = min(Total, na.rm = TRUE),
    Max = max(Total, na.rm = TRUE))

# Print out TPT for each income group (report means in the paper)
income_groups

# Export dataframe as excel file
write.xlsx(income_groups, "output\\Income Groups.xlsx")

###################################################################################################################################
### Footnote 13 Regression ###
###################################################################################################################################

# Run linear model on relationship between GDP and TPT (in paper)
gdp <- lm(dfNew$Total ~ dfGraphs$gdpPerCapita, )

# Summarize the linear model and extract the adjusted R squared to report in the footnote
summary_gdp <- summary(gdp)
gdp_r2 <- summary_gdp$adj.r.squared
gdp_r2

###################################################################################################################################
### TPP Calculation ###
###################################################################################################################################

# Import statistics on volunteering 
orgvol <- read.csv("input\\FOW_TVOL_AGE_VOL_RT_A.csv")

# Select relevant variables
orgvol <- orgvol %>%
  select(ï..ref_area, classif1, classif2, time, obs_value)

# Subset dataframe to only include Total (i.e., all age groups)
orgvol <- subset(orgvol, classif1 == "AGE_AGGREGATE_TOTAL")
orgvol <- subset(orgvol, classif2 == "VOL_TYPE_ORG")

# Create wider dataframe to have each year become a column
orgvol <- pivot_wider(orgvol, names_from = time, values_from = obs_value)

# Re-order columns
orgvol <- orgvol %>%
  select(ï..ref_area, classif1, classif2, `2008`, `2010`, `2011`,
         `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`)

# Average of all years together
orgvol$ILO_org <- rowMeans(orgvol[,c(4:15)], na.rm = TRUE)

# Mapping of country names to codes
orgvol$Country[orgvol$ï..ref_area == "AUS"] <- "Australia"
orgvol$Country[orgvol$ï..ref_area == "AUT"] <- "Austria"
orgvol$Country[orgvol$ï..ref_area == "BEL"] <- "Belgium"
orgvol$Country[orgvol$ï..ref_area == "BGR"] <- "Bulgaria"
orgvol$Country[orgvol$ï..ref_area == "CAN"] <- "Canada"
orgvol$Country[orgvol$ï..ref_area == "CHE"] <- "Switzerland"
orgvol$Country[orgvol$ï..ref_area == "CYP"] <- "Cyprus"
orgvol$Country[orgvol$ï..ref_area == "CZE"] <- "Czech Republic"
orgvol$Country[orgvol$ï..ref_area == "DEU"] <- "Germany"
orgvol$Country[orgvol$ï..ref_area == "DNK"] <- "Denmark"
orgvol$Country[orgvol$ï..ref_area == "ESP"] <- "Spain"
orgvol$Country[orgvol$ï..ref_area == "EST"] <- "Estonia"
orgvol$Country[orgvol$ï..ref_area == "FIN"] <- "Finland"
orgvol$Country[orgvol$ï..ref_area == "FRA"] <- "France"
orgvol$Country[orgvol$ï..ref_area == "GBR"] <- "United Kingdom"
orgvol$Country[orgvol$ï..ref_area == "HRV"] <- "Croatia"
orgvol$Country[orgvol$ï..ref_area == "HUN"] <- "Hungary"
orgvol$Country[orgvol$ï..ref_area == "IRL"] <- "Ireland"
orgvol$Country[orgvol$ï..ref_area == "ISL"] <- "Iceland"
orgvol$Country[orgvol$ï..ref_area == "ITA"] <- "Italy"
orgvol$Country[orgvol$ï..ref_area == "LTU"] <- "Lithuania"
orgvol$Country[orgvol$ï..ref_area == "LUX"] <- "Luxembourg"
orgvol$Country[orgvol$ï..ref_area == "LVA"] <- "Latvia"
orgvol$Country[orgvol$ï..ref_area == "MKD"] <- "Macedonia"
orgvol$Country[orgvol$ï..ref_area == "MLT"] <- "Malta"
orgvol$Country[orgvol$ï..ref_area == "NLD"] <- "Netherlands"
orgvol$Country[orgvol$ï..ref_area == "NOR"] <- "Norway"
orgvol$Country[orgvol$ï..ref_area == "NZL"] <- "New Zealand"
orgvol$Country[orgvol$ï..ref_area == "POL"] <- "Poland"
orgvol$Country[orgvol$ï..ref_area == "PRT"] <- "Portugal"
orgvol$Country[orgvol$ï..ref_area == "ROU"] <- "Romania"
orgvol$Country[orgvol$ï..ref_area == "SRB"] <- "Serbia"
orgvol$Country[orgvol$ï..ref_area == "SVK"] <- "Slovak Republic"
orgvol$Country[orgvol$ï..ref_area == "SVN"] <- "Slovenia"
orgvol$Country[orgvol$ï..ref_area == "SWE"] <- "Sweden"
orgvol$Country[orgvol$ï..ref_area == "UKR"] <- "Ukraine"
orgvol$Country[orgvol$ï..ref_area == "USA"] <- "United States"
orgvol$Country[orgvol$ï..ref_area == "ZAF"] <- "South Africa"

# Select country and ILO - organization based volunteering percentages (mean of 2008 - 2020)
orgvol <- orgvol %>%
  select(Country, ILO_org)

# Add country column to all_countries
all_countries <- rownames_to_column(all_countries)
names(all_countries)[names(all_countries) == "rowname"] <- "Country"

# Create ALL dataframe with information from all_countries (input information), 
  # dfNew (predictions), country_groups (country groups and income levels), 
  # and orgvol (organization-based volunteering)
ALL <- full_join(all_countries, dfNew, by = join_by(Country))
ALL <- full_join(ALL, country_groups, by = join_by(Country))
ALL <- full_join(ALL, orgvol, by = join_by(Country))

# Create variables for Full time equivalent (Average % * population) 
  # and Volunteering (Plural - paid (prioritizing actual valies and if blank then using estimated values))
ALL$FTE <- ALL$Average / 100 * ALL$population
ALL$Volunteering <- ifelse(!is.na(ALL$Plural) & !is.na(ALL$Plural...paid), ALL$Plural - ALL$Plural...paid, ALL$Average - ALL$`Paid staff`)

# Create new data frame to include only rows where both ILO_org and 
  # Volunteering are not NA (complete data - report number of rows/countries in paper)
Ratio_calc <- ALL[!is.na(ALL$ILO_org) & !is.na(ALL$Volunteering), ]

# Calculate mean of ILO_org for new data frame
Mean_ILO_org <- mean(Ratio_calc$ILO_org, na.rm = TRUE)

# Calculate mean of Volunteering for new data frame
Mean_Volunteering <- mean(Ratio_calc$Volunteering, na.rm = TRUE)

# Calculate Ratio only for cases where both ILO_org and Volunteering have non-NA values
Ratio <- Mean_ILO_org / Mean_Volunteering

# Print ratio (report in paper)
Ratio

# Add "% of active people" and "people active as a volunteer" to ALL dataframe 
ALL$Active_percentage <- ifelse(!is.na(ALL$ILO_org), ALL$ILO_org, ifelse(ALL$Volunteering == "", "", ALL$Volunteering * Ratio))
ALL$Active_people <- ALL$Active_percentage / 100 * ALL$population

# Calculate the mean Active_percentage
Mean_Active <- mean(ALL$Active_percentage, na.rm = TRUE)

# Print the mean of Active_percentage (report in paper)
Mean_Active

###################################################################################################################################
### Top and bottom 3 of Third Pillar Participation (TPP) ###
###################################################################################################################################

# Top and bottom 3 countries and their associated percentages
top3_TPP <- ALL[order(-ALL$Active_percentage, na.last = NA), ][1:3, c("Country", "Active_percentage")]
bottom3_TPP <- ALL[order(ALL$Active_percentage, na.last = NA), ][1:3, c("Country", "Active_percentage")]

# Print the top and bottom 3 countries (report in paper)
top3_TPP
bottom3_TPP

###################################################################################################################################
###################################################################################################################################
### FINAL DATASET ###
###################################################################################################################################
###################################################################################################################################

# Export ALL to excel files in Output folder (turn all NAs blank)
ALL[is.na(ALL)] <- ""
write.xlsx(ALL, "output\\All variables_thirdpillar.xlsx")

###################################################################################################################################
### END ###
###################################################################################################################################
