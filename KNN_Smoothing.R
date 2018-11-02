# Purpose
# For Commercial Package Policies (CPP), wwe plan on building 3 sepearate models by line of business

# Liability
# Property - Group II
# Property - Group I and Special Cause of Loss (all non-Group II)
# The first step in this process was to build the modeling dataset. This information can be found in the _Input_ section below. The following steps contain the scope of this

# Creation of R dataset from SAS file (A future iteration will have CPP code generated in R)
# Exploratory Statistics
# Modeling Code
# Model Output/Validation
# Filing Support
# Input
# We will use the file, _CPP_modeling_data_, located in F:\CorpHQ\SAS_DATA_Reporting\BRE6322\CPP\Models\2018\v1.0.0\GL\data\GL\DATA_SAS The code used to generate this file, *CPP_GL_DATA.egp*, can be found in I:\Team Drives\CL Predictive Modeling\models\2018\cpp\1.0.0 In the future, we plan on replacing this code with R.

# Readme Future
# Once more code has been built out, the readme will contain more info on the order of which files need to be run.

library(kknn)		# performs k-nearest neighbor classification - https://www.rdocumentation.org/packages/kknn/versions/1.3.1/topics/kknn
library(readr)		# provides a fast and friendly way to read rectangular data (like csv, tsv, and fwf) - https://cran.r-project.org/web/packages/readr/README.html
library(data.table)	# fast aggregation of large data (100GB in RAM), fast ordered joins, and more data manipulation - https://cran.r-project.org/web/packages/data.table/index.html
library(dplyr)		# fast, consistent tool for working with data frame like objects - https://cran.r-project.org/web/packages/dplyr/index.html
library(foreign)	# reading and writing data stored by some versions of 'SAS, 'SPSS', and more - https://cran.r-project.org/web/packages/foreign/index.html
library(tidycensus)	# integrated R interface to the decennial US Census and American Community Survey APIs and the US Census Bureau's geographic boundary files - https://cran.r-project.org/web/packages/tidycensus/index.html

files = list.files("F:/CorpHQ/CHQ Product Management/Internal/Countrywide Actuarial/Personal Lines/Mapping/ZipCode/2018Q2/State")

get_all_shapes = function(x){
  read_shapes = function(rid, curr_st) {
    curr_st = curr_st[[rid]]
    tmp_shp = read.dbf(file = paste0("F:/CorpHQ/CHQ Product Management/Internal/Countrywide Actuarial/Personal Lines/Mapping/ZipCode/2018Q2/State/",
                                      curr_st,
                                      "/ZIP_CITY_COUNTY_ALL_",
                                      toupper(substr(curr_st,2,3)),
                                      ".dbf"))
  }
  rbindlist(lapply(seq_along(x), read_shapes, curr_st=files))
}

all_shapes = get_all_shapes(files)
setnames(all_shapes, make.names(tolower(names(all_shapes)), unique=TRUE))
all_shapes$enc_zip = as.numeric(levels(all_shapes$enc_zip))[all_shapes$enc_zip]
all_shapes$state = as.character(all_shapes$state)
all_shapes = all_shapes %>% select(
  enc_zip, 
  state,
  featarea,
  cent_lat,
  cent_long) %>% arrange(
    enc_zip, desc(featarea)
  ) %>% group_by(
    enc_zip
  ) %>% filter(
    row_number()==1
  )

#Read in ISO Data pull
ISO = read_csv("F:/CorpHQ/SAS_DATA_Reporting/BRE6322/CPP/Models/2018/v1.0.0/GL/inputs/RABOP_ZIP_MULTISTATE_10022018_112507_5569.csv")
setnames(ISO, make.names(tolower(names(ISO)), unique=TRUE))
ISO$zip = as.numeric(ISO$zip)

#Get population info
census_api_key(key = "894943b7e7bdba08392ae8eb360f274127f8f258")
zip_pop_estimates = get_acs(geography = "zcta", year=2016, variables = "B01003_001")
setnames(zip_pop_estimates, make.names(tolower(names(zip_pop_estimates)), unique=TRUE))
zip_pop_estimates$geoid = as.numeric(zip_pop_estimates$geoid)

#Merge Data together & Filter down to appropriate states
joined_data = all_shapes %>% 
                left_join(ISO, by = c("enc_zip" = "zip", "state"="st")) %>%
                left_join(., zip_pop_estimates, by = c("enc_zip" = "geoid")) 

df_data = joined_data %>% select(
  version, 
  zip = enc_zip, 
  state, 
  cent_lat, 
  cent_long, 
  liabilityolcs, 
  liabilityllcs, 
  population=estimate)

df_data2 = df_data[complete.cases(df_data),]
states = unique(ISO$st)
pred_data = df_data %>%
              filter(state %in% states)

#Replace NA population with 0
pred_data$population = tidyr::replace_na(pred_data$population, 0)

#Smooth and apply to data with metrics and those that lack metrics
knn_olcs = kknn(formula = liabilityolcs ~ cent_lat + cent_long,
                k = 75,
                kernel = "gaussian",
                train = df_data2,
                test = pred_data)

knn_llcs = kknn(formula = liabilityllcs ~ cent_lat + cent_long,
                k = 75,
                kernel = "gaussian",
                train = df_data2,
                test = pred_data)

#Create final dataset with desired variables
df_smooth = data.table(iso_v_pulldate = '20181002', 
                       smooth_v_date = '20181009',
                       iso_state_version = pred_data$version,
                       zip = pred_data$zip, 
                       state = pred_data$state, 
                       population = pred_data$population,
                       liabilityolcs = pred_data$liabilityolcs, 
                       liabilityolcs_s = knn_olcs$fitted.values,
                       liabilityllcs = pred_data$liabilityllcs,
                       liabilityllcs_s = knn_llcs$fitted.values)

#Cap to +-30%
df_smooth[liabilityolcs_s < liabilityolcs * .7, ]$liabilityolcs_s <- df_smooth[liabilityolcs_s < liabilityolcs * .7, ]$liabilityolcs*.7
df_smooth[liabilityolcs_s > liabilityolcs * 1.3, ]$liabilityolcs_s <- df_smooth[liabilityolcs_s > liabilityolcs * 1.3, ]$liabilityolcs*1.3
df_smooth[liabilityllcs_s < liabilityllcs * .7, ]$liabilityllcs_s <- df_smooth[liabilityllcs_s < liabilityllcs * .7, ]$liabilityllcs*.7
df_smooth[liabilityllcs_s > liabilityllcs * 1.3, ]$liabilityllcs_s <- df_smooth[liabilityllcs_s > liabilityllcs * 1.3, ]$liabilityllcs*1.3

#Create the state relativites
df_smooth_rel <- df_smooth %>%
  group_by(state) %>%
  mutate(mean_olcs_s = weighted.mean(liabilityolcs_s, population, na.rm=TRUE),
         mean_llcs_s = weighted.mean(liabilityllcs_s, population, na.rm=TRUE),
         olcs_s_rel = liabilityolcs_s/mean_olcs_s,
         llcs_s_rel = liabilityllcs_s/mean_llcs_s)