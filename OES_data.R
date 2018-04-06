library(readxl)
library(dplyr)
library(stringr)

tonum_tot_emp <- function(tot_emp)  {
  if (tot_emp == "*" || tot_emp == "*") {
    return(NULL)
  }
  else  {
    return(as.numeric(tot_emp))
  }
}

percent_change <- function(num1, num2)  {
  return((num2-num1)/num1*100)
}

##
# This section imports the Occupational Employment Statistics (OES) data for both state and national
# taken from https://www.bls.gov/oes/tables.htm
##

# import state OES data and add the year to the end

oes_2014_state <- read_excel("~/market_research/OES/state_M2014_dl.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "text", "text"))


oes_2014_state$YEAR <- "2014"


oes_2015_state <- read_excel("~/market_research/OES/state_M2015_dl.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "text", "text"))


oes_2015_state$YEAR <- "2015"

oes_2016_state <- read_excel("~/market_research/OES/state_M2016_dl.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "text", "text"))

oes_2016_state$YEAR <- "2016"

oes_2017_state <- read_excel("~/market_research/OES/state_M2017_dl.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", "text", "text"))


oes_2017_state$YEAR <- "2017"



oes_state <- bind_rows(oes_2014_state,oes_2015_state,oes_2016_state,oes_2017_state)
#View(oes_state)
oes_utah <- filter(oes_state,STATE=="Utah")

# import national Occupational Employment Statistics (OES) data and add the year to the end

oes_2014_national <- read_excel("~/market_research/OES/national_M2014_dl.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "text"))

oes_2014_national$YEAR <- "2014"


oes_2015_national <- read_excel("~/market_research/OES/national_M2015_dl.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "text"))

oes_2015_national$YEAR <- "2015"


oes_2016_national <- read_excel("~/market_research/OES/national_M2016_dl.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "text"))

oes_2016_national$YEAR <- "2016"


oes_2017_national <- read_excel("~/market_research/OES/national_M2017_dl.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "text"))

oes_2017_national$YEAR <- "2017"

oes_national <- bind_rows(oes_2014_national,oes_2015_national,oes_2016_national,oes_2017_national)
#View(oes_national)

##
# Wrangles the data for both state and national to create a comprehensive job list
# and then filters out any trade-related occupations, or any specific occupations that
# indicated.
##

# Create comprehensive occupation list from national and state data
occupations_columns <- c("OCC_CODE","OCC_TITLE")
occupation_list <- bind_rows(select(oes_state,one_of(occupations_columns)),
                             select(oes_national,one_of(occupations_columns))) %>%
  distinct() %>%
  tidyr::separate("OCC_CODE",c("major","detailed"), remove=FALSE)


# Vector with trade-related major OCC_CODES. Will be used to filter out trade-related jobs
occupations_trade_groups <- c("00","33","35","37","39","41","43","45","47","49","51","53")

# Vector with specific occupations (full OCC_CODE) that will not be considered
occupations_notConsidered <- c("11-1031","27-4011")


# Create a list of occupations that will be considered for review
occupation_list_considered <- filter(occupation_list,!(major %in% occupations_trade_groups |
                                                         OCC_CODE %in% occupations_notConsidered))

##
# Wrangles dat for state and national to get a vector of the years considered
##

years <- bind_rows(select(oes_state,matches("year")),
                   select(oes_2014_national,matches("year"))) %>%
  distinct()




##
# Examines state and national data trends for considered occupations
##

utah_employment <- oes_utah %>% filter(OCC_CODE %in% occupation_list_considered$OCC_CODE) %>%
  select(AREA,ST,STATE,OCC_CODE,OCC_TITLE,OCC_GROUP,TOT_EMP,YEAR) %>% tidyr::spread(YEAR,TOT_EMP)

# Add a variable for the percent increase each between each year
for (i in 1:(length(years$YEAR)-1)) {
  #print(paste0("PERCENT_INC_",years$YEAR[i],"-",years$YEAR[i+1]))
  utah_employment[paste0("PERCENT_INC_",years$YEAR[i],"-",years$YEAR[i+1])] <-
    mapply(percent_change, utah_employment[years$YEAR[i]], utah_employment[years$YEAR[i+1]])
  #mapply(percent_change, utah_employment$`2014`, utah_employment$`2015`)
}

# Add an average increase
utah_employment$AVG_INCREASE <- utah_employment %>% select(starts_with("PERCENT_INC")) %>% rowMeans(na.rm=TRUE)

View(utah_employment)

# Look at top fields in Utah
utah_employment %>% filter(OCC_GROUP == "major") %>% View()



#utah_employment %>%
#  filter(OCC_GROUP !="major") %>%
#  group_by (YEAR) %>%
#  top_n(20,TOT_EMP) %>%
#  select(OCC_CODE, OCC_TITLE, TOT_EMP, YEAR) %>%
#  arrange(OCC_CODE,YEAR,TOT_EMP) %>%
#  View()


national_employment <- oes_national %>% filter(OCC_CODE %in% occupation_list_considered$OCC_CODE) %>%
  select(OCC_CODE,OCC_TITLE,OCC_GROUP,TOT_EMP,YEAR) %>% tidyr::spread(YEAR,TOT_EMP)


