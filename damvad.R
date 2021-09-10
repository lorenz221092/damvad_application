##########################################################################################
# script:  damvad.R
# purpose: gain insights vom data
# project: application
# author:  lorenz
##########################################################################################
# load packages###########################################################################

# install.packages("dplyr")
# install.packages("countrycode")
# install.packages("cepiigeodist")
# install.packages("WDI")

require(dplyr)
require(cepiigeodist)
require(countrycode)
require(WDI)

##########################################################################################


# load the data set
data <- read.csv("./full202052.dat")

# subset the data frame to total (TOTAL) inter EU (I) trade
data1 <- data %>% filter(PRODUCT_NC=="TOTAL", TRADE_TYPE=="I")

# check data for redundancies
for (i in 1:2) {print(sum(data1$VALUE_IN_EUROS[data1$FLOW==i]))}

# as the sums are almost identical, one can assume that both imports and exports are 
# declared by each country in the dataset. the difference in the sums very likely stem 
# from transport margins, as no tariffs exist within the EU.

# subset the dataset to one of the two trade types for the rest of the analysis, as it
# is not clear which trade flow is which, lets arbitrarily focus on 1.
data1 <- data1 %>% filter(FLOW==1)

# check variables
unique(data1$PARTNER_ISO)

# partner countries "qy" and "gv" are very likely just residual positions -> remove
data1 <- data1 %>%  filter(PARTNER_ISO != "QV" & PARTNER_ISO !="QY")

# define a function for crating unique ids
idmaker <- function(x){ return(paste(sort(x), collapse=""))}

# create unique IDs for combinations of trading partners
data1$ID <- apply(data1[, c("PARTNER_ISO", "DECLARANT_ISO")], 1, idmaker)

# aggregate imports and exports
data1 <- data1 %>% group_by(ID) %>% 
  summarise_at(c("VALUE_IN_EUROS", "QUANTITY_IN_KG"),sum)

# check data again
View(data1)

# insight 1: 
# in absolute terms of quantity (kg) and value, most trade takes place between germany and
# the netherlands. one can expect that many goods consumed/produced in germany are first
# shipped to large ports in the netherlands and then imported to germany/exported to the 
# rest of the world.


#######


# load distances between countries from package
distances <- dist_cepii[,c("iso_o", "iso_d", "dist")]

# transform iso-3 to iso-2
distances$iso_o <- countrycode(distances$iso_o, destination = "iso2c", origin = "iso3c")
distances$iso_d <- countrycode(distances$iso_d, destination = "iso2c", origin = "iso3c")

# create ID
distances$ID <- paste0(distances$iso_d, distances$iso_o)

# merge the distances and data
data1 <- left_join(data1, distances)

# estimate an oversimplified gravity model: trade value between two countries is 
# determined by the distance between them. We expect this relationship to be negative.
summary (lm(log(VALUE_IN_EUROS) ~ log(dist), data1))

# insight 2:
# the hypothesis holds. increasing distance by one percent decreases trade by 1.6 percent
# on average. this simple model very likely suffers from omitted variable bias as 
# size of the resprective economies and other variables might be important in determining 
# the amount of trade.


##################end of script###########################################################