install.packages("tidyverse")
install.packages("writexl")
install.packages("modelsummary")
install.packages("fixest")

library(tidyverse)
library(writexl)
library(modelsummary)
library(fixest)

gravity<- read_csv("Gravity_V202211.csv")
key<-read_csv("country_codes_V202501.csv")

## create a country list
origin_ctr <- c("India")  # India as exporter
dest_ctr <- c("Brunei", "Cambodia", "Indonesia", "Lao PDR", 
              "Malaysia", "Myanmar", "Philippines", 
              "Singapore", "Thailand", "Vietnam")  # ASEAN as importers

## choose only variables we need

## Choose variables (replace tradeflow_baci with the correct one)
vrb <- c("iso3num_o","iso3num_d","year","iso3_o", "iso3_d", 
         "distw_harmonic", "contig", "comcol", "comlang_off",
         "gdp_o","gdp_d","gdpcap_o","gdpcap_d","fta_wto",
         "tradeflow_comtrade_d")  # Used tradeflow_comtrade_d instead of tradeflow_baci

## 2000 to 2023

gravity2<-gravity |>
  filter(year >= 2000 & year <= 2023) |> # 1. Keep years 2000-2023
  filter(country_id_o!="IND.1") |> 
  filter(country_id_d!="IND.1") |> 

#gravity2 <- gravity2 |> 
  #filter(country_id_o %in% key_origin$country_iso3 &    # India as origin
           #country_id_d %in% key_dest$country_iso3)       # ASEAN as destination

  gravity2 <- gravity |> 
  filter(country_id_o %in% key_origin$country_iso3 &    # India as origin
           country_id_d %in% key_dest$country_iso3) |>    # ASEAN as destination
  select(all_of(vrb))
  
## Keep countries in the list
#key_origin <- key |> filter(country_name %in% origin_ctr)
#key_dest <- key |> filter(country_name %in% dest_ctr)


#gravity2_clean <- gravity2 |>
  #filter(!is.na(tradeflow_comtrade_d) & tradeflow_comtrade_d >= 0)


#for country pair fixed effects
dest_iso3 <- c("BRN", "KHM", "IDN", "LAO", "MYS", 
               "MMR", "PHL", "SGP", "THA", "VNM")

gravity2 <- gravity |>
  filter(iso3_o == "IND") |>                  # India as exporter
  filter(iso3_d %in% dest_iso3)              # ASEAN as importers

gravity2 <- gravity |>
  filter(year >= 2000 & year <= 2023) |>     # Years 2000-2023
  filter(country_id_o != "IND.1") |>         # Remove colonial Indonesia
  filter(country_id_d != "IND.1") |>
  filter(iso3_o == "IND") |>                 # India as exporter
  filter(iso3_d %in% dest_iso3) |>          # All 10 ASEAN as importers
  select(all_of(vrb))

gravity2 <- gravity2 |>
  mutate(
    lgdpd = log(gdp_d),                       # ASEAN GDP
    lgdpcd = log(gdpcap_d),                   # ASEAN GDP per capita
    country_pair = paste(iso3_o, iso3_d, sep = "_")  # Country pair identifier
  )

#print("Country pairs:")
#print(unique(gravity2$country_pair))

gravity2_clean <- gravity2 |>
  filter(!is.na(tradeflow_comtrade_d) & tradeflow_comtrade_d >= 0) |>
  filter(!is.na(lgdpd) & !is.na(lgdpcd))

## PPML with Country Pair + Year Fixed Effects
model_country_pair <- fepois(tradeflow_comtrade_d ~ lgdpd + lgdpcd + fta_wto | 
                               country_pair + year, 
                             data = gravity2_clean)

summary(model_country_pair)

## Make a log versin
gravity2<-gravity2 |>
  mutate(ldist=log(distw_harmonic),
         lgdpo=log(gdp_o),
         lgdpd=log(gdp_d),
         lgdpco=log(gdpcap_o),
         lgdpcd=log(gdpcap_d),
         logtrade=log(1+tradeflow_comtrade_d))
         

summary(gravity2)

#Main model

model3 <- feols(logtrade ~ lgdpd + lgdpcd + ldist + contig + 
                  comcol + comlang_off + fta_wto | year, 
                data = gravity2)
model31 <- feols(data=gravity2,logtrade ~ lgdpd + lgdpcd + ldist + contig + 
                  comcol + comlang_off + fta_wto)
                

model_ppml <- fepois(tradeflow_comtrade_d ~ lgdpd + lgdpcd + ldist + 
                       contig + comcol + comlang_off + fta_wto | year, 
                     data = gravity2)
summary(model3)
summary(model_ppml)
summary(model31)
# Side-by-side comparison
modelsummary(list("Log-Linear" = model3, "PPML" = model_ppml , "PPML FULL" = model_ppml_full , "Country-pair" = model_country_pair))


#importer and expoerter year fixed effect 


gravity2 <- gravity2 |>
  mutate(exporter_year = paste(iso3_o, year, sep = "_"),
         importer_year = paste(iso3_d, year, sep = "_"))

model_ppml_mrt <- fepois(tradeflow_comtrade_d ~fta_wto | 
                                  importer_year, 
                                data = gravity2)
                         
model_ppml_full <- fepois(tradeflow_comtrade_d ~ lgdpd + lgdpcd + ldist + 
                            contig + comcol + comlang_off + fta_wto | 
                            iso3_d + year, 
                          data = gravity2)

summary(model_ppml_full)





