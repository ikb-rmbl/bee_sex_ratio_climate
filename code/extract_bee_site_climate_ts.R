#### Script to extract 
#### climate data at Becky Irwin's bee phenology sites from the RMBL spatial data platform.
#### Author: Ian Breckheimer
#### Updated: 23 January 2023

#### Sets up workspace.

#install.packages(c("devtools","sf","terra"))
#devtools::install_github("rmbl-sdp/rSDP") #Installs the development version of the rSDP package.
library(sf)
library(terra)
library(rSDP)
library(tidyr)
library(dplyr)
library(ggplot2)

##Loads data for coordinates of field sites.
sites <- read.csv("./data/site_info.csv")
sites_sv <- vect(sites,geom=c("long", "lat"), crs="EPSG:4326")

##Gets information about available climate and snow products.
sdp_cat <- sdp_get_catalog(domains="UG", 
                           types=c("Snow","Climate"),
                           deprecated=FALSE,
                           return_stac=FALSE)

## Gets the appropriate Catalog ID for each product.
snow_onset_catID <- sdp_cat$CatalogID[sdp_cat$Product=="Snow Onset Day of Year Yearly Timeseries"]
snow_persist_catID <- sdp_cat$CatalogID[sdp_cat$Product=="Snow Persistence Day of Year Yearly Timeseries"]
tmax_catID <- sdp_cat$CatalogID[sdp_cat$Product=="Maximum 2m Air Temperature Daily Timeseries"]
tmin_catID <- sdp_cat$CatalogID[sdp_cat$Product=="Minimum 2m Air Temperature Daily Timeseries"]

## Grabs snow products for the appropriate years.
bee_years <- 2006:2022
snow_onset_rast <- sdp_get_raster(catalog_id = snow_onset_catID,
                                  years=bee_years)
snow_persist_rast <- sdp_get_raster(catalog_id = snow_persist_catID,
                                    years=bee_years)

##Reprojects site locations to the same coordinate system as the raster.
sites_utm <- project(sites_sv,snow_persist_rast)
#plot(snow_onset_rast[[1]])
#plot(sites_utm,add=TRUE)

##Extracts snow data at the site locations.
sites_snow_persist <- sdp_extract_data(snow_persist_rast,sites_utm,
                                       return_spatvector = FALSE, bind=FALSE)
sites_snow_persist$Dataset <- "SnowPersistDOY"
sites_snow_persist$Site <- sites_utm$site

sites_snow_onset <- sdp_extract_data(snow_onset_rast,sites_utm,
                                     return_spatvector = FALSE, bind=FALSE)
sites_snow_onset$Dataset <- "SnowOnsetDOY"
sites_snow_onset$Site <- sites_utm$site

##Reshapes data to long format and binds datasets together.
sites_snow_persist_long <- pivot_longer(sites_snow_persist,
                                        cols=num_range(prefix="",range=bee_years),
                                        names_to="Year")
sites_snow_onset_long <- pivot_longer(sites_snow_onset,
                                        cols=num_range(prefix="",range=bee_years),
                                        names_to="Year")
sites_snow_long <- rbind(sites_snow_persist_long,sites_snow_onset_long)
sites_snow_wide <- pivot_wider(sites_snow_long,names_from="Dataset",values_from="value")
sites_snow_wide$SnowLengthDays <-  sites_snow_wide$SnowPersistDOY - sites_snow_wide$SnowOnsetDOY

##Rescales snow onset to calendar year and DOY.
SnowOnsetDOY_df <- data.frame(Site=sites_snow_wide$Site,
                              SnowOnsetDOY_calendar=sites_snow_wide$SnowOnsetDOY + 365,
                              Year=as.character(as.numeric(sites_snow_wide$Year) - 1))
sites_snow_wide <- left_join(sites_snow_wide,SnowOnsetDOY_df,by=c("Site","Year"))

## Writes snow data to disk.
write.csv(sites_snow_wide,"./output/bee_sites_snow_2006_2022.csv",row.names=FALSE)

## Joins to site metadata.
sites_snow_wide_meta <- left_join(sites_snow_wide,sites,by=c("Site"="site"))

## Makes a quick sanity plot.
ggplot(sites_snow_wide_meta)+
  geom_point(aes(x=elevation_m,y=SnowPersistDOY,color=as.factor(Year)),shape="+")+
  geom_smooth(aes(x=elevation_m,y=SnowPersistDOY,color=as.factor(Year)),se=FALSE)+
  geom_point(aes(x=elevation_m,y=SnowOnsetDOY,color=as.factor(Year)),shape=".")+
  geom_smooth(aes(x=elevation_m,y=SnowOnsetDOY,color=as.factor(Year)),se=FALSE)+
  theme_bw()

#### Extracts daily time-series (can take a few hours)
bee_dates <- seq(as.Date("2006-01-01"),as.Date("2022-10-30"),by="day")
bee_years <- format(bee_dates,format="%Y")
unique_years <- unique(bee_years)

extracted_temps <- list()

for (i in 1:length(unique_years)){
  print(paste("Extracting data for year",unique_years[i]))
  start_date <- min(bee_dates[which(bee_years==unique_years[i])])
  end_date <- max(bee_dates[which(bee_years==unique_years[i])])
  
  ##Builds a remote raster dataset
  tmax_year_rast <- sdp_get_raster(tmax_catID,date_start=start_date,date_end=end_date)
  tmin_year_rast <- sdp_get_raster(tmin_catID,date_start=start_date,date_end=end_date)
  
  ##Samples datasets.
  tmax_sites_year <- sdp_extract_data(tmax_year_rast,sites_utm,
                                      return_spatvector = FALSE, bind=FALSE)
  tmin_sites_year <- sdp_extract_data(tmin_year_rast,sites_utm,
                                      return_spatvector = FALSE, bind=FALSE)
  ##Populates other metadata.
  tmax_sites_year$Dataset <- "Tmax"
  tmax_sites_year$Site <- sites_utm$site
  tmin_sites_year$Dataset <- "Tmin"
  tmin_sites_year$Site <- sites_utm$site
  
  ##Converts to long format.
  tmin_sites_year_long <- pivot_longer(tmin_sites_year,
                                       cols=-c(ID,Dataset,Site),
                                       names_to="Date")
  tmax_sites_year_long <- pivot_longer(tmax_sites_year,
                                       cols=-c(ID,Dataset,Site),
                                       names_to="Date")
  
  ##Binds output together.
  temp_sites_year <- rbind(tmax_sites_year_long,tmin_sites_year_long)
  temp_sites_year_wide <- pivot_wider(temp_sites_year,names_from="Dataset",values_from="value")
  extracted_temps[[i]] <- temp_sites_year_wide
}
extracted_temp_df <- bind_rows(extracted_temps)
write.csv(extracted_temp_df,file="./output/bee_sites_temp_2006_2022.csv",row.names=FALSE)


##Computes weekly quantiles.
extracted_temp_df$Date <- as.Date(extracted_temp_df$Date)
extracted_temp_df$DOY <- as.numeric(format(extracted_temp_df$Date,format="%j"))
extracted_temp_df$Year <- format(extracted_temp_df$Date,format="%Y")

ggplot(extracted_temp_df)+
  geom_line(aes(x=DOY,y=Tmax,color=Year),alpha=0.5)+
  geom_line(aes(x=DOY,y=Tmin,color=Year),alpha=0.5)+
  geom_abline(aes(intercept=0,slope=0),linetype="dotted")+
  #scale_x_date(limits=as.Date(c("2007-01-01","2007-12-31")))+
  facet_wrap(facets=~Site)+
  theme_bw()

## Computes weekly quantiles.
extracted_temp_df$Week <- format(extracted_temp_df$Date,format="%W")

weekly_sum <- extracted_temp_df %>% group_by(Site,Week) %>% 
  summarise(Tmin_quant_min=min(Tmin),
            Tmin_quant_025=quantile(Tmin,probs=c(0.025)),
            Tmin_quant_10=quantile(Tmin,probs=c(0.1)),
            Tmin_quant_25=quantile(Tmin,probs=c(0.25)),
            Tmin_quant_50=quantile(Tmin,probs=c(0.5)),
            Tmin_quant_75=quantile(Tmin,probs=c(0.75)),
            Tmin_quant_90=quantile(Tmin,probs=c(0.9)),
            Tmin_quant_975=quantile(Tmin,probs=c(0.975)),
            Tmin_quant_max=max(Tmin),
            Tmax_quant_min=min(Tmax),
            Tmax_quant_025=quantile(Tmax,probs=c(0.025)),
            Tmax_quant_10=quantile(Tmax,probs=c(0.1)),
            Tmax_quant_25=quantile(Tmax,probs=c(0.25)),
            Tmax_quant_50=quantile(Tmax,probs=c(0.5)),
            Tmax_quant_75=quantile(Tmax,probs=c(0.75)),
            Tmax_quant_90=quantile(Tmax,probs=c(0.9)),
            Tmax_quant_975=quantile(Tmax,probs=c(0.975)),
            Tmax_quant_max=max(Tmax))

extracted_temp_df <- left_join(extracted_temp_df,weekly_sum,by=c("Site","Week"))

##Joins temp and snow data.
extracted_temp_df <- left_join(extracted_temp_df,sites_snow_wide,by=c("Site","Year"))

##Creates plots for every year.
for(i in 1:length(unique_years)){
  
  print(paste("Plotting year", unique_years[i]))
  
  p1 <- ggplot(filter(extracted_temp_df,Year == unique_years[i]))+
    labs(title=paste("Air Temperature",unique_years[i]),y="Air Temp. (C)")+
    geom_abline(aes(intercept=0,slope=0),linetype="dotted")+
    geom_segment(aes(x=))
    geom_ribbon(aes(x=DOY,ymin=Tmax_quant_min,ymax=Tmax_quant_max),fill="darkred",alpha=0.1)+
    geom_ribbon(aes(x=DOY,ymin=Tmax_quant_10,ymax=Tmax_quant_90),fill="darkred",alpha=0.25)+
    #geom_ribbon(aes(x=DOY,ymin=Tmax_quant_25,ymax=Tmax_quant_75),fill="darkred",alpha=0.5)+
    #geom_line(aes(x=DOY,y=Tmax_quant_50),color="darkred",alpha=0.25)+
    geom_ribbon(aes(x=DOY,ymin=Tmin_quant_min,ymax=Tmin_quant_max),fill="darkblue",alpha=0.1)+
    geom_ribbon(aes(x=DOY,ymin=Tmin_quant_10,ymax=Tmin_quant_90),fill="darkblue",alpha=0.25)+
    #geom_ribbon(aes(x=DOY,ymin=Tmin_quant_25,ymax=Tmin_quant_75),fill="darkblue",alpha=0.5)+
    #geom_line(aes(x=DOY,y=Tmin_quant_50),color="darkblue",alpha=0.25)+
    geom_line(aes(x=DOY,y=Tmax,linetype=Year),color="darkred")+
    geom_line(aes(x=DOY,y=Tmin,linetype=Year),color="darkblue")+
    facet_wrap(facets=~Site)+
    theme_bw()+
    theme(legend.position="none")
  
  plot_filename <- paste0("./figs/air_temp_daily_allsites_year_",unique_years[i],".pdf")
  pdf(plot_filename,width=12,height=10)
  print(p1)
  dev.off()
}

  
  

