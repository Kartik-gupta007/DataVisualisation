# Pre-Processing ----------------------------------------------------------

library(tidyverse)

x <- read_csv("/Users/kartikgupta/Downloads/P_Data_Extract_From_World_Development_Indicators (1)/owid.csv")

install.packages("WDI")
library(WDI)

new_wdi_cache <- WDIcache()

y <- WDI(country=c("IND", "USA","CHN","PAK","IDN","BRA","BGD","NGA","RUS","MEX"),
         indicator = c("SP.DYN.IMRT.IN","SP.DYN.IMRT.FE.IN","SP.DYN.IMRT.MA.IN",
                       "SH.DYN.NMRT","SH.DYN.MORT","SH.DTH.IMRT","SH.DTH.NMRT","SH.DTH.MORT","SP.DYN.LE00.IN","SP.DYN.CBRT.IN",
                       "NY.GNP.MKTP.PP.CD" , "NY.GDP.MKTP.CD" , "NY.GDP.PCAP.CD" , "SH.IMM.IDPT" , "SH.IMM.MEAS" ,
                       "SH.IMM.HEPB" , "SP.POP.TOTL" ),
         start = 1997 , end = 2020,
         extra=TRUE,
         cache = new_wdi_cache)
new_wdi_cache <- WDIcache()

x <- WDI(country= "all",
         indicator = c("SP.DYN.IMRT.IN","SP.DYN.IMRT.FE.IN","SP.DYN.IMRT.MA.IN",
                       "SH.DYN.NMRT","SH.DYN.MORT","SH.DTH.IMRT","SH.DTH.NMRT","SH.DTH.MORT","SP.DYN.LE00.IN","SP.DYN.CBRT.IN",
                       "NY.GNP.MKTP.PP.CD" , "NY.GDP.MKTP.CD" , "NY.GDP.PCAP.CD" , "SH.IMM.IDPT" , "SH.IMM.MEAS" ,
                       "SH.IMM.HEPB" , "SP.POP.TOTL"),
         start = 1997 , end = 2020,
         extra=TRUE,
         cache = new_wdi_cache)
new_wdi_cache <- WDIcache()


options(scipen=999)


#########################################MAP#####################################

world_map <- map_data("world")
countryDataWDI <- x %>% mutate(country=recode(str_trim(country),
                                              "United States"="USA",
                                              "United Kingdom"="UK",
                                              "Congo, Dem. Rep."="Democratic Republic of the Congo",
                                              "Congo, Rep."="Republic of Congo",
                                              "Kyrgyz Republic"="Kyrgyzstan",
                                              "Egypt, Arab Rep."="Egypt",
                                              "Russian Federation"="Russia",
                                              "Iran, Islamic Rep."="Iran",
                                              "Venezuela, RB"="Venezuela",
                                              "Yemen, Rep."="Yemen",
                                              "Turkiye"="Turkey",
                                              "Czechia"="Czech Republic",
                                              "Slovak Republic"="Slovakia",
                                              "Cote d'Ivoire"="Ivory Coast",
                                              "Lao PDR"="Laos",
                                              "Korea, Dem. People's Rep."="North Korea",
                                              "Korea, Rep."="South Korea"))

countryDataWDIMap <- left_join(world_map,countryDataWDI,by = c("region"="country"))
ggplot(filter(countryDataWDIMap, year=="2020"),
       aes(long,lat,group=group))+
  geom_polygon(aes(fill= (SP.DYN.IMRT.IN + SH.DYN.NMRT + SH.DYN.MORT)/3),colour="white")+
  scale_fill_viridis_c(option = "H" , limits = c(0,100), breaks = c(0,25,50,75,100), 
                       guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)) +
  theme_void()+
  labs(fill=" Percentage of Mortality Rate\n", title="World map coloured by Child & Infant Mortality in 2020",
       caption="Data source: World Development Indicators") +
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 14),
        legend.title = element_text(colour = "black" , size = 16 , face = "italic" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ),
        legend.position = "bottom",
        legend.key.width = unit(10,"lines"))





############################SCATTERPLOT+LINE#####################################

ggplot(filter(x,region!="Aggregates" & region!="NA"),  
       aes(x = (SP.DYN.IMRT.IN + SH.DYN.NMRT + SH.DYN.MORT)/3 ,y=NY.GNP.MKTP.PP.CD/SP.POP.TOTL)) +
  geom_point(aes(  color=region), alpha= 0.8  ) + 
  labs(colour = "Country \n", title = "Comparing Effect of Child & Infant Mortality on National Income\n" , 
       x = "Infant Mortality Rate\n" , y = "Gross National Income Per Capita" , size = "Infant Deaths" , caption="Data source: World Development Indicators") + 
  scale_y_continuous(trans = "log") +
  geom_smooth(method=lm, se = TRUE , ) +
  theme_linedraw() + 
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 14),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ))



# geom_smooth(method=lm, se = FALSE)



#####################################HeatMap##############################

library(reshape)
yy <- data.frame(country = y$country,
                 year = y$year,
                 Mortality_Rate_Infant = y$SP.DYN.IMRT.IN,
                 Mortality_Rate_Neonatal = y$SH.DYN.NMRT,
                 Mortality_Rate_Under5 = y$SH.DYN.MORT,
                 Life_Expectancy = y$SP.DYN.LE00.IN,
                 Birth_Rate = y$SP.DYN.CBRT.IN,
                 Immunization_DPT = y$SH.IMM.IDPT , 
                 Immunization_Measles = y$SH.IMM.MEAS ,
                 Immunization_HepB3 = y$SH.IMM.HEPB )
yy <- filter(yy, 
             year==2010)
yy = subset(yy, select = -c(year) )

yy<-melt(yy)

ggplot(yy,aes(y = country, x = reorder(variable, value), fill = value)) + geom_tile() +
  scale_fill_viridis_c( option = "H" , limits = c(0,150), breaks = c(0,30, 60, 90, 120, 150), 
                        guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)) + 
  theme_linedraw() + 
  labs( title = "Analyses of Top 10 Populated Countries with all the Robust Determinants in 2010" , 
        x = "Determinants" , y = "Country" , fill = "Scale" , caption="Data source: World Development Indicators") +
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 14),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(color = "black", size = 9, face = "bold"),
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ),
        legend.key.height = unit(7 , "lines"))

#################################BubbleGraph#########################################
library(hrbrthemes)
library(viridis)

ggplot (filter(y))+
  geom_point(aes(x = year, y =SH.DYN.NMRT, colour = country, size =SH.DTH.NMRT ),alpha = 0.8 )+
  scale_size(range = c(1, 20), breaks = c(200000,400000,600000,800000,1000000)) +
  theme_linedraw() +
  theme(legend.position="right") +
  ylab("Mortality Rate") +
  xlab("Year") +
  labs(fill=" Percentage of \nNeonatal Mortality Rate", title= "Overview of Neonatal Mortality From 1997 to 2020\n",
         colour = "Country" , size = "Number of \nNeonatal Deaths" , caption="Data source: World Development Indicators")+
  scale_fill_viridis_d(aesthetics ="colour", option = "H") +
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 14),
        axis.title.x.bottom = element_text (color = "black", size = 14, face = "bold", hjust = 0.5),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ))
###############################RADAR/SPIDER ------------------------------------------------------------
install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)

library(ggradar)
library(scales)
library(tidyverse)



xyz11 <- data.frame(country = y$country,
                  year = y$year,
                  
                  Life_Expectancy = y$SP.DYN.LE00.IN,
                  Birth_Rate = y$SP.DYN.CBRT.IN,
                  Mortality_Rate_Female = y$SP.DYN.IMRT.FE.IN,
                  Mortality_Rate_Male = y$SP.DYN.IMRT.MA.IN,
                  Mortality_Rate_Infant = y$SP.DYN.IMRT.IN,
                  Immunization = (y$SH.IMM.HEPB + y$SH.IMM.IDPT + y$SH.IMM.MEAS)/3 )

xyz11 <- filter(xyz11, (country=="India" | country=="China" | 
                      country=="United States" | country=="Indonesia") &
                year==2020)


xyz11<-xyz11[xyz11$country!="Aggregates",]
xyz11<-na.omit(xyz11)
xyz11 = subset(xyz11 , select = -c(year) )

xyz11 %>% mutate_if(is.numeric, rescale) %>%
  mutate(new_country=str_replace_all(country, " ", "_")) %>%
  group_by(new_country) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 5,
          legend.text.size = 10,
          plot.title = "         Comparing Key Factors: Developed Vs Developing Nations       ",
          legend.title = "Country",
          legend.position = "bottom",
          values.radar = c("Low","Avg","High"))






#New RADAR/Spider

#install.packages("fmsb")
library(fmsb)

xyz <- data.frame(country = xy$country,
                  year = xy$year,
                  Mortality_Rate_Infant = xy$SP.DYN.IMRT.IN,
                  Mortality_Rate_Neonatal = xy$SH.DYN.NMRT,
                  Mortality_Rate_Under5 = xy$SH.DYN.MORT,
                  Life_Expectancy = xy$SP.DYN.LE00.IN,
                  Birth_Rate = xy$SP.DYN.CBRT.IN)

xyz <- filter(xyz, (country=="India" | country=="United States" | 
                      country=="China" | country=="Nigeria") &
                year==2020)
xyz<-na.omit(xyz)
xyz <- xyz %>% 
  mutate(new_country=str_replace_all(country, " ", "_")) %>%
  group_by(new_country) %>%
  summarise_if(is.numeric, mean)
xyz = subset(xyz , select = -c(year) )

max_min <- data.frame(new_country = c("Max","Min"), 
                      Mortality_Rate_Infant = c(max(xyz$Mortality_Rate_Infant),min(xyz$Mortality_Rate_Infant)), 
                      Mortality_Rate_Neonatal = c(max(xyz$Mortality_Rate_Neonatal),min(xyz$Mortality_Rate_Neonatal)), 
                      Mortality_Rate_Under5 = c(max(xyz$Mortality_Rate_Under5),min(xyz$Mortality_Rate_Under5)),
                      Life_Expectancy = c(max(xyz$Life_Expectancy),min(xyz$Life_Expectancy)), 
                      Birth_Rate = c(max(xyz$Birth_Rate),min(xyz$Birth_Rate)))
xyz <- rbind(max_min, xyz)
xyz <- data.frame(xyz, row.names = 1)

areas <- c(rgb(1, 0, 0, 0.5),
           rgb(0, 1, 0, 0.46),
           rgb(0, 0, 1, 0.43),
           rgb(0, 1, 1, 0.40))

radarchart(xyz,
           cglty = 1,       # Grid line type
           cglcol = "black",# Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas)   # Color of the areas   
title(main = "Energy Production from Different Sources")
legend(x=1.4, y=0.5, title = "Country", title.adj = 0.1, title.cex = 1,
       legend = rownames(xyz[-c(1,2),]), bty = "n", pch = 20, col = areas,
       text.col = "grey25", cex = 1, pt.cex = 2.5) 









####################################Lollipop############
library(dplyr)
library(forecast)

y %>%
  arrange(SP.DYN.CBRT.IN) %>%
  
  ggplot( aes(x=country, y=SP.DYN.CBRT.IN)) +
  geom_segment( aes(xend=country, yend=0 , color = income )) +
  geom_point( aes(color=income , size = SP.DYN.LE00.IN)) +
  theme_bw() +
  labs( title = "Comparing Effect of Birth Rate \nwith Country Income Status" , 
        x = "Year" , y = "Birth Rate\n" , color = "Financial Status")


y %>%
  arrange(SP.DYN.LE00.IN) %>%
  
  ggplot( aes(x=country, y=SP.DYN.LE00.IN)) +
  geom_segment( aes(xend=country, yend=0 , color = income )) +
  geom_point( aes(color=income , size = SP.DYN.CBRT.IN)) +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  labs( title = "Comparing Effect of Life Expectancy and Birth rate \nwith Income Status" , 
        x = "Year" , y = "Birth Rate\n" , color = "Financial Status" , size = "Birth Rate")



xy %>%
  mutate(country = fct_reorder(country,SP.DYN.CBRT.IN )) %>%
  ggplot( aes(x=country, y=SP.DYN.CBRT.IN)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()



#extra

library(ggplot2)
library(lubridate)
theme_set(theme_bw())

df <- economics[, c("date", "psavert", "uempmed")]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]

# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)

# plot
ggplot(y, aes(x=year)) + 
  geom_area(aes(y=SP.DYN.IMRT.FE.IN, fill="SP.DYN.IMRT.IN")) + 
  geom_area(aes(y=SP.DYN.IMRT.MA.IN, fill="SP.DYN.IMRT.IN")) + 
  labs(title="Area Chart of Returns Percentage", 
       subtitle="From Wide Data format", 
       caption="Source: Economics", 
       y="Returns %")   # title and caption

scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  scale_fill_manual(name="", 
                    values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid

###############

xyz11<-na.omit(xyz11)
xyz11 <- xyz11 %>% 
  mutate(new_country=str_replace_all(country, " ", "_")) %>%
  group_by(new_country) %>%
  summarise_if(is.numeric, mean)
xyz11 = subset(xyz11 , select = -c(year) )

max_min <- data.frame(new_country = c("Max","Min"), 
                      
                      Life_Expectancy = c(max(xyz11$Life_Expectancy),min(xyz11$Life_Expectancy)),
                      Birth_Rate = c(max(xyz11$Birth_Rate),min(xyz11$Birth_Rate)),
                      Mortality_Rate_Female = c(max(xyz11$Mortality_Rate_Female),min(xyz11$Mortality_Rate_Female)),
                      Mortality_Rate_Male = c(max(xyz11$Mortality_Rate_Male),min(xyz11$Mortality_Rate_Male)),
                      Mortality_Rate_Infant = c(max(xyz11$Mortality_Rate_Infant),min(xyz11$Mortality_Rate_Infant)),
                      Immunization = c(max(xyz11$Immunization),min(xyz11$Immunization)))

xyz11 <- rbind(max_min, xyz11)
xyz11 <- data.frame(xyz11, row.names = 1)

areas <- c(rgb(0, 0, 0, 0.5),
           rgb(0, 1, 1, 0.40),
           rgb(0, 0, 1, 0.43),
           rgb(1, 1, 0, 0.46))

radarchart(xyz11,
           cglty = 1,       # Grid line type
           cglcol = "black",# Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas)   # Color of the areas   
title(main = "Spider")
legend(x=1.4, y=0.5, title = "Country", title.adj = 0.1, title.cex = 1,
       legend = rownames(xyz11[-c(1,2),]), bty = "n", pch = 20, col = areas,
       text.col = "grey25", cex = 1, pt.cex = 2.5) 








###########################SPIDER(NOT USED) ------------------------------------------------------------------

install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)

library(ggradar)
library(scales)
library(tidyverse)

xy <- WDI(country=c("IND", "USA","CHN","PAK","IDN","BRA","BGD","NGA","RUS","MEX"),
          indicator = c("SP.DYN.IMRT.IN",
                        "SH.DYN.NMRT",
                        "SH.DYN.MORT",
                        "SP.DYN.LE00.IN",
                        "SP.DYN.CBRT.IN"
          ),
          start = 2020 , end = 2020,
          extra=TRUE,
          cache = new_wdi_cache)

xy<-xy[xy$country!="Aggregates",]
xy<-na.omit(xy)



xy %>% mutate_if(is.numeric, rescale) %>%
  mutate(new_country=str_replace_all(country, " ", "_")) %>%
  group_by(new_country) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 3,
          legend.text.size = 8)

