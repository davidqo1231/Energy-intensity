library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)
library(data.table)
library(lubridate)
library(sjmisc)
library(stringr)

library(ggiraph)
library(ggtext)

library(sysfonts)
library(showtext)

library(htmlwidgets)

# List of countries
countries <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN",
               "FRA", "GBR", "GRC", "HRV", "HUN", "CHE", "CHN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", 
               "LTU", "LUX", "LVA", "MEX", "MLT", "NOR", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", "SWE", 
               "TUR", "TWN", "USA")

# Loading data - Electricity and heat consumption in TJ by industry in 2014
df_electricity <- readr::read_csv("https://raw.githubusercontent.com/davidqo1231/Energy-intensity/main/Data/Electricity_data.csv") %>%
  select(-"...1")

industry_names <- df_electricity %>%
  select(industry) 

# Loading data - Input-output tables (2014), World input-output database
io_datasets <- list()
tot_prod <- list()
for(i in countries){
  path <- paste("https://raw.githubusercontent.com/davidqo1231/Energy-intensity/main/Data/NIOT/", i, "_NIOT_nov16.csv", sep = "")
  io_datasets[[i]] <- readr::read_csv2(path)
  tot_prod[[i]] <- transpose(io_datasets[[i]] %>% slice(1801))
}

# Total production by country and industry
tot_prod_noblank <- apply(as.data.table(tot_prod), 2, str_remove_all, " ")  

df_tot_prod <- as.data.table(tot_prod_noblank) %>%
  slice(5:60) %>%
  mutate_all(., function(x) as.numeric(as.character(x))) %>%
  bind_cols(industry_names) %>%
  relocate(industry)

# Energy intensity of industries - calculations
df_tot_prod[df_tot_prod == 0] <- 0.001
tot_prod_mat <- df_tot_prod %>%
  select(-industry) %>%
  as.matrix()

electricity_mat <- df_electricity %>%
  select(-industry) %>%
  as.matrix()

el_intensity <- electricity_mat / tot_prod_mat

# Finalny data frame, priama narocnost na elektrinu po odvetviach a po krajinach
df_elintensity <- as.data.frame(el_intensity) %>%
  bind_cols(industry_names) %>%
  relocate(industry)


## Visualization ##

#set font for later (uses sysfonts and showtext)
font_add_google("jost", "jost")
showtext_auto()

# EU countries
EU_countries <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "POL", "PRT", "ROU", "SVK", "SVN", "SWE") 

# Select industries for plot
#fig_industries <- c("vA01", "vB", "vC10_12", "vC13_15", "vC17", "vC18", "vC19", "vC20", "vC21", "vC22", "vC23", "vC24", "vC25", "vC26", "vC27", "vC28", "vC29", "vC30", "vC31_32", "vC33")
#Five most energy intensive industries (based on median values)
fig_industries <- c("vB", "vC23", "vC24", "vC20", "vC17")

# Industries full names
industry_names_full <- as.data.table(io_datasets[["AUS"]][["Description"]]) %>%
  slice(2:57) %>% 
  rename(industry_name = V1)

df_plot <- df_elintensity %>%
  bind_cols(industry_names_full) %>%
  relocate(industry_name) %>%
  gather(country, value, -c(industry, industry_name)) %>%
  relocate(country) %>%
  mutate(EU = ifelse(country %in% EU_countries, "EU", "non-EU")) %>%
  filter(country != "RUS") %>%
  group_by(industry, EU) %>%
  mutate(EU_avg = mean(value, na.rm=TRUE),
         EU_avg = replace(EU_avg, EU != "EU", NA),
         EU_median = median(value, na.rm = TRUE),
         EU_median = replace(EU_median, EU != "EU", NA)) %>%
  filter(industry  %in% fig_industries) %>%
  mutate(SVK_val = value, 
         SVK_val = replace(value, country != "SVK", NA),
         value = replace(value, country == "SVK", NA),
         DEU_val = value, 
         DEU_val = replace(value, country != "DEU", NA),
         value = replace(value, country == "DEU", NA),
         FRA_val = value, 
         FRA_val = replace(value, country != "FRA", NA),
         value = replace(value, country == "FRA", NA),
         CZE_val = value, 
         CZE_val = replace(value, country != "CZE", NA),
         value = replace(value, country == "CZE", NA)) %>%
  filter(EU == "EU")

# PLOT
pal<-c(
  'grey', #blue
  '#F2CA6D', #yellow
  '#E85477', #losos
  '#0C1D2B', #tmavomodra
  '#6535F2' #fialova
)

#geom_tile_interactive(aes(tooltip=tooltip, data_id=id, onclick=onclick),colour="white", width=.9, height=.9)+
  

plot <- ggplot(df_plot, aes(x = reorder(industry_name, desc(EU_avg)), y = value)) +
  geom_jitter_interactive(aes(color = "EU countries", tooltip = country, data_id = country), size =3, alpha = 0.8, width = 0.15) +
  geom_point_interactive(aes(x=industry_name, y=SVK_val, color = "Slovakia", tooltip = country, data_id = country), size=4, alpha = 0.8)+
  geom_point_interactive(aes(x=industry_name, y=DEU_val, color = "Germany", tooltip = country, data_id = country), size=4, alpha = 0.8)+
  geom_point_interactive(aes(x=industry_name, y=CZE_val, color = "Czechia", tooltip = country, data_id = country),  size=4, alpha = 0.8)+
  geom_point_interactive(aes(x=industry_name, y=EU_median, tooltip = country, data_id = country), shape = 4, color="#0C1D2B", size=6)+
  
  scale_y_continuous(limits = c(0, 6)) +
  #scale_fill_manual(values=pal)+
  #scale_color_manual(values=pal)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  
  coord_flip() +
  
  xlab("") + 
  ylab("TJ of electricity needed to produce 1 mil. USD of output") +
  
  scale_color_manual(breaks=c('EU countries', 'Slovakia', 'Germany', "Czechia"),
                     values=c('EU countries'='grey', 'Slovakia'='#0C1D2B', 'Germany'='#E85477', "Czechia"= "#6535F2")) +
  
  theme_minimal()+
  theme(text=element_text(family = "jost", size = 10, color = "#555555"),
        legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x = element_text(size = 10),
        panel.grid.major.x = element_line(size = 0.15, colour = "grey"),
        panel.grid.minor.x = element_line(size = 0.10, colour = "grey"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F9F5EB", color = "#F9F5EB"),
        plot.background = element_rect(fill = "#F9F5EB", color ="#F9F5EB"),
        plot.title = element_text(hjust=-0.4, size=20, vjust=0, color="#555555"),
        plot.subtitle = element_markdown(hjust=1.7, halign = 0, size=10, color="#827f7f", margin=margin(t=5, b=5)),
        plot.caption = element_text(size=8, hjust=0.95, margin=margin(t=15))) +
  
        labs(caption="Data from World Input-Output Database | Chart by David Hojdan",
             subtitle="Manufacturing of basic metals needed the most electric energy to make products of 1 mil. USD. Slovakia has the most energy intensive <br>
             manufacturing of basic metals mainly due to large aluminium production plant that consume 10% of the country's electricity production. <br>
             These industries may be significantly affected due to the recent increase in electricity prices in Europe.",
             title="HARD TIMES FOR METALS", fill="")

interactive<-girafe(ggobj=plot,  
                    options = list(opts_hover(css = "fill:#ffffff;cursor:pointer;")),
                    width_svg=9, height_svg=5.25)
#Preview
interactive
  
#save plot as HTML file
saveWidget(interactive, "intensity_plot.html", selfcontained = T)



