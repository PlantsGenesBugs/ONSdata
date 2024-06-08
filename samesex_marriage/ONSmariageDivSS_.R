
# source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/vitalstatisticspopulationandhealthreferencetables
# can't compare rates heterosexual/homosexual (rates calculated from total married pop;
# same-sex marriage a small proportion so rate disproportionately small)

# Marriages of same sex couples have been possible in England and Wales since 29 March 2014, in Scotland since 31 December 2014 and in Northern Ireland since 13 January 2020 
# sheet5 (England and Wales - NA in 2020-21; Scotland full; Ireland NA 2014-2019)
# Marriages involving couples who converted an existing civil partnership into a marriage are not included

# estim population 2021 (ONS data): England 56,536,000;  Scotland 5,480,000; Wales 3,105,000; NI 1,905,000
estim_rel_pop_size <- data.frame(country = c("England", "Scotland", "Wales", "Northern Ireland"),
                                 pop = c(56536000, 5480000, 3105000, 1905000))
estim_rel_pop_size <- estim_rel_pop_size |>
  mutate(estim_rel_pop = (pop/estim_rel_pop_size$pop[estim_rel_pop_size$country == 'England'])*100)


library(readxl)  # for reading in data
library(tidyverse)  # for tidying data
library(extrafont)  # for creating own theme
library(ggpubr)  # to use image as background
library(jpeg)  # to read in jpeg image
library(gridGraphics)  # to plot two graphs over each other
library(janitor)


# set palette according to background image to be used and imagecolorpicker.com/en
red <- "#fb0404"
purple <- "#8d058c"


## Single-sex marriage
ss_marriage <- read_excel("annualreferencetables2021.xlsx", sheet="5")

# remove informational rows and promote header row to column names
ss_marriage <- ss_marriage[-c(1:4),] |>
  row_to_names(row_number=1)

# remove ":" using na_if() and then convert numbers to numeric
ss_marriage <- ss_marriage |>
  mutate(across(where(is.character), ~na_if(., ":"))) |>
  mutate(across(where(is.character), ~na_if(., "z"))) |>
  mutate(across(where(is.character), as.numeric)) 


# pull into tidy data format
ss_marriage <- ss_marriage |> pivot_longer(!Year) |>
  separate_wider_delim(name, ":", names=c("country", "count"))

# remove white space around text
ss_marriage <- ss_marriage |>
  mutate(count = str_squish(count))

# filter to years where same sex marriage was leaally allowed; remove rates and combined country rows
ss_marriage |>
  filter(Year > 2013,
         country %in% c("England", "Northern Ireland", "Scotland", "Wales"),
         count != "Total") |>
  ggplot(aes(x = Year, y = value, colour=count)) +
  geom_point(shape = 18, size=3) +
  geom_line() +
  scale_colour_manual(values = c(red, purple)) +
  ggtitle("Same-sex marriage") +
  ylab("Count") +
  theme(
    plot.background = element_blank()
  ) +
  facet_wrap(~country, scales="free") -> marriage_plot

# more female ss marriages than male
# more marriages in England than any other country, followed by Scotland, Wales and Northern Ireland (order of population size as well)



## Single-sex civil partnerships; transform data as above
ss_civil <- read_excel("annualreferencetables2021.xlsx", sheet="6")
ss_civil <- ss_civil[-c(1:4),] |>
  row_to_names(row_number=1)

ss_civil <- ss_civil |>
  mutate(across(where(is.character), ~na_if(., ":"))) |>
  mutate(across(where(is.character), ~na_if(., "z"))) |>
  mutate(across(where(is.character), as.numeric)) 

ss_civil <- ss_civil |> pivot_longer(!Year) |>
  separate_wider_delim(name, ":", names=c("country", "count"))

ss_civil <- ss_civil |>
  mutate(count = str_squish(count))

# draw graph excluding years with no data
ss_civil |>
  filter(Year > 2000,
         country %in% c("England", "Northern Ireland", "Scotland", "Wales"),
         count != "Total") |>
  ggplot(aes(x = Year, y = value, colour = count)) +
  geom_point(shape = 18, size=3) +
  geom_line() +
  scale_colour_manual(values = c(red, purple)) +
  ggtitle("Same-sex civil partnership")+
  ylab("Count") +
  theme(
    plot.background = element_blank()
  ) +
  facet_wrap(~country, scales="free") -> civil_plot

# strong uptick from 2005 to 2006 due to fact that Civil Partnership Act 2004 came into force on 5 December 2005 (allowing
# same-sex couples to obtain legal recognition of their relationship)
# In general civil partnerships decrease in 2014 due to the passing of the Marriage (Same Sex Couples) Act in 2013. 
# First SS marriage 29 March 2014. Same sex marriage only legal in NI since 13 January 2020.
# Civil partnerships equally popular between female and male couples in England and NI (in contrast to marriage), some diff 
# in Wales and Scotland.



## Single Sex couple divorce
ss_divorce <- read_excel("annualreferencetables2021.xlsx", sheet="9")
ss_divorce <- ss_divorce[-c(1:4),] |>
  row_to_names(row_number=1) |>
  dplyr::select(1:7)

ss_divorce <- ss_divorce |>
  mutate(across(where(is.character), ~na_if(., ":"))) |>
  mutate(across(where(is.character), ~na_if(., "z"))) |>
  mutate(across(where(is.character), as.numeric)) 

ss_divorce <- ss_divorce |> pivot_longer(!Year) |>
  separate_wider_delim(name, ":", names=c("country", "count"))

ss_divorce <- ss_divorce |>
  mutate(count = str_squish(count))

ss_divorce |>
  filter(Year > 2013,
         count != "Total") |>
  ggplot(aes(x = Year, y = value, colour = count)) +
  geom_point(shape = 18, size=3) +
  geom_line() +
  scale_colour_manual(values = c(red, purple)) +
  ggtitle("Same-sex divorce") +
  ylab("Count") +
  theme(
    plot.background = element_blank()
  ) +
  facet_wrap(~country, scales="free") -> divorce_plot

# limited divorce data; in England & Wales it has gone up (in line with number of marriages/partnerships??)
# wedding totals in England and Wales aren't reflected by individual country's numbers (see ONS notes), so 
# can't really calculate rate of divorce based on number of marriages


# create and apply own theme for final graphing
theme_new <- function(base_size = 13,
                      base_family = "Gill Sans",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.9)),
      axis.text = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(0.8)),
      panel.background = element_rect(fill = 'white'),
      panel.grid.major = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted"),   
      panel.grid.minor = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted", 
        size = rel(4)), 
      legend.title=element_blank(),
      
      complete = TRUE
    )
}

# add theme_new() to each plot
mtn <- marriage_plot + 
  theme_new(base_size = 16) 
ctn <- civil_plot +
  theme_new(base_size = 16)
dtn <- divorce_plot +
  theme_new(base_size = 16)


# prepare background plot for background of facetted graphs
# read in image
gilbert1_alpha2 <- readJPEG("Gilbert-1-alpha2.jpg")


# Basic setup for background plot
p_background <- ggplot() +
  theme_void() +
  background_image(gilbert1_alpha2)


# use gridGraphics functions to overlay
# mtn
p_background
print(mtn,
      vp = viewport(width = 0.95,
                    height = 0.95))
grid.newpage() 

#ctn
p_background
print(ctn,
      vp = viewport(width = 0.95,
                    height = 0.95))
grid.newpage()

#dtn
p_background
print(dtn,
      vp = viewport(width = 0.95,
                    height = 0.95))

# export from RStudio interface as .png









