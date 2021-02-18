library(tidyverse)
library(RPostgreSQL)
library(sf)
library(viridis)
library(cowplot)
library(extrafont)

# Read in some data ------------------------------------------------------------
conn = dbConnect("PostgreSQL",dbname='pablo') 
pu_sf = st_read(conn, "pu_by_country") #read from pg table
pu = pu_sf %>%
  select(gid, adm0_a3) %>%
  st_set_geometry(NULL) %>%
  as_tibble() %>%
  mutate(gid=as.integer(gid))

sp = read_csv("tabular_data/pablo_birds.csv",col_types = 'iiiii')
lan_Count  = read_csv("tabular_data/Official_Languages_by_Country.csv", col_types = cols(adm0_a3='c',Country='c',.default = 'i')) # call table of languages by country

# Distinct PU/sisid
sp_unique = sp %>%
  distinct(gid,sisid)

#Species richness per PU
sp_rich = sp %>%
  group_by(gid) %>% 
  summarise(n_sp = n_distinct(sisid))
#write_csv(sp_rich,"tabular_data/sp_rich.csv")

# Number of countries per species
sp_country = sp %>%
  left_join(pu, by = 'gid') %>%
  group_by(sisid) %>%
  summarise(n_cnt = n_distinct(adm0_a3))

#Languages by sisid/country
sp_unique_lang = sp_unique %>%
  left_join(pu, by='gid') %>%
  distinct(adm0_a3, sisid) %>%
  arrange(sisid) %>%
  left_join(lan_Count, by = 'adm0_a3') %>%
  pivot_longer(cols = -c(sisid, adm0_a3, Country),
               names_to = 'lang') %>%
  filter(value != 0)

# Count of  distinct languages by sisid
sp_unique_lang_count = sp_unique_lang %>%
  group_by(sisid) %>%
  summarise(n_langs=n_distinct(lang))

# Count distinct number of languages per PU
pu_lang_count = sp_unique_lang %>%
  right_join(pu, by = 'adm0_a3') %>%
  distinct(lang,gid) %>%
  group_by(gid) %>%
  summarise(n_lang=n_distinct(lang))
                              
# Mean languages per species that intersect with PU 
pu_avg_lang_count_by_sp = sp_unique %>%
  left_join(sp_unique_lang_count, by = 'sisid') %>%
  group_by(gid) %>%
  summarise(avg_lang=mean(n_langs))
#write_csv(pu_avg_lang_count_by_sp, "tabular_data/pu_avg.csv")

# Merge needed attributes all together -----------------------------------------
df = pu_sf %>%
  left_join(sp_rich,by = 'gid') %>%
  left_join(pu_avg_lang_count_by_sp, by = 'gid') %>%
  select(gid,adm0_a3,n_sp,avg_lang) %>% 
  replace_na(list(n_sp=0, avg_lang=0))

write_sf(df, "pu_data.gpkg") # save output to geopackage

# Create Bivariate Map ---------------------------------------------------------
default_font_family = 'Open Sans'
default_font_color = '#484242'
default_background_color = '#F5F5F2'
default_caption = 'I am a map.'

theme_map = function(...) {
  theme_minimal() +
  theme(
    text = element_text(family = default_font_family,
                        color = default_font_color),
    # remove all axes
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # add a subtle grid
    panel.grid.major = element_line(color = "white", size = 0.05),
    panel.grid.minor = element_blank(),
    # background colors
    plot.background = element_rect(fill = default_background_color,
                                   color = NA),
    panel.background = element_rect(fill = default_background_color,
                                    color = NA),
    legend.background = element_rect(fill = alpha(default_background_color,1),
                                     color = NA),
    # borders and margins
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    panel.border = element_blank(),
    panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
    # titles
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9, hjust = 0,
                               color = default_font_color),
    plot.title = element_text(size = 15, hjust = 0.5,
                              color = default_font_color),
    plot.subtitle = element_text(size = 10, hjust = 0.5,
                                 color = default_font_color,
                                 margin = margin(b = -0.1,
                                                 t = -0.1,
                                                 l = 2,
                                                 unit = "cm"),
                                 debug = F),
    # captions
    plot.caption = element_text(size = 7,
                                hjust = .5,
                                margin = margin(t = 0.2,
                                                b = 0,
                                                unit = "cm"),
                                color = "#939184"),
    ...
  )
}

# define number of classes
no_classes = 4

# create 4 buckets for species richness
quantiles_sp = df %>%
  pull(n_sp) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1))

# create 4 buckets for languages
quantiles_lang = df %>%
  pull(avg_lang) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1))

# create color scale that encodes two variables; purples for species and green
# for languages - the special notation with gather is for readibility reasons
bivariate_color_scale = tibble(
  "4 - 4" = "#80004F", # high richness, high language
  "3 - 4" = "#5D297C",
  "2 - 4" = "#266ABD", 
  "1 - 4" = "#0195EF", # low richness, high language
  "4 - 3" = "#A4413C", # high richness, mid-high language
  "3 - 3" = "#835D6C",
  "2 - 3" = "#5C7DA6", 
  "1 - 3" = "#37A1DF", # low richness, mid-high language
  "4 - 2" = "#DA9C2B", # high richness, mid-low language
  "3 - 2" = "#C2A756",
  "2 - 2" = "#9DA993", 
  "1 - 2" = "#87B3CC", # low richness, mid-low language
  "4 - 1" = "#FAE407", # high richness, low language
  "3 - 1" = "#EBDA46", 
  "2 - 1" = "#D4CB7C",
  "1 - 1" = "#C2C1BF" # low richness, low langauge
) %>%
  gather("group", "fill")

# cut into groups defined above and join fill
df = df %>%
  mutate(
    sp_quantiles = cut(
      n_sp,
      breaks = quantiles_sp,
      include.lowest = TRUE
    ),
    lang_quantiles = cut(
      avg_lang,
      breaks = quantiles_lang,
      include.lowest = TRUE
    ),
    # paste the factors together as numbers to match the groups defined in the
    # tibble bivariate_color_scale
    group = paste(
      as.numeric(sp_quantiles), "-",
      as.numeric(lang_quantiles)
    )
  ) %>%
  # join the actual hex values per "group" so each PU knows its hex value
  # based on the sp and lang value
  left_join(bivariate_color_scale, by = "group")

map = ggplot(
  # use the same dataset as before
  data = df
  ) +
  geom_sf(
    aes(
      fill = fill
    ),
    color = NA, # No lines on PU boundaries
    size = 0.1
  ) +
  # as the sf object df has a column with name "fill" that
  # contains the literal color as hex code for each PU, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # add titles
  labs(x = NULL,
         y = NULL,
         title = "Avian Richness vs. Languages Spoken",
         subtitle = "Birdlife Species Range and Average (Official) Spoken Languages per Species' Range",
         caption = default_caption) +
  # add the theme
  theme_map()

# separate the groups
bivariate_color_scale  = bivariate_color_scale %>% 
  separate(group, into = c("sp", "lang"), sep = " - ") %>%
  mutate(sp = as.integer(sp),
         lang = as.integer(lang))

legend = ggplot() +
  geom_text() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = sp,
      y = lang,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Species Richness ⟶️",
       y = "Languages Spoken ⟶️") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8)
  ) +
  # quadratic tiles
  coord_fixed()

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.0001, 0.1, 0.35, 0.35)

ggsave("figures/birds_alt_fig1.png", dpi = 600)

#Trim off any unwanted margins
system("convert figures/birds_alt_fig1.png -trim figures/birds_alt_fig1_trim.png")
