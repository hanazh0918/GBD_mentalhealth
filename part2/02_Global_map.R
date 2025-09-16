# =============================================================================
# Title: Global map — Percent Change (PC) in Prevalent Cases (Number) for CD, 1990–2021
# Purpose:
#   - Compute percent change between 1990 and 2021 for prevalence (Number) in <20y, Both sex
#   - Harmonize country names to match maps::map_data('world')
#   - Draw a world choropleth with regional insets
#
# Inputs:
#   - conduct_204: data.frame with columns {location, year, age, sex, metric, measure, val, upper, lower}
#       * age: "<20 years"; sex: "Both"
#       * metric == "Number"; measure == "Prevalence"
#
# Output objects:
#   - f2_a1: ggplot object (main map + insets)
#
# Notes:
#   - PC is computed as (value_2021 - value_1990) / value_1990, i.e., a proportion in [-1, +∞).
#   - If you want to show “percent” instead of “proportion”, multiply by 100 and adjust breaks/labels.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(maps)       # for map_data('world')
  library(patchwork)
})

## ----- 2021 snapshot: prevalence (Number)
data <- conduct_204
num_2021 <- data %>%
  dplyr::filter(
    year == 2021, age == "<20 years", sex == "Both",
    metric == "Number", measure == "Prevalence"
  ) %>%
  dplyr::select(location, val, upper, lower) %>%
  dplyr::mutate(
    val_2021   = val,
    upper_2021 = upper,
    lower_2021 = lower,
    v_2021_show = paste0(val_2021, " (", lower_2021, ", ", upper_2021, ")")
  ) %>%
  dplyr::select(location, val_2021, upper_2021, lower_2021, v_2021_show)

## ----- 1990 snapshot: prevalence (Number)
data <- conduct_204
num_1990 <- data %>%
  dplyr::filter(
    year == 1990, age == "<20 years", sex == "Both",
    metric == "Number", measure == "Prevalence"
  ) %>%
  dplyr::select(location, val, upper, lower) %>%
  dplyr::mutate(
    val_1990   = val,
    upper_1990 = upper,
    lower_1990 = lower,
    v_1990_show = paste0(val_1990, " (", lower_1990, ", ", upper_1990, ")")
  ) %>%
  dplyr::select(location, val_1990, upper_1990, lower_1990, v_1990_show)

## ----- Merge & compute percent change (proportion)
data <- dplyr::left_join(num_1990, num_2021, by = "location") %>%
  dplyr::mutate(
    dplyr::across(c(val_1990, upper_1990, lower_1990,
                    val_2021, upper_2021, lower_2021), as.numeric),
    pc_v   = (val_2021   - val_1990)  / val_1990,
    pc_u   = (upper_2021 - upper_1990)/ upper_1990,
    pc_l   = (lower_2021 - lower_1990)/ lower_1990,
    pc_show = paste0(pc_v, " (", pc_l, ", ", pc_u, ")")
  )

## use pc_v as mapping value
country_asr <- data
country_asr$val <- country_asr$pc_v

## ----- Base map data
worldData <- map_data("world") %>%
  dplyr::filter(region != "Antarctica")

## (Optional) quick consistency checks
asr_locations  <- unique(country_asr$location)
world_regions  <- unique(worldData$region)
common_countries <- intersect(asr_locations, world_regions)
unique_asr    <- setdiff(asr_locations, world_regions)
unique_world  <- setdiff(world_regions, asr_locations)

## ----- Harmonize location names to map regions
country_asr$location <- as.character(country_asr$location)

# Simple one-to-one recodes
country_asr$location[country_asr$location == "Türkiye"] <- "Turkey"
country_asr$location[country_asr$location == "Czechia"] <- "Czech Republic"
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
country_asr$location[country_asr$location == "Principality of Andorra"] <- "Andorra"
country_asr$location[country_asr$location == "Republic of the Union of Myanmar"] <- "Myanmar"
country_asr$location[country_asr$location == "Kingdom of Belgium"] <- "Belgium"
country_asr$location[country_asr$location == "Russian Federation"] <- "Russia"
country_asr$location[country_asr$location == "Republic of Paraguay"] <- "Paraguay"
country_asr$location[country_asr$location == "Islamic Republic of Afghanistan"] <- "Afghanistan"
country_asr$location[country_asr$location == "Federative Republic of Brazil"] <- "Brazil"
country_asr$location[country_asr$location == "Republic of Italy"] <- "Italy"
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] <- "Bolivia"
country_asr$location[country_asr$location == "Kingdom of Bhutan"] <- "Bhutan"
country_asr$location[country_asr$location == "Grand Duchy of Luxembourg"] <- "Luxembourg"
country_asr$location[country_asr$location == "State of Libya"] <- "Libya"
country_asr$location[country_asr$location == "Republic of the Philippines"] <- "Philippines"
country_asr$location[country_asr$location == "Republic of Vanuatu"] <- "Vanuatu"
country_asr$location[country_asr$location == "Togolese Republic"] <- "Togo"
country_asr$location[country_asr$location == "Eastern Republic of Uruguay"] <- "Uruguay"
country_asr$location[country_asr$location == "French Republic"] <- "France"
country_asr$location[country_asr$location == "People's Republic of Bangladesh"] <- "Bangladesh"
country_asr$location[country_asr$location == "Republic of Tajikistan"] <- "Tajikistan"
country_asr$location[country_asr$location == "Republic of Ecuador"] <- "Ecuador"
country_asr$location[country_asr$location == "Republic of Korea"] <- "South Korea"
country_asr$location[country_asr$location == "Commonwealth of Dominica"] <- "Dominica"
country_asr$location[country_asr$location == "Republic of Poland"] <- "Poland"
country_asr$location[country_asr$location == "Brunei Darussalam"] <- "Brunei"
country_asr$location[country_asr$location == "Kingdom of Cambodia"] <- "Cambodia"
country_asr$location[country_asr$location == "Antigua and Barbuda"] <- "Antigua"
country_asr$location[country_asr$location == "Republic of Uzbekistan"] <- "Uzbekistan"
country_asr$location[country_asr$location == "Republic of Chile"] <- "Chile"
country_asr$location[country_asr$location == "Republic of Austria"] <- "Austria"
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] <- "North Korea"
country_asr$location[country_asr$location == "Lebanese Republic"] <- "Lebanon"
country_asr$location[country_asr$location == "Kingdom of Sweden"] <- "Sweden"
country_asr$location[country_asr$location == "Republic of Colombia"] <- "Colombia"
country_asr$location[country_asr$location == "Republic of Honduras"] <- "Honduras"
country_asr$location[country_asr$location == "People's Republic of China"] <- "China"
country_asr$location[country_asr$location == "Republic of Armenia"] <- "Armenia"
country_asr$location[country_asr$location == "Republic of Singapore"] <- "Singapore"
country_asr$location[country_asr$location == "Republic of Zimbabwe"] <- "Zimbabwe"
country_asr$location[country_asr$location == "Independent State of Samoa"] <- "Samoa"
country_asr$location[country_asr$location == "Kingdom of Bahrain"] <- "Bahrain"
country_asr$location[country_asr$location == "Republic of Albania"] <- "Albania"
country_asr$location[country_asr$location == "Republic of Belarus"] <- "Belarus"
country_asr$location[country_asr$location == "Republic of Mozambique"] <- "Mozambique"
country_asr$location[country_asr$location == "Republic of Bulgaria"] <- "Bulgaria"
country_asr$location[country_asr$location == "Republic of Peru"] <- "Peru"
country_asr$location[country_asr$location == "Republic of Fiji"] <- "Fiji"
country_asr$location[country_asr$location == "Republic of Malta"] <- "Malta"
country_asr$location[country_asr$location == "Gabonese Republic"] <- "Gabon"
country_asr$location[country_asr$location == "Republic of Guinea"] <- "Guinea"
country_asr$location[country_asr$location == "Republic of Liberia"] <- "Liberia"
country_asr$location[country_asr$location == "Republic of Sudan"] <- "Sudan"
country_asr$location[country_asr$location == "Kingdom of Morocco"] <- "Morocco"
country_asr$location[country_asr$location == "Federal Republic of Germany"] <- "Germany"
country_asr$location[country_asr$location == "Republic of Nicaragua"] <- "Nicaragua"
country_asr$location[country_asr$location == "Kingdom of Denmark"] <- "Denmark"
country_asr$location[country_asr$location == "Republic of Guyana"] <- "Guyana"
country_asr$location[country_asr$location == "Eswatini"] <- "Swaziland"
country_asr$location[country_asr$location == "Republic of Guinea-Bissau"] <- "Guinea-Bissau"
country_asr$location[country_asr$location == "Republic of Serbia"] <- "Serbia"
country_asr$location[country_asr$location == "Republic of Iraq"] <- "Iraq"
country_asr$location[country_asr$location == "Slovak Republic"] <- "Slovakia"
country_asr$location[country_asr$location == "Kingdom of Tonga"] <- "Tonga"
country_asr$location[country_asr$location == "Republic of Costa Rica"] <- "Costa Rica"
country_asr$location[country_asr$location == "Republic of Rwanda"] <- "Rwanda"
country_asr$location[country_asr$location == "Republic of Cyprus"] <- "Cyprus"
country_asr$location[country_asr$location == "Kingdom of Norway"] <- "Norway"
country_asr$location[country_asr$location == "Republic of Equatorial Guinea"] <- "Equatorial Guinea"
country_asr$location[country_asr$location == "Republic of Iceland"] <- "Iceland"
country_asr$location[country_asr$location == "Sultanate of Oman"] <- "Oman"
country_asr$location[country_asr$location == "Republic of Guatemala"] <- "Guatemala"
country_asr$location[country_asr$location == "Kingdom of the Netherlands"] <- "Netherlands"
country_asr$location[country_asr$location == "Commonwealth of the Bahamas"] <- "Bahamas"
country_asr$location[country_asr$location == "Congo"] <- "Republic of Congo"
country_asr$location[country_asr$location == "Republic of Maldives"] <- "Maldives"
country_asr$location[country_asr$location == "Republic of Mauritius"] <- "Mauritius"
country_asr$location[country_asr$location == "Democratic Republic of Timor-Leste"] <- "Timor-Leste"
country_asr$location[country_asr$location == "United States of America"] <- "USA"
country_asr$location[country_asr$location == "Taiwan (Province of China)"] <- "Taiwan"
country_asr$location[country_asr$location == "Democratic Socialist Republic of Sri Lanka"] <- "Sri Lanka"
country_asr$location[country_asr$location == "Republic of South Africa"] <- "South Africa"
country_asr$location[country_asr$location == "Republic of Slovenia"] <- "Slovenia"
country_asr$location[country_asr$location == "Republic of Malawi"] <- "Malawi"
country_asr$location[country_asr$location == "Republic of Turkey"] <- "Turkey"
country_asr$location[country_asr$location == "Republic of Haiti"] <- "Haiti"
country_asr$location[country_asr$location == "Republic of Croatia"] <- "Croatia"
country_asr$location[country_asr$location == "Republic of Azerbaijan"] <- "Azerbaijan"
country_asr$location[country_asr$location == "State of Kuwait"] <- "Kuwait"
country_asr$location[country_asr$location == "Republic of Yemen"] <- "Yemen"
country_asr$location[country_asr$location == "Republic of Estonia"] <- "Estonia"
country_asr$location[country_asr$location == "Kingdom of Thailand"] <- "Thailand"
country_asr$location[country_asr$location == "United States Virgin Islands"] <- "Virgin Islands"
country_asr$location[country_asr$location == "Viet Nam"] <- "Vietnam"
country_asr$location[country_asr$location == "Kingdom of Spain"] <- "Spain"
country_asr$location[country_asr$location == "Republic of Finland"] <- "Finland"
country_asr$location[country_asr$location == "Portuguese Republic"] <- "Portugal"
country_asr$location[country_asr$location == "Republic of Madagascar"] <- "Madagascar"
country_asr$location[country_asr$location == "Republic of India"] <- "India"
country_asr$location[country_asr$location == "Republic of Seychelles"] <- "Seychelles"
country_asr$location[country_asr$location == "Republic of Mali"] <- "Mali"
country_asr$location[country_asr$location == "Republic of Namibia"] <- "Namibia"
country_asr$location[country_asr$location == "Principality of Monaco"] <- "Monaco"
country_asr$location[country_asr$location == "Swiss Confederation"] <- "Switzerland"
country_asr$location[country_asr$location == "Hellenic Republic"] <- "Greece"
country_asr$location[country_asr$location == "State of Qatar"] <- "Qatar"
country_asr$location[country_asr$location == "Kingdom of Saudi Arabia"] <- "Saudi Arabia"
country_asr$location[country_asr$location == "Republic of San Marino"] <- "San Marino"
country_asr$location[country_asr$location == "Republic of Kiribati"] <- "Kiribati"
country_asr$location[country_asr$location == "Federal Democratic Republic of Ethiopia"] <- "Ethiopia"
country_asr$location[country_asr$location == "State of Israel"] <- "Israel"
country_asr$location[country_asr$location == "Republic of Suriname"] <- "Suriname"  # fix
country_asr$location[country_asr$location == "United Republic of Tanzania"] <- "Tanzania"
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] <- "Laos"
country_asr$location[country_asr$location == "Republic of Ghana"] <- "Ghana"
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] <- "Iran"
country_asr$location[country_asr$location == "Republic of Indonesia"] <- "Indonesia"
country_asr$location[country_asr$location == "United Mexican States"] <- "Mexico"
country_asr$location[country_asr$location == "Republic of Nauru"] <- "Nauru"
country_asr$location[country_asr$location == "Republic of Tunisia"] <- "Tunisia"
country_asr$location[country_asr$location == "Federal Democratic Republic of Nepal"] <- "Nepal"
country_asr$location[country_asr$location == "Islamic Republic of Pakistan"] <- "Pakistan"
country_asr$location[country_asr$location == "Republic of Senegal"] <- "Senegal"
country_asr$location[country_asr$location == "Islamic Republic of Mauritania"] <- "Mauritania"
country_asr$location[country_asr$location == "Syrian Arab Republic"] <- "Syria"
country_asr$location[country_asr$location == "Independent State of Papua New Guinea"] <- "Papua New Guinea"
country_asr$location[country_asr$location == "Republic of the Marshall Islands"] <- "Marshall Islands"
country_asr$location[country_asr$location == "Union of the Comoros"] <- "Comoros"
country_asr$location[country_asr$location == "Kyrgyz Republic"] <- "Kyrgyzstan"
country_asr$location[country_asr$location == "People's Democratic Republic of Algeria"] <- "Algeria"
country_asr$location[country_asr$location == "Micronesia (Federated States of)"] <- "Micronesia"
country_asr$location[country_asr$location == "Hashemite Kingdom of Jordan"] <- "Jordan"
country_asr$location[country_asr$location == "Republic of Lithuania"] <- "Lithuania"
country_asr$location[country_asr$location == "Republic of Chad"] <- "Chad"
country_asr$location[country_asr$location == "Republic of Zambia"] <- "Zambia"
country_asr$location[country_asr$location == "Republic of South Sudan"] <- "South Sudan"
country_asr$location[country_asr$location == "Republic of Latvia"] <- "Latvia"
country_asr$location[country_asr$location == "Republic of the Niger"] <- "Niger"
country_asr$location[country_asr$location == "Argentine Republic"] <- "Argentina"
country_asr$location[country_asr$location == "Republic of Kazakhstan"] <- "Kazakhstan"
country_asr$location[country_asr$location == "Republic of Niue"] <- "Niue"
country_asr$location[country_asr$location == "Republic of El Salvador"] <- "El Salvador"
country_asr$location[country_asr$location == "Republic of Benin"] <- "Benin"
country_asr$location[country_asr$location == "Republic of Burundi"] <- "Burundi"
country_asr$location[country_asr$location == "Federal Republic of Somalia"] <- "Somalia"
country_asr$location[country_asr$location == "Cabo Verde"] <- "Cape Verde"
country_asr$location[country_asr$location == "Republic of Cuba"] <- "Cuba"
country_asr$location[country_asr$location == "State of Eritrea"] <- "Eritrea"
country_asr$location[country_asr$location == "Republic of Palau"] <- "Palau"
country_asr$location[country_asr$location == "Arab Republic of Egypt"] <- "Egypt"
country_asr$location[country_asr$location == "Republic of Djibouti"] <- "Djibouti"
country_asr$location[country_asr$location == "Republic of Cameroon"] <- "Cameroon"
country_asr$location[country_asr$location == "Republic of Uganda"] <- "Uganda"
country_asr$location[country_asr$location == "Republic of Moldova"] <- "Moldova"
country_asr$location[country_asr$location == "Democratic Republic of Sao Tome and Principe"] <- "Sao Tome and Principe"
country_asr$location[country_asr$location == "Republic of Angola"] <- "Angola"
country_asr$location[country_asr$location == "Republic of Botswana"] <- "Botswana"
country_asr$location[country_asr$location == "Republic of Panama"] <- "Panama"
country_asr$location[country_asr$location == "Federal Republic of Nigeria"] <- "Nigeria"
country_asr$location[country_asr$location == "Republic of Kenya"] <- "Kenya"
country_asr$location[country_asr$location == "Côte d'Ivoire"] <- "Ivory Coast"
country_asr$location[country_asr$location == "Kingdom of Lesotho"] <- "Lesotho"
country_asr$location[country_asr$location == "Republic of the Gambia"] <- "Gambia"
country_asr$location[country_asr$location == "Republic of Sierra Leone"] <- "Sierra Leone"

## Split combined entities to match map polygons (duplication)
country_asr$location[country_asr$location == "United Kingdom"] <- "UK"
country_asr$location[country_asr$location == "Trinidad and Tobago"] <- "Trinidad"
extra <- country_asr[country_asr$location == "Trinidad", ]; extra$location <- "Tobago"; country_asr <- rbind(country_asr, extra)
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] <- "Saint Kitts"
extra <- country_asr[country_asr$location == "Saint Kitts", ]; extra$location <- "Nevis"; country_asr <- rbind(country_asr, extra)
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] <- "Saint Vincent"
extra <- country_asr[country_asr$location == "Saint Vincent", ]; extra$location <- "Grenadines"; country_asr <- rbind(country_asr, extra)

## ----- Join map + data
total <- dplyr::full_join(worldData, country_asr, by = c("region" = "location")) %>%
  dplyr::filter(!is.na(val))

## Binning (proportions). If you switch to percent, also change breaks/labels accordingly.
total <- total %>%
  dplyr::mutate(
    val2 = cut(
      val,
      breaks = c(-1.0, -0.25, 0, 0.25, 0.75, 1, Inf),
      labels = c("-1.00–-0.25", "-0.25–0", "0–0.25", "0.25–0.75", "0.75–1.00", ">1.00"),
      include.lowest = TRUE, right = TRUE
    )
  )

## ----- Main map
p1 <- total %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = val2),
               colour = "black", size = 0.5) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues") +
  guides(fill = guide_legend(
    title = "PC in prevalent cases\nfor CD (1990–2021)", ncol = 2
  )) +
  labs(title = "(A) Conduct Disorder (PC, 1990–2021)", x = "", y = "", fill = "") +
  theme(
    legend.position = c(0.15, 0.25),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.title = element_text(color = "black", size = 11, face = "bold"),
    legend.text  = element_text(color = "black", size = 10, face = "bold"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    plot.title   = element_text(size = 14, face = "bold", hjust = 0, vjust = 1)
  )

## ----- Insets
p2 <- p1 + labs(x = " ", y = "", title = "Caribbean and Central America") +
  coord_cartesian(xlim = c(-92, -60), ylim = c(5, 27)) +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))

p3 <- p1 + labs(x = " ", y = "", title = "Persian Gulf") +
  coord_cartesian(xlim = c(45, 55), ylim = c(19, 31)) +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))

p4 <- p1 + labs(x = " ", y = "", title = "Balkan Peninsula") +
  coord_cartesian(xlim = c(12, 32), ylim = c(35, 53)) +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))

p5 <- p1 + labs(x = " ", y = "", title = "Southeast Asia") +
  coord_cartesian(xlim = c(98, 123), ylim = c(-10, 8)) +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))

p6 <- p1 + labs(x = " ", y = "", title = "West Africa") +
  coord_cartesian(xlim = c(-17, -7), ylim = c(7, 20)) +
  theme(legend.position = "none", plot.title = element_text(size = 8, face = "bold"))

p7 <- p1 + labs(x = " ", y = "", title = "Eastern\nMediterranean") +
  coord_cartesian(xlim = c(32, 37), ylim = c(29, 35)) +
  theme(legend.position = "none", plot.title = element_text(size = 8, face = "bold"))

p8 <- p1 + labs(x = " ", y = "", title = "Northern Europe") +
  coord_cartesian(xlim = c(5, 25), ylim = c(48, 60)) +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))

A <- (p6 + p7) / p8

## ----- Assemble final figure
f2_a1 <- p1 + (p2 + p3 + p4 + p5 + A + plot_layout(ncol = 5, widths = c(1.5, 1, 1.1, 1.2, 1))) +
  plot_layout(nrow = 2, heights = c(0.9, 0.3))

f2_a1
