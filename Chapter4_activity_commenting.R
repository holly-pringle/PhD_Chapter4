# ACTIVITY (DIURNALITY) 
brazil_30min<-read.csv("brazil_30min.csv") #https://drive.google.com/drive/folders/1cS2ekm7iep97Rzqh9GXaJIDGbLMAfXsz?usp=drive_link
nepal_30min<-read.csv("nepal_30min4.csv")
kenya_30min<-read.csv("kenya_30min.csv")
head(brazil_30min)
head(nepal_30min)
head(kenya_30min)

library(dplyr)
library(lubridate)
library(overlap)

brazil_clean <- brazil_30min %>%
  filter(keep == TRUE) %>%
  transmute(
    Species,
    CT_site,
    biome = "Brazil",
    datetime = ymd_hms(DateTime),
    sunrise  = ymd_hms(sunrise),
    sunset   = ymd_hms(sunset)
  )

nepal_clean <- nepal_30min %>%
  filter(keep == TRUE) %>%
  transmute(
    Species,
    CT_site = station,
    biome = "Nepal",
    datetime = ymd_hms(DateTime),
    sunrise  = ymd_hms(sunrise),
    sunset   = ymd_hms(sunset)
  )

kenya_clean <- kenya_30min %>%
  filter(keep == TRUE) %>%
  transmute(
    Species = label,
    CT_site,
    biome = "Kenya",
    datetime = ymd_hms(DateTime),
    sunrise  = ymd_hms(sunrise),
    sunset   = ymd_hms(sunset)
  )

all_detections <- bind_rows(brazil_clean, nepal_clean, kenya_clean)

nrow(all_detections)
unique(all_detections$Species)

all_detections_clean <- all_detections %>%
  mutate(Species = case_when(
    Species == "Lupulella_species" ~ "Lupulella_mesomelas",
    Species == "Mungos_species"    ~ "Mungos_mungo",
    TRUE ~ Species
  ))

all_detections_clean <- all_detections_clean %>%
  filter(!Species %in% c("Cavia_aperea", "Cabassous_species", "Leopardus_species",
                         "Dasypus_species", "Lepus_Springhare"))

all_detections_clean <- all_detections_clean%>%
  mutate(
    clock_hr   = hour(datetime) + minute(datetime)/60 + second(datetime)/3600,
    clock_rad  = clock_hr / 24 * 2 * pi,
    sunrise_hr = hour(sunrise) + minute(sunrise)/60 + second(sunrise)/3600,
    sunrise_rad = sunrise_hr / 24 * 2 * pi,
    sunset_hr  = hour(sunset) + minute(sunset)/60 + second(sunset)/3600,
    sunset_rad = sunset_hr / 24 * 2 * pi
  )

n_distinct(all_detections_clean$Species)
all_detections_clean <- all_detections_clean %>%
  filter(!is.na(clock_rad))

nrow(all_detections_clean)
sum(is.na(all_detections_clean$clock_rad))  # should be 0

system.time({ #report how long
  test_times <- all_detections_clean %>% filter(Species == "Mazama_gouazoubira") %>% pull(clock_rad)
  test_fit <- fitact(test_times, sample = "data", reps = 1000)
})

species_list <- unique(all_detections_clean$Species)
length(species_list)

activity_fits <- list()
activity_results <- data.frame()

for (i in seq_along(species_list)) {
  
  sp <- species_list[i]
  
  times <- all_detections_clean %>%
    filter(Species == sp) %>%
    pull(clock_rad)
  
  cat(sprintf("[%d/%d] %s - n = %d\n", i, length(species_list), sp, length(times)))
  
  fit <- tryCatch(
    fitact(times, sample = "data", reps = 1000),
    error = function(e) { cat("  Failed:", conditionMessage(e), "\n"); NULL }
  )
  
  activity_fits[[sp]] <- fit
  
  if (!is.null(fit)) {
    activity_results <- rbind(activity_results, data.frame(
      Species = sp,
      n       = length(times),
      act_est = fit@act[1],
      act_se  = fit@act[2],
      act_lcl = fit@act[3],
      act_ucl = fit@act[4]
    ))
  }
  
  saveRDS(activity_fits, "activity_fits_all_progress.rds")
  saveRDS(activity_results, "activity_results_progress.rds")
}

nrow(activity_results)
activity_results %>% arrange(n) %>% as.data.frame()

activity_results %>% filter(act_lcl > act_ucl)

activity_results <- activity_results %>%
  mutate(ci_width = act_ucl - act_lcl)

activity_results %>% arrange(desc(ci_width)) %>% select(Species, n, act_est, ci_width)

plot(activity_fits[["Mazama_gouazoubira"]], main = "Mazama gouazoubira") # roughly matches what I got in my masters so that's good!
plot(activity_fits[["Chrysocyon_brachyurus"]], main = "Chrysocyon_brachyurus") #my fave
plot(activity_fits[["Equus_quagga"]], main = "Equus_quagga") #erins fave
plot(activity_fits[["Paguma_larvata"]], main = "Paguma_larvata") #adorable https://live.staticflickr.com/1932/44895449872_7897a05643_b.jpg

# which species failed to produce any result at all?
setdiff(species_list, activity_results$Species)

activity_results_clean <- activity_results %>%
  filter(n >= 2)

nrow(activity_results_clean)

get_diurnal_proportion <- function(fit, sunrise_hr, sunset_hr) {
  if (is.null(fit)) return(NA_real_)
  
  pdf_matrix <- fit@pdf
  x_radians  <- pdf_matrix[, "x"]   # grid points, in radians (0 to 2*pi)
  y_density  <- pdf_matrix[, "y"]   # fitted density values
  
  x_hours <- x_radians / (2 * pi) * 24   # convert grid back to hour-of-day
  
  daylight <- x_hours >= sunrise_hr & x_hours <= sunset_hr
  
  sum(y_density[daylight]) / sum(y_density)
}

test_fit <- activity_fits[["Mazama_gouazoubira"]]
range(test_fit@pdf[, "x"])   # confirm this spans 0 to 2*pi (or close to it)

get_diurnal_proportion(test_fit, sunrise_hr = 6, sunset_hr = 18)

species_names <- names(activity_fits)[!sapply(activity_fits, is.null)]

diurnal_props <- data.frame()

for (sp in species_names) {
  prop <- get_diurnal_proportion(activity_fits[[sp]], sunrise_hr = 6, sunset_hr = 18)
  
  diurnal_props <- rbind(diurnal_props, data.frame(
    Species = sp,
    pct_diurnal_kde = prop * 100
  ))
}

diurnal_props %>% arrange(pct_diurnal_kde) %>% as.data.frame() #looks sensible

lupulella_times <- all_detections_clean %>%
  filter(Species == "Lupulella_mesomelas") %>%
  pull(clock_rad)

mungos_times <- all_detections_clean %>%
  filter(Species == "Mungos_mungo") %>%
  pull(clock_rad)

length(lupulella_times)
length(mungos_times)

activity_fits[["Lupulella_mesomelas"]] <- fitact(lupulella_times, sample = "data", reps = 1000)
activity_fits[["Mungos_mungo"]]        <- fitact(mungos_times, sample = "data", reps = 1000)

#saveRDS(activity_fits, "activity_fits_all_progress.rds")

lupulella_prop <- get_diurnal_proportion(activity_fits[["Lupulella_mesomelas"]], sunrise_hr = 6, sunset_hr = 18)
mungos_prop    <- get_diurnal_proportion(activity_fits[["Mungos_mungo"]], sunrise_hr = 6, sunset_hr = 18)

diurnal_props <- diurnal_props %>%
  filter(!Species %in% c("Lupulella_mesomelas", "Mungos_mungo")) %>%  # clear any stale/partial entries first
  bind_rows(
    data.frame(
      Species = c("Lupulella_mesomelas", "Mungos_mungo"),
      pct_diurnal_kde = c(lupulella_prop * 100, mungos_prop * 100)
    )
  )

nrow(diurnal_props)  # confirm count as expected
diurnal_props %>% filter(Species %in% c("Lupulella_mesomelas", "Mungos_mungo"))
diurnal_props <- diurnal_props %>%
  filter(!Species %in% c("Lupulella_species", "Mungos_species"))

nrow(diurnal_props)  # should now drop by 2, to 85

species_kde_scaling <- diurnal_props %>%
  filter(!Species %in% c("Galictis_cuja", "Herpestes_urva", "Hydrochoerus_hydrochaeris",
                         "Mellivora_capensis", "Puma_yagouaroundi")) %>%
  rename(binomial_name = Species, pct_diurnal_kde_raw = pct_diurnal_kde) %>%
  mutate(pct_diurnal_kde_scaled = as.numeric(scale(pct_diurnal_kde_raw)))

# check it worked as expected
mean(species_kde_scaling$pct_diurnal_kde_scaled, na.rm = TRUE)  # should be ~0
sd(species_kde_scaling$pct_diurnal_kde_scaled, na.rm = TRUE)    # should be exactly 1
