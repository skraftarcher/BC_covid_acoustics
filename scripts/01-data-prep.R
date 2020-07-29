# A project to determine how COVID-19 has impacted the soundscape around Nanaimo, BC

# this script extracts and merges sound pressure data

# packages----
if (!require(tidyverse)) install.packages("tidyverse") library(tidyverse)
if (!require(readxl)) install.packages("readxl") library(readxl)
if (!require(lubridate)) install.packages("lubridate") library(lubridate)

theme_set(theme_bw())


## data----
in19 <- read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2019")
in20 <- read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_In_2020")
out19 <- read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_Out_2019")
out20 <- read_xlsx("odata/Broadband SPL RCA.xlsx", sheet = "RCA_Out_2020")

# add in identifiers that allow us to combine the datasets
in192 <- in19 %>%
  mutate(
    grp = "RCA In 2019",
    rca = "in",
    year = 2019
  ) %>%
  filter(!(Month == 4 & Day == 10)) # day deployed

out192 <- out19 %>%
  mutate(
    grp = "RCA Out 2019",
    rca = "out",
    year = 2019
  ) %>%
  filter(!(Month == 4 & Day == 10)) # day deployed

in202 <- in20 %>%
  mutate(
    grp = "RCA In 2020",
    rca = "in",
    year = 2020
  ) %>%
  filter( # trim to full days in water
    !(Month == 4 & Day == 18) & # day deployed for period 1
      !(Month == 5 & Day == 24) & # day deployed for period 2
      !(Month == 6 & Day >= 21) # day retrived for period 2
  )

out202 <- out20 %>%
  mutate(
    grp = "RCA Out 2020",
    rca = "out",
    year = 2020
  ) %>%
  filter( # trim to full days in water
    !(Month == 4 & Day == 18) & # day deployed for period 1
      !(Month == 5 & Day == 24) & # day deployed for period 2
      !(Month == 6 & Day >= 19) # day retrived for period 2
  )

# create a dataset that is aggregated by the hour,
# several variables are calculated, 1,5,50,95, and 99 quantiles as well as the root mean square error

splhr <- bind_rows(in192, in202, out192, out202) %>%
  separate(Time, into = c("hr", "min", "sec"), sep = ":") %>%
  separate(hr, into = c("wste", "hr"), sep = " ", remove = TRUE) %>%
  select(-wste) %>%
  group_by(year, rca, grp, Month, Day, hr) %>%
  summarize(
    spl50 = median(SPL),
    rmsspl = sqrt(mean(SPL^2)),
    spl01 = quantile(SPL, .01),
    spl05 = quantile(SPL, .05),
    spl95 = quantile(SPL, .95),
    spl99 = quantile(SPL, .99)
  ) %>%
  drop_na() %>%
  rename(month = Month, day = Day) %>%
  mutate(
    hr = as.numeric(hr),
    date = make_date(year, month, day),
    md = format(date, format = "%m-%d"),
    mdh = paste0(md, "-", if_else(hr < 10, paste0(0, hr), as.character(hr))),
    ymdh = paste0(year, "-", month, "-", day, " ", hr, ":00:00"),
    datehr = as.POSIXct(strptime(ymdh, "%Y-%m-%d %H:%M:%S"))
  ) %>%
  ungroup() %>%
  arrange(mdh)

# save and reload in R only form
saveRDS(splhr, "wdata/splbyhour.rds")
splhr <- readRDS("wdata/splbyhour.rds")

# save for export to other platforms
write.csv(splhr, "wdata/splbyhour.csv")

# grouped by hour----
ggplot(splhr) +
  geom_line(aes(y = rmsspl, x = mdh, color = as.factor(year), group = grp)) +
  facet_wrap(~rca)

# create a dataset that is aggregated by day, same variables are calculated
spld <- bind_rows(in192, in202, out192, out202) %>%
  separate(Time, into = c("hr", "min", "sec"), sep = ":") %>%
  separate(hr, into = c("wste", "hr"), sep = " ", remove = TRUE) %>%
  select(-wste) %>%
  group_by(year, rca, grp, Month, Day) %>%
  summarize(
    spl50 = median(SPL),
    rmsspl = sqrt(mean(SPL^2)),
    spl01 = quantile(SPL, .01),
    spl05 = quantile(SPL, .05),
    spl95 = quantile(SPL, .95),
    spl99 = quantile(SPL, .99)
  ) %>%
  drop_na() %>%
  rename(month = Month, day = Day) %>%
  mutate(
    date = make_date(year, month, day),
    md = format(date, format = "%m-%d")
  ) %>%
  ungroup() %>%
  arrange(md)

saveRDS(spld, "wdata/splbyday.rds")
spld <- readRDS("wdata/splbyday.rds")
write.csv(spld, "wdata/splbyday.csv")

# grouped by day----
ggplot(spld) +
  geom_line(aes(y = spl50, x = mdh, color = as.factor(year), group = grp)) +
  facet_grid(~rca, scales = "free")

# subset down to only time periods recorded in 2020
# create datasets that only include the time periods where there is data in 2020 then subset the
# larger datasets down to those time periods

d19 <- spld %>%
  ungroup() %>%
  filter(year == 2019) %>%
  select(md) %>%
  distinct()
sort(d19[[1]])

d20 <- spld %>%
  ungroup() %>%
  filter(year == 2020) %>%
  select(md) %>%
  distinct()
sort(d20[[1]])

hr20 <- splhr %>%
  ungroup() %>%
  filter(year == 2020) %>%
  select(mdh) %>%
  distinct()

spld2 <- spld %>%
  filter(mdh %in% d20$md)

splhr2 <- splhr %>%
  filter(mdh %in% hr20$mdh)

# plotting only data in both data sets
ggplot(spld2) +
  geom_line(aes(y = spl50, x = mdh, color = as.factor(year), group = grp), size = 2) +
  facet_grid(~rca, scales = "free")

ggplot(splhr2) +
  geom_line(aes(y = spl50, x = mdh, color = as.factor(year), group = grp)) +
  facet_grid(~rca, scales = "free")

# get weather data
# note: may require updated version of tidyselect package
if (!require(weathercan)) devtools::install_github("ropensci/weathercan")

wthr19 <- weathercan::weather_dl(station_ids = 29411, start = "2019-04-14", end = "2019-06-25")
wthr20 <- weathercan::weather_dl(station_ids = 29411, start = "2020-04-19", end = "2020-06-20")

# add weather by day
wthr <- bind_rows(wthr19, wthr20) %>%
  select(date, year, month, day,
    wind_dir, wind_spd
  ) %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  group_by(date, year, month, day) %>%
  summarize(
    wdir = mean(wind_dir, na.rm = TRUE),
    wspeed = mean(wind_spd, na.rm = TRUE)
  ) %>%
  # join with sound pressure data by year, Month, Day
  left_join(spld) %>%
  filter(!is.na(rca)) %>%
  # filter(mdh %in% d20$mdh) %>%
  ungroup() %>%
  distinct()

wthr1.19 <- wthr %>%
  filter(date <= as.Date("2019-05-10") & date >= as.Date("2019-04-14")) %>%
  mutate(period = "early") %>%
  arrange(date) %>%
  # adds d2 variable that is the sequence of days within each sampling period
  # can change or remove grouping by period if sequence for whole year desired
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()

wthr1.20 <- wthr %>%
  filter(date <= as.Date("2020-05-10") & date >= as.Date("2020-04-14")) %>%
  mutate(period = "early") %>%
  arrange(date) %>%
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()

wthr2.19 <- wthr %>%
  filter(date >= as.Date("2019-05-20") & date <= as.Date("2019-06-25")) %>%
  mutate(period = "late") %>%
  arrange(date) %>%
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()

wthr2.20 <- wthr %>%
  filter(date >= as.Date("2020-05-20") & date <= as.Date("2020-06-25")) %>%
  mutate(period = "late") %>%
  arrange(date) %>%
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()


wthr2 <- bind_rows(wthr1.19, wthr1.20, wthr2.19, wthr2.20)

saveRDS(wthr2, "wdata/trimmed_daily_weather.rds")
wthr2 <- readRDS("wdata/trimmed_daily_weather.rds")
write.csv(wthr2, "wdata/trimmed_daily_weather.csv")


# add weather by hr
hr_wthr <- bind_rows(wthr19, wthr20) %>%
  separate(hour, into = c("hr", "min"), sep = ":") %>%
  select(date, year, month, day, hr,
    wind_dir, wind_spd
  ) %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day),
    hr = as.numeric(hr)
  ) %>%
  group_by(date, year, month, day, hr) %>%
  summarize(
    wdir = mean(wind_dir, na.rm = TRUE),
    wspeed = mean(wind_spd, na.rm = TRUE)
  ) %>%
  # join with sound pressure data by year, Month, Day, hr
  left_join(splhr) %>%
  filter(!is.na(rca)) %>%
  # filter(mdh %in% d20$mdh)%>%
  ungroup() %>%
  distinct()

hr_wthr1.19 <- hr_wthr %>%
  filter(date <= as.Date("2019-05-10") & date >= as.Date("2019-04-14")) %>%
  mutate(period = "early") %>%
  arrange(date) %>%
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()

hr_wthr1.20 <- hr_wthr %>%
  filter(date <= as.Date("2020-05-10") & date >= as.Date("2020-04-14")) %>%
  mutate(period = "early") %>%
  arrange(date) %>%
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()

hr_wthr2.19 <- hr_wthr %>%
  filter(date >= as.Date("2019-05-20") & date <= as.Date("2019-06-25")) %>%
  mutate(period = "late") %>%
  arrange(date) %>%
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()

hr_wthr2.20 <- hr_wthr %>%
  filter(date >= as.Date("2020-05-20") & date <= as.Date("2020-06-25")) %>%
  mutate(period = "late") %>%
  arrange(date) %>%
  group_by(rca, year, period) %>%
  mutate(d2 = seq_len(n())) %>%
  ungroup()

hr_wthr2 <- bind_rows(hr_wthr1.19, hr_wthr1.20, hr_wthr2.19, hr_wthr2.20)

saveRDS(wthr2, "wdata/trimmed_hourly_weather.rds")
wthr2 <- readRDS("wdata/trimmed_hourly_weather.rds")
write.csv(wthr2, "wdata/trimmed_hourly_weather.csv")
