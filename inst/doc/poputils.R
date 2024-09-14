## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(poputils)
library(dplyr, warn.conflicts = FALSE)
tibble(original = c("5 to 9", "5_9", "05-09"),
       reformated = reformat_age(original))

## -----------------------------------------------------------------------------
df <- data.frame(age = c("5-9", "0-4", "15-19", "10-14"),
                 population = c(3, 7, 2, 4))
df
df |>
  arrange(age_lower(age))

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(df, aes(x = age_mid(age), y = population)) +
  geom_point()

## -----------------------------------------------------------------------------
tibble(age = age_labels("lt", max = 30),
       age_5 = combine_age(age, to = "five"),
       age_25plus = set_age_open(age, lower = 20))

## -----------------------------------------------------------------------------
reformat_sex(c("M", "F", "Diverse", "Fem"), except = "Diverse")

## -----------------------------------------------------------------------------
nzmort |>
  filter(year == 2022,
         gender == "Female") |>
  lifetab(mx = mx)  

## -----------------------------------------------------------------------------
nzmort |>
  lifeexp(mx = mx,
          by = c(gender, year))  

## -----------------------------------------------------------------------------
nzmort |>
  group_by(gender, year) |>
  lifeexp(mx = mx)

## -----------------------------------------------------------------------------
west_lifetab |>
  group_by(level, sex) |>
  lifeexp(qx = qx)

## -----------------------------------------------------------------------------
nzmort |>
  lifeexp(mx = mx,
          at = 65,
          by = c(gender, year))  

## -----------------------------------------------------------------------------
lin <- nzmort |>
  lifeexp(mx = mx,
          by = c(gender, year),
          infant = "linear",
          suffix = "lin")
ak <- nzmort |>
  lifeexp(mx = mx,
          sex = gender,
          by = year,
          infant = "AK", 
          suffix = "ak")
inner_join(lin, ak, by = c("year", "gender")) |>
  mutate(diff = ex.lin - ex.ak)

## -----------------------------------------------------------------------------
nzmort

## -----------------------------------------------------------------------------
library(rvec)
nzmort_rvec

## -----------------------------------------------------------------------------
library(rvec)
nzmort_rvec |>
  filter(year == 2022,
         gender == "Female") |>
  lifetab(mx = mx) |>
  select(age, qx, lx)

