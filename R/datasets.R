
#' Age-Specific Fertility Rates in Iran
#'
#' Estimates of age-specific fertility rates,
#' (births per 1000 person-years lived)
#' for rural and urban areas, in Iran, 1986-2000.
#' Calculated by Mohammad Jalal Abbasi-Shavazi and
#' Peter McDonald from data from the 2000 Iran
#' Demographic and Health Survey.
#'
#' @format A tibble with 2010 rows and the following
#' columns:
#' - time Calendar year
#' - age Five-year age group from `"15-19"` to `"45-49"`
#' - area `"Rural" or `"Urban"`
#' - rate Age-specific fertility rate
#'
#' @source Tables 4.1 and 4.2 of
#' Abbasi-Shavazi, M J, McDonald, P (2005).
#' \emph{National and provincial level fertility trends in Iran,
#' 1972–2006}. Australian National University.
#' Working Papers in Demography no. 94.
"iran_fertility"


#' Mortality Data for New Zealand
#'
#' Counts of deaths and population, by age, sex, and calendar
#' year, plus mortality rates, for New Zealand, 2021-2022.
#'
#' @format A data frame with 84 rows and the
#' following variables:
#' - `year`: Calendar year.
#' - `gender`: `"Female"`, and `"Male"`.
#' - `age`: Age, in life table age groups, with an open age
#'    group of 95+.
#' - `deaths`: Counts of deaths, randomly rounded to base 3.
#' - `popn`: Estimates of average annual population.
#' - `mx`: Mortality rates (deaths / popn).
#'
#' @source Modified from data in tables
#' "Deaths by age and sex (Annual-Dec)" and
#' "Estimated Resident Population by Age and Sex (1991+) (Annual-Dec)"
#' from Stats NZ online database *Infoshare*,
#' downloaded on 24 September 2023.
"nzmort"


#' Mortality Data and Probabilistic Rates for New Zealand
#'
#' A modified version of \code{link{nzmort}} where `mx`
#' columns is an [rvec][rvec::rvec()], rather than an ordinary
#' R vector. The rvec holds the random draws from the posterior
#' distribution obtained from by a Bayesian statistical model.
"nzmort_rvec"



#' Coale-Demeny West Model Life Tables
#'
#' Life table quantities from the "West" family
#' of Coale-Demeny model life tables.
#'
#' @format A data frame with 1,050 rows and the
#' following variables:
#' - `level`: Index for life table. Lower level implies
#'    lower life expectancy.
#' - `sex`: `"Female"`, and `"Male"`.
#' - `age`: Age, in life table age groups, with an open age
#'    group of 95+.
#' - `mx`: Mortality rate.
#' - `ax`: Average years lived in age interval by people
#'   who die in that interval.
#' - `qx`: Probability some alive at start of age interval
#'   dies during interval.
#' - `lx`: Number of people still alive at start of
#'   age interval.
#' - `dx`: Number of people dying during age interval.
#' - `Lx`: Number of person-years lived during age interval.
#' - `ex`: Expectation of life at start of age interval.
#'
#' @source Coale A, Demeny P, and Vaughn B. 1983.
#' Regional model life tables and stable populations.
#' 2nd ed. New York: Academic Press,
#' accessed via `demogR::cdmltw()`.
"west_lifetab"
