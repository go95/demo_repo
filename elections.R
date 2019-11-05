library("reshape2")
library("dplyr")
set.seed(42)

elections <- read.csv2("elections.csv")
randomization <- read.csv2("randomization.csv")
population <- read.csv2("population.csv")
income <- read.csv2("income.csv")


elections_wide <- dcast(
  elections,
  ... ~ candidate_elections,
  value.var = "value"
)



mun_chars <- full_join(
  income,
  population,
  by = "municipality_code_noarr"
)


randomization_final <- left_join(
  randomization,
  mun_chars,
  by = c("municipality_code" = "municipality_code_noarr")
)


final_dataset <- left_join(
  randomization_final,
  elections_wide,
  by = c("municipality_code", "precinct_code")
)


statistics <- final_dataset %>%
  mutate(turnout = nb_voters_pr12t1_an / nb_registered_pr12t1_an) %>%
  group_by(treatment) %>%
  summarize(
              avg = mean(turnout, na.rm = TRUE),
              stddev = sd(turnout, na.rm = TRUE)
            )

print(statistics)

my_randomization <- final_dataset %>%
  mutate(my_treatment = runif(length(treatment)) <0.5)

print(head(my_randomization$my_treatment))
