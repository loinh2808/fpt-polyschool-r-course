library(tidyverse)
library(openxlsx)
library(MASS)

set.seed(2026)

n <- 503

# Correlated latent variables
latent_names <- c("PE", "EE", "SI", "FC", "HM", "PV", "HT", "PI")

R <- matrix(
  c(
    1.00, .45, .35, .40, .50, .35, .45, .40,
    .45, 1.00, .30, .45, .45, .30, .35, .35,
    .35, .30, 1.00, .35, .30, .25, .35, .30,
    .40, .45, .35, 1.00, .40, .35, .35, .35,
    .50, .45, .30, .40, 1.00, .40, .35, .40,
    .35, .30, .25, .35, .40, 1.00, .30, .30,
    .45, .35, .35, .35, .35, .30, 1.00, .35,
    .40, .35, .30, .35, .40, .30, .35, 1.00
  ),
  nrow = 8,
  byrow = TRUE
)

latent_raw <- MASS::mvrnorm(
  n = n,
  mu = rep(0, 8),
  Sigma = R
)

latent_raw <- as_tibble(latent_raw)
names(latent_raw) <- latent_names

# Convert latent variables to Likert-centered scale
latent <- latent_raw |>
  mutate(
    PE = 5.1 + PE * 0.85,
    EE = 5.5 + EE * 0.75,
    SI = 5.3 + SI * 0.85,
    FC = 5.0 + FC * 0.85,
    HM = 5.5 + HM * 0.80,
    PV = 5.4 + PV * 0.80,
    HT = 5.1 + HT * 0.90,
    PI = 4.8 + PI * 0.90
  )

PE <- latent$PE
EE <- latent$EE
SI <- latent$SI
FC <- latent$FC
HM <- latent$HM
PV <- latent$PV
HT <- latent$HT
PI <- latent$PI

# Target-like structural model
BI_latent <-
  0.26 * PE +
  0.08 * EE +
  0.09 * SI -
  0.02 * FC +
  0.19 * HM +
  0.08 * PV +
  0.34 * HT +
  0.09 * PI +
  rnorm(n, 0, 0.70)

UB_latent <-
  0.19 * FC +
  0.26 * HT +
  0.42 * BI_latent +
  rnorm(n, 0, 1.20)

make_items <- function(latent, prefix, k, error_sd = 0.45) {
  out <- purrr::map_dfc(
    1:k,
    ~ round(
      pmin(
        pmax(
          latent + rnorm(length(latent), 0, error_sd),
          1
        ),
        7
      )
    )
  )
  names(out) <- paste0(prefix, 1:k)
  out
}

PE_items <- make_items(PE, "PE", 4, 0.45)
EE_items <- make_items(EE, "EE", 4, 0.45)
SI_items <- make_items(SI, "SI", 3, 0.45)
FC_items <- make_items(FC, "FC", 3, 0.45)
HM_items <- make_items(HM, "HM", 3, 0.45)
PV_items <- make_items(PV, "PV", 3, 0.45)
HT_items <- make_items(HT, "HT", 4, 0.45)
PI_items <- make_items(PI, "PI", 4, 0.45)

BI_items <- make_items(BI_latent, "BI", 3, 0.55)

UB1 <- round(
  pmin(
    pmax(
      UB_latent,
      1
    ),
    7
  )
)

gender <- sample(
  c("Male", "Female", "Prefer not to say"),
  n,
  replace = TRUE,
  prob = c(0.53, 0.42, 0.05)
)

study_level <- sample(
  c("Bachelor Year 1", "Bachelor Year 2", "Bachelor Year 3",
    "Master Year 1", "Master Year 2", "PhD"),
  n,
  replace = TRUE,
  prob = c(0.05, 0.32, 0.38, 0.10, 0.13, 0.02)
)

age <- round(
  pmin(
    pmax(
      rnorm(n, mean = 21.5, sd = 2.2),
      18
    ),
    35
  )
)

utaut_data <- bind_cols(
  tibble(
    respondent_id = 1:n,
    gender,
    age,
    study_level
  ),
  PE_items,
  EE_items,
  SI_items,
  FC_items,
  HM_items,
  PV_items,
  HT_items,
  PI_items,
  BI_items,
  tibble(UB1)
)

dir.create(
  "data/case-studies/utaut",
  recursive = TRUE,
  showWarnings = FALSE
)

openxlsx::write.xlsx(
  utaut_data,
  "data/case-studies/utaut/utaut_dataset2.xlsx",
  overwrite = TRUE
)

dplyr::glimpse(utaut_data)