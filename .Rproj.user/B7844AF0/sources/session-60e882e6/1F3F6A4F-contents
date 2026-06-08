# =====================================================
# Synthetic UTAUT2 Dataset Generator
# Case Study: ChatGPT Adoption in Higher Education
# =====================================================

library(tidyverse)
library(openxlsx)

set.seed(2026)

n <- 503

# -----------------------------------------------------
# Latent Variables
# -----------------------------------------------------

PE <- rnorm(n, 5.4, 0.9)
EE <- rnorm(n, 5.2, 0.9)
SI <- rnorm(n, 4.8, 1.0)
FC <- rnorm(n, 5.1, 0.9)
HM <- rnorm(n, 5.5, 0.8)
PV <- rnorm(n, 5.0, 0.9)
HT <- rnorm(n, 5.6, 0.8)
PI <- rnorm(n, 5.3, 0.9)

# -----------------------------------------------------
# Behavioral Intention
# -----------------------------------------------------

BI_latent <-
  0.18*PE +
  0.10*EE +
  0.08*SI +
  0.05*FC +
  0.15*HM +
  0.08*PV +
  0.30*HT +
  0.20*PI +
  rnorm(n,0,0.5)

# -----------------------------------------------------
# Use Behavior
# -----------------------------------------------------

UB_latent <-
  0.20*FC +
  0.30*HT +
  0.45*BI_latent +
  rnorm(n,0,0.6)

# -----------------------------------------------------
# Helper Function
# -----------------------------------------------------

make_items <- function(latent, prefix, k){
  
  out <- map_dfc(
    1:k,
    ~ round(
      pmin(
        pmax(
          latent + rnorm(length(latent),0,0.5),
          1
        ),
        7
      )
    )
  )
  
  names(out) <- paste0(prefix,1:k)
  
  out
}

# -----------------------------------------------------
# Indicators
# -----------------------------------------------------

PE_items <- make_items(PE,"PE",4)
EE_items <- make_items(EE,"EE",4)
SI_items <- make_items(SI,"SI",3)
FC_items <- make_items(FC,"FC",3)
HM_items <- make_items(HM,"HM",3)
PV_items <- make_items(PV,"PV",3)
HT_items <- make_items(HT,"HT",4)
PI_items <- make_items(PI,"PI",4)

BI_items <- make_items(BI_latent,"BI",3)

UB1 <- round(
  pmin(
    pmax(
      UB_latent,
      1
    ),
    7
  )
)

# -----------------------------------------------------
# Demographics
# -----------------------------------------------------

gender <- sample(
  c("Male","Female"),
  n,
  replace = TRUE
)

study_level <- sample(
  c(
    "Year1",
    "Year2",
    "Year3",
    "Year4"
  ),
  n,
  replace = TRUE
)

age <- round(
  rnorm(
    n,
    mean = 21,
    sd = 2
  )
)

# -----------------------------------------------------
# Final Dataset
# -----------------------------------------------------

utaut_data <-
  bind_cols(
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

# -----------------------------------------------------
# Export
# -----------------------------------------------------

write.xlsx(
  utaut_data,
  "utaut_dataset.xlsx",
  overwrite = TRUE
)

glimpse(utaut_data)

summary(utaut_data)
