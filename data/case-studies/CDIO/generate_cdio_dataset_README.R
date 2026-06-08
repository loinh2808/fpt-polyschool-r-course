# =====================================================
# Synthetic CDIO Digital Assessment Adoption Dataset
# For UTAUT / PLS-SEM / Multi-group Analysis
# =====================================================

# This file documents the dataset logic.
# The Excel dataset was generated as synthetic teaching data.
# It is not real survey data and should not be treated as empirical evidence.

# Recommended R import:
# library(readxl)
# cdio_data <- readxl::read_excel("data/case-studies/cdio/cdio_adoption_synthetic_dataset.xlsx", sheet = "Data")
# dplyr::glimpse(cdio_data)

# Suggested constructs:
# PE = Performance Expectancy
# EE = Effort Expectancy
# SI = Social Influence
# FC = Facilitating Conditions
# DR = Digital Readiness
# OS = Organizational Support
# IN = System Integration
# WC = Workload Concern
# LE = Learning Effectiveness
# AC = Accessibility
# BI = Behavioral Intention
# AU = Actual Use / Use Behavior

# Suggested model:
# PE, EE, SI, FC, DR, OS, IN, WC, LE, AC -> BI
# BI, FC, DR -> AU
# Multi-group analysis: Teacher vs Student
