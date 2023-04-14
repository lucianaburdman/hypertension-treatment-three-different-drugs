---
title: "Cost-Effectiveness Acceptability Curve Plots"
author: "Luciana Burdman"
date: "2023-04-14"
output: html_document
---

# Introduction

This project is designed to demonstrate the creation of cost-effectiveness acceptability curve plots using simulated data for three drugs, A, B, and C. The project walks through the entire process, starting from the simulation of the data, the cost-effectiveness analysis, and finally the creation of the cost-effectiveness acceptability curves, which show the probability of each drug being cost-effective at various willingness-to-pay (WTP) thresholds. The project uses R programming language and several R packages, including BCEA, ggplot2, dplyr, and tidyr.

The project presented here aims to apply skills in Health Economics and Outcomes Research (HEOR), specifically in cost-effectiveness analysis and acceptability curve plots. HEOR is a multidisciplinary field that involves the use of economic and epidemiological methods to evaluate the value of healthcare interventions and inform healthcare decision-making. The project simulates a cost-effectiveness analysis of three different drugs and demonstrates how to generate and interpret cost-effectiveness acceptability curves using R software. By learning these techniques, the project aims to equip individuals with practical skills to apply HEOR principles in real-world settings.


```{r}
# Load required packages
library(BCEA)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

```

# Simulated data

First, we will simulate some data for the three drugs. We assume that each drug has a different cost and effectiveness, and that the costs and effectiveness are normally distributed with the following means and standard deviations:

* Drug A: cost = 100, effectiveness = 0.8
* Drug B: cost = 200, effectiveness = 0.85
* Drug C: cost = 300, effectiveness = 0.9

We will simulate 1000 patients for each drug.

```{r}

# Simulate data for Drug A
drug_a <- data.frame(cost = rnorm(1000, mean = 100, sd = 10),
                      eff = rnorm(1000, mean = 0.8, sd = 0.05),
                      drug = "A")

# Simulate data for Drug B
drug_b <- data.frame(cost = rnorm(1000, mean = 200, sd = 20),
                      eff = rnorm(1000, mean = 0.85, sd = 0.03),
                      drug = "B")

# Simulate data for Drug C
drug_c <- data.frame(cost = rnorm(1000, mean = 300, sd = 30),
                      eff = rnorm(1000, mean = 0.9, sd = 0.02),
                      drug = "C")

# Combine data
sim_data <- bind_rows(drug_a, drug_b, drug_c)


```

# Cost-effectiveness analysis

Next, we will perform a cost-effectiveness analysis to compare the three drugs. We will assume a willingness-to-pay threshold of $50,000 per quality-adjusted life year (QALY).

```{r}
# Define cost-effectiveness model
ce_model <- function(cost, eff) {
  ce_ratio <- cost / eff
  ce_ratio[is.na(ce_ratio)] <- Inf
  return(ce_ratio)
}

# Calculate cost-effectiveness ratios
ce_ratios <- ce_model(sim_data$cost, sim_data$eff)

# Calculate incremental cost-effectiveness ratios
icrs <- function(d1, d2) {
  ce1 <- ce_model(d1$cost, d1$eff)
  ce2 <- ce_model(d2$cost, d2$eff)
  return(ce2 - ce1)
}

icr_ab <- icrs(drug_a, drug_b)
icr_ac <- icrs(drug_a, drug_c)
icr_bc <- icrs(drug_b, drug_c)

# Calculate net monetary benefits
nmb <- function(ce_ratio, wtp) {
  return(ce_ratio * wtp - 1)
}

nmb_a <- nmb(ce_ratios, 50000)
nmb_b <- nmb(ce_ratios - icr_ab, 50000)
nmb_c <- nmb(ce_ratios - icr_ac, 50000)

# Calculate probabilities of being cost-effective
pc_a <- mean(nmb_a > 0)
pc_b <- mean(nmb_b > 0)
pc_c <- mean(nmb_c > 0)


```


# Cost-Effectiveness Acceptability Curve plots

Finally, we will create Cost-Effectiveness Acceptability Curve plots to visualize the results.

```{r}

# Calculate acceptability curves
ac_data <- data.frame(wtp = seq(0, 150000, 5000))

ac_data$prob_a <- sapply(ac_data$wtp, function(w) mean(nmb_a > w))
ac_data$prob_b <- sapply(ac_data$wtp, function(w) mean(nmb_b > w))
ac_data$prob_c <- sapply(ac_data$wtp, function(w) mean(nmb_c > w))

ac_data <- ac_data %>% pivot_longer(cols = c(prob_a, prob_b, prob_c),
                                     names_to = "Drug",
                                     values_to = "Probability")

# Plot acceptability curves
ggplot(ac_data, aes(x = wtp, y = Probability, color = Drug)) +
  geom_line(size = 1.5) +
  scale_x_continuous("Willingness-to-pay (per QALY)",
                     limits = c(0, 150000),
                     breaks = seq(0, 150000, 25000)) +
  scale_y_continuous("Probability of being cost-effective",
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  labs(title = "Cost-Effectiveness Acceptability Curve",
       subtitle = "Comparison of Drugs A, B, and C",
       color = "Drug",
       caption = "Source: Simulated data") +
  theme_bw()


```

# Cost-Effectiveness Acceptability Curves for the three drugs

<img src="https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs/blob/4d6a18f31ed636b03cbb4cd91308a223262577d1/Image1.png">

The plot shows the Cost-Effectiveness Acceptability Curves for the three drugs (A, B, and C) over a range of willingness-to-pay (WTP) thresholds. The x-axis shows the WTP threshold in dollars per quality-adjusted life year (QALY), while the y-axis shows the probability of each drug being cost-effective at that WTP threshold.

We can see that Drug A is the most cost-effective drug, as it has the highest probability of being cost-effective at all WTP thresholds. Drug B is the second most cost-effective drug, while Drug C is the least cost-effective drug.

At a WTP threshold of USD50,000 per QALY (the standard threshold used in many countries), the probabilities of being cost-effective are 0.99 for Drug A, 0.85 for Drug B, and 0.59 for Drug C. This means that if the decision maker is willing to pay up to USD50,000 per QALY, there is a 99% probability that Drug A is the most cost-effective option.

Overall, the Cost-Effectiveness Acceptability Curve plots provide a useful tool for comparing the cost-effectiveness of different drugs and for informing decision making in healthcare.
