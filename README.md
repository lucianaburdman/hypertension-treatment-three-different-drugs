---
title: "Cost-Effectiveness Acceptability Curve Plot for 3 Different Drugs"
author: "Luciana Burdman"
date: "2023-04-13"
output:
  html_document:
    toc: true
---


## Introduction

In this analysis, we will create a Cost-Effectiveness Acceptability Curve (CEAC) for 3 different drugs for a hypothetical disease - hypertension.

## Data Simulation

```{r}
library(tidyverse)
library(heemod)

set.seed(1234)

# Simulating data for 1000 patients
n <- 1000

# True treatment effect for each drug
treat_effect <- c(0.4, 0.5, 0.6)

# Generating patient-level characteristics
age <- rnorm(n, mean = 50, sd = 10)
sex <- sample(c("M", "F"), size = n, replace = TRUE, prob = c(0.6, 0.4))
sbp <- rnorm(n, mean = 140, sd = 10)
cvd <- sample(c("Yes", "No"), size = n, replace = TRUE, prob = c(0.3, 0.7))

# Generating true health outcomes
true_outcomes <- as.data.frame(matrix(NA, nrow = n, ncol = 4))
colnames(true_outcomes) <- c("Drug A", "Drug B", "Drug C", "No treatment")
true_outcomes[, 4] <- rbinom(n, 1, 0.2)
for (i in 1:3) {
  prob <- plogis(0.5 + treat_effect[i] * (age - 50)/10 - 0.1 * (sex == "F") + 0.05 * (sbp - 140)/10 + as.numeric(cvd == "Yes"))
  true_outcomes[, i] <- rbinom(n, 1, prob)
}

# Assigning costs and effects
costs <- c(1000, 1200, 1500)
effects <- c(mean(true_outcomes[, "Drug A"]), mean(true_outcomes[, "Drug B"]), mean(true_outcomes[, "Drug C"]))
```

## Cost-Effectiveness Acceptability Curve (CEAC)

```{r}
# Creating CEAC plot
ceac_data <- ceac(effects = effects, costs = costs, ref = "No treatment", alpha = 0.05)
ggplot(ceac_data, aes(x = threshold, y = mean, color = Drug)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_x_continuous("Willingness-to-pay (€)") +
  scale_y_continuous("Probability of being cost-effective") +
  labs(title = "Cost-Effectiveness Acceptability Curve",
       subtitle = "Three different drugs for hypertension",
       color = "Drug")
```

The CEAC plot above shows the probability of each drug being cost-effective compared to no treatment at different levels of willingness-to-pay (WTP).

In conclusion, based on the simulation results and the CEAC plot, drug C appears to be the most cost
