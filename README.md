---
title: "Cost-Effectiveness Acceptability Curve Plots"
author: "Luciana Burdman"
date: "2023-04-14"
output: html_document
---

# Introduction

As a biostatistician with a strong background in HEOR, I am excited to showcase my skills and expertise in a validation project that aligns with Parexel's core values and services. Leveraging my experience in the industry and my familiarity with the company, I have developed a project that focuses on three critical aspects of HEOR: evidence identification, health economic modeling, and clinical outcomes assessments.

By conducting a thorough literature review, designing a robust health economic model, and selecting the most appropriate clinical outcomes assessments, my project aims to demonstrate my ability to provide a comprehensive and data-driven evidence package that meets HTA guidelines. I am confident that this project will not only showcase my skills but also demonstrate my commitment to advancing clinical research and improving patient outcomes.

**If you'd like to go straight to the results plots and interpretation, click [here](##-results)**

# Evidence Identification with Literature Review

As a biostatistician with 8+ years of experience in life science research, I understand the importance of evidence identification through systematic and comprehensive literature reviews in Health Economics and Outcomes Research (HEOR). Parexel's focus on HTA-compliant reviews, covering clinical trial outcomes, quality of life, product comparators, economic evidence, and more, aligns with my analytical mindset and research expertise. With a collaborative and self-reliant approach, I am confident in my ability to eliminate risk of bias, identify necessary data for reimbursement, and adapt our approach to meet the objectives of the study. My track record of leading research projects and proposing innovative solutions, coupled with strong communication skills, will enable me to work effectively with a diverse team of medical professionals, statisticians, and investors to provide a comprehensive evidence package that advances clinical research in healthcare's most complex areas.

# Health Economic Modeling

In this section, I will showcase my skills in health economic modeling by simulating and comparing the cost-effectiveness of three drugs for treating hypertension using R, in line with Parexel's focus on generating evidence-based value propositions and decision-making tools.

The following R code simulates data for three drugs (A, B, C) to treat hypertension and estimates the effect of each drug on systolic

```{r}
library(tidyverse)

# Simulating data for three drugs (A, B, C) to treat hypertension
set.seed(123)
n <- 1000

# Drug A
a <- tibble(
  id = seq(1:n),
  age = rnorm(n, 50, 10),
  sex = sample(c("male", "female"), n, replace = TRUE),
  sbp = rnorm(n, 140, 15),
  dbp = rnorm(n, 90, 10),
  drug = rep("A", n),
  ldl = rnorm(n, 100, 20),
  hdl = rnorm(n, 50, 10),
  bmi = rnorm(n, 25, 5),
  smoking = sample(c("yes", "no"), n, replace = TRUE)
)

# Drug B
b <- tibble(
  id = seq(1:n),
  age = rnorm(n, 50, 10),
  sex = sample(c("male", "female"), n, replace = TRUE),
  sbp = rnorm(n, 135, 15),
  dbp = rnorm(n, 85, 10),
  drug = rep("B", n),
  ldl = rnorm(n, 90, 15),
  hdl = rnorm(n, 55, 10),
  bmi = rnorm(n, 24, 5),
  smoking = sample(c("yes", "no"), n, replace = TRUE)
)

# Drug C
c <- tibble(
  id = seq(1:n),
  age = rnorm(n, 50, 10),
  sex = sample(c("male", "female"), n, replace = TRUE),
  sbp = rnorm(n, 130, 15),
  dbp = rnorm(n, 80, 10),
  drug = rep("C", n),
  ldl = rnorm(n, 80, 10),
  hdl = rnorm(n, 60, 10),
  bmi = rnorm(n, 23, 5),
  smoking = sample(c("yes", "no"), n, replace = TRUE)
)

# Combining data for all three drugs
data <- bind_rows(a, b, c)

# Creating a linear regression model to estimate the effect of each drug on SBP
model <- lm(sbp ~ drug + age + sex + ldl + hdl + bmi + smoking, data = data)

# Cost-utility analysis
# Assuming the cost of each drug is $100 per month
cost <- c(100, 150, 200) # cost of drugs A, B, C respectively

# Quality-adjusted life years (QALYs) gained
# QALYs gained = (change in SBP / 10) * 0.03 (0.03 is the utility weight for hypertension)
data$qalys <- ((predict(model, newdata = data) - data$sbp) / 10) * 0.03

# Cost per QALY gained for each drug
data$cpq <- cost / data$qalys

# Plotting the results
ggplot(data, aes(x = drug, y = cpq, fill = drug)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$"), limits = c(-10000, 10000)) +
  labs(title = "Cost per QALY gained for three drugs to treat hypertension",
       x = "Drug",
       y = "Cost per QALY gained ($/QALY)")
```

<img src="https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs/blob/28ed6072dbaed988c7c543ad99f7c6c20def1f88/Image1.png">

To visualize the results, we created a cost-effectiveness plane (Figure 1) and an incremental cost-effectiveness ratio (ICER) plot (Figure 2). Figure 1 shows the cost and QALY results for each drug, with each point representing one simulation. The bottom right quadrant of the plot represents the most cost-effective option, and as we can see, drug A dominates the other two drugs. Figure 2 shows the ICER results, which compare the incremental cost per QALY gained between each pair of drugs. The vertical line represents the willingness-to-pay threshold, which is the maximum amount the decision maker is willing to pay for an additional QALY gained. As we can see, drug A is the most cost-effective option, as it has the lowest ICER compared to the other two drugs.

## Results

To visualize the results, we created a cost-effectiveness plane (Figure 1) and an incremental cost-effectiveness ratio (ICER) plot (Figure 2). Figure 1 shows the cost and QALY results for each drug, with each point representing one simulation. The bottom right quadrant of the plot represents the most cost-effective option, and as we can see, drug A dominates the other two drugs. Figure 2 shows the ICER results, which compare the incremental cost per QALY gained between each pair of drugs. The vertical line represents the willingness-to-pay threshold, which is the maximum amount the decision maker is willing to pay for an additional QALY gained. As we can see, drug A is the most cost-effective option, as it has the lowest ICER compared to the other two drugs.

```{r}
## Cost-effectiveness plane
# Create data frame with results for each drug
results <- data.frame(drug = c(rep("A", 1000), rep("B", 1000), rep("C", 1000)),
                      cost = c(rnorm(1000, 10000, 500), rnorm(1000, 12000, 500), rnorm(1000, 15000, 500)),
                      qaly = c(rnorm(1000, 10, 0.5), rnorm(1000, 8, 0.5), rnorm(1000, 6, 0.5)))

# Plot cost-effectiveness plane
ggplot(results, aes(x = cost, y = qaly, color = drug)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_hline(yintercept = max(results$qaly), linetype = "dashed") +
  geom_vline(xintercept = min(results$cost), linetype = "dashed") +
  labs(x = "Total costs ($)",
       y = "QALYs gained",
       color = "Drug",
       title = "Cost-effectiveness plane") +
  theme_minimal()
  
  ## Incremental cost-effectiveness ratio (ICER) plot

# Calculate ICER between drugs A and B
icer_ab <- (mean(results$cost[results$drug == "B"]) - mean(results$cost[results$drug == "A"])) /
  (mean(results$qaly[results$drug == "B"]) - mean(results$qaly[results$drug == "A"]))

# Calculate ICER between drugs A and C
icer_ac <- (mean(results$cost[results$drug == "C"]) - mean(results$cost[results$drug == "A"])) /
  (mean(results$qaly[results$drug == "C"]) - mean(results$qaly[results$drug == "A"]))

# Calculate ICER between drugs B and C
icer_bc <- (mean(results$cost[results$drug == "C"]) - mean(results$cost[results$drug == "B"])) /
  (mean(results$qaly[results$drug == "C"]) - mean(results$qaly[results$drug == "B"]))

# Create data frame with ICER results
icer_results <- data.frame(drugs = c("A vs B", "A vs C", "B vs C"),
                           ICER = c(icer_ab, icer_ac, icer_bc))

# Plot ICER results
ggplot(icer_results, aes(x = drugs, y = ICER)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sprintf("$%0.0f", ICER)), vjust = -0.5) +
  labs(x = "Drug comparison",
       y = "Incremental cost-effectiveness ratio (ICER)",
       title = "ICER plot") +
  theme_minimal()

```

<img src="https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs/blob/28ed6072dbaed988c7c543ad99f7c6c20def1f88/Image2.png">
Figure 2: Cost-effectiveness plane.

<img src="https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs/blob/28ed6072dbaed988c7c543ad99f7c6c20def1f88/Image3.png">
Figure 3: Incremental cost-effectiveness ratio (ICER) plot.

## Interpretation

Based on our simulations, drug A is the most cost-effective option for treating hypertension, as it has the lowest overall costs and highest QALYs gained. This suggests that if decision makers are looking to maximize health outcomes while minimizing costs, they should consider using drug A over the other two options.

It is important to note that our simulations were based on several assumptions and limitations, such as the accuracy of the data used and the assumption that the model accurately reflects real-world conditions. Additionally, the results of the simulations may not be generalizable to all populations or healthcare settings. As such, decision makers should consider the limitations of the model and the specific context in which it will be used before making any final decisions.

Overall, our health economic modeling provides valuable insights into the cost-effectiveness of different drugs for treating hypertension, and can help inform decision making and resource allocation in healthcare settings.
