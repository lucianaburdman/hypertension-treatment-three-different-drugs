---
title: "Evaluation of three drugs in the hypertension treatment"
author: "Luciana Burdman"
date: "2023-04-14"
output: html_document
---

# Introduction

As a biostatistician with a strong background in cutting-edge statistical and scientific developments, I am excited to showcase my skills and expertise that aligns with Parexel's core values and services. Leveraging my experience in the industry and in Parexel, I have developed a repository that focuses on two of the three essential element of HEOR that Parexel provides support for: evidence identification and health economic modeling.

**If you'd like to go straight to the results plots and interpretation, click [here](https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs#results)**

# Evidence Identification with Literature Review

As a biostatistician with 8+ years of experience in life science research, I understand the importance of evidence identification through systematic and comprehensive literature reviews in Health Economics and Outcomes Research (HEOR). Parexel's focus on HTA-compliant reviews, covering clinical trial outcomes, quality of life, product comparators and economic evidence, aligns with my analytical mindset and research expertise. With a collaborative and self-reliant approach, I am confident in my ability to eliminate risk of bias, identify necessary data and adapt to meet the objectives of the study. My track record of leading research projects and proposing innovative solutions, coupled with strong communication skills, will enable me to work effectively with a diverse team of medical professionals, statisticians, and investors to provide a comprehensive evidence package that advances clinical research in healthcare's most complex areas.

# Health Economic Modeling

In this section, I will showcase my skills in health economic modeling by simulating and comparing the cost-effectiveness of three drugs for treating hypertension using R, in line with Parexel's focus on generating evidence-based value propositions and decision-making tools.

The following R code simulates data for three drugs (A, B, C) to treat hypertension and estimates the effect of each drug.

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
Figure 1: Cost per QALY gained for three drugs to treat hypertension.

Although it is not normal to have negative costs per QALYs gained, this is a limitation of our simulated data. Negative costs per QALYs gained would suggest that a treatment not only improves health outcomes but also saves costs, which is typically not the case.

One potential explanation for negative costs per QALYs gained in real data, could be that the model did not take into account all relevant costs and benefits associated with each treatment option. For example, if the model did not account for all indirect costs (such as productivity losses) or included some double counting of costs or benefits, it could potentially lead to negative costs per QALYs gained.

## Results

To visualize the results, we create a cost-effectiveness plane (Figure 2) and an incremental cost-effectiveness ratio (ICER) plot (Figure 3). Figure 2 shows the cost and QALY results for each drug, with each point representing one simulation. The bottom right quadrant of the plot represents the most cost-effective option, and as we can see, drug A dominates the other two drugs. Figure 3 shows the ICER results, which compare the incremental cost per QALY gained between each pair of drugs. The vertical line represents the willingness-to-pay threshold, which is the maximum amount the decision maker is willing to pay for an additional QALY gained. As we can see, drug A is the most cost-effective option, as it has the lowest ICER compared to the other two drugs.

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

The cost-effectiveness plane (Figure 2), shows the cost and QALY results for each drug, with each point representing one simulation. The bottom right quadrant of the plot represents the most cost-effective option, and as we can see, drug A dominates the other two drugs. 

<img src="https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs/blob/28ed6072dbaed988c7c543ad99f7c6c20def1f88/Image2.png">
Figure 2: Cost-effectiveness plane.

We can see that the simulations for drug A have the lowest cost and highest QALYs, followed by drug B and drug C. This suggests that drug A is the most cost-effective option for treating hypertension among the three drugs.

The incremental cost-effectiveness ratio (ICER) plot (Figure 3) shows the results which compare the incremental cost per QALY gained between each pair of drugs. The vertical line represents the willingness-to-pay threshold, which is the maximum amount the decision maker is willing to pay for an additional QALY gained. As we can see, drug A is the most cost-effective option, as it has the lowest ICER compared to the other two drugs.

<img src="https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs/blob/28ed6072dbaed988c7c543ad99f7c6c20def1f88/Image3.png">
Figure 3: Incremental cost-effectiveness ratio (ICER) plot.

When comparing drug A to drug B, we can see that the ICER is negative (-999), indicating that drug A dominates drug B. This means that drug A is both less costly and more effective than drug B, making it the dominant treatment option. The same happens for A vs. C (-1252) and B vs. C (-1504).

A negative ICER means that the treatment option in the numerator (drug A in our case) is both less costly and more effective than the treatment option in the denominator (drug B in our case). When this occurs, the treatment option in the numerator is said to dominate the treatment option in the denominator, and it is considered the more cost-effective option. In other words, the negative ICER indicates that the decision-maker can gain more health benefits at a lower cost by choosing the treatment option in the numerator over the treatment option in the denominator.

## Interpretation

Based on our simulations, drug A is the most cost-effective option for treating hypertension, as it has the lowest overall costs and highest QALYs gained. This suggests that if decision makers are looking to maximize health outcomes while minimizing costs, they should consider using drug A over the other two options.

It is important to note that our simulations were based on several assumptions and limitations, such as the accuracy of the data used and the assumption that the model accurately reflects real-world conditions. Additionally, the results of the simulations may not be generalizable to all populations or healthcare settings. As such, decision makers should consider the limitations of the model and the specific context in which it will be used before making any final decisions.

Overall, our health economic modeling provides valuable insights into the cost-effectiveness of different drugs for treating hypertension, and can help inform decision making and resource allocation in healthcare settings.

# Would you like to see a small project on RWE?

Follow this [link](https://github.com/lucianaburdman/hypertension-treatment-three-different-drugs)

# About the author

If you made it this far, thank you for your attention!

I am a very active doctor in biology, pationate about statistics and projects planning. You may learn more about me and find my contact information on my [LinkedIn profile](https://www.linkedin.com/in/luciana-burdman-biostatistician/).
