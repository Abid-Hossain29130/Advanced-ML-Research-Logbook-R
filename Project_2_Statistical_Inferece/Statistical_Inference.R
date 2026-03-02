# --- Step 1: Initialize Parameters ---
# Managing libraries efficiently
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

# Setting constants
n <- 40              # Sample size
Simulations <- 1000  # Number of iterations
lambda <- 0.2        # Rate parameter

# Reproducibility
set.seed(123)

# --- Step 2: Simulation Engine ---
# Create an empty vector to store the 1,000 averages
sample_means <- NULL

# Execute Loop
for (i in 1:Simulations) {
  sample_means <- c(sample_means, mean(rexp(n, lambda)))
}

# Audit the first few rows of the generated data
head(sample_means)

# 1. The Canvas (The Histogram)
hist(sample_means, 
     breaks = 40, 
     prob = TRUE, 
     main = "Normality Audit: Distribution of Sample Means", 
     xlab = "Mean Values", 
     col = "darkseagreen3", 
     border = "white")

# 2. The Math (Defining the X and Y axes)
# We create 100 points between the min and max of your data
x_axis <- seq(min(sample_means), max(sample_means), length = 100)

# We calculate the 'Normal Density' for those points
y_axis <- dnorm(x_axis, mean = 5, sd = 5/sqrt(40))

# 3. The Overlay (Drawing the Line)
lines(x_axis, y_axis, col = "royalblue", lwd = 3)

# --- Step 4: Numerical Variance Comparison ---

# 1. Calculate the 'Simulated' Variance (from our experiment)
sim_variance <- var(sample_means)

# 2. Calculate the 'Theoretical' Variance (from the math formula)
# Formula: (Standard Deviation^2) / n
theor_variance <- (1/lambda)^2 / n

# Display results for Audit
cat("Simulated Variance:", sim_variance, "\n")
cat("Theoretical Variance:", theor_variance, "\n")


# --- Part 2: Step 1 - Initialization ---
data("ToothGrowth")

# Convert dose to factor to enable group-based inference
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Verify the metadata and structure
str(ToothGrowth)
summary(ToothGrowth)

library(ggplot2)

# Creating a professional Boxplot for initial comparison
ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot() +
  facet_grid(. ~ supp) +
  labs(title="Tooth Growth Analysis by Supplement and Dosage",
       x="Dose (mg/day)", y="Tooth Length") +
  theme_minimal() +
  scale_fill_manual(values=c("orange", "seagreen3"))

# Testing if Supplement Type has a significant effect on tooth growth
t.test(len ~ supp, data = ToothGrowth)

# Comparing growth between 0.5mg and 2.0mg dose groups
# We subset the data to only look at these two groups
dose_test <- subset(ToothGrowth, dose %in% c(0.5, 2.0))
t.test(len ~ dose, data = dose_test)



















