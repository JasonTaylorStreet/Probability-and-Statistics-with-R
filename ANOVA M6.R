# Task 1 One-Way ANOVA
# Load the AMD.csv data set
assembly.lines <- read.csv(file = "C:/Users/jasst/OneDrive/Desktop/William & Mary/Prob and Stats with R/data/AMD.csv",
                           stringsAsFactors = TRUE)
#   1A-Step 1:
#     H0: The mean production rate of all 3 lines are the same. (=)
#     HA: There are differences in the mean production rates of the 3 lines (!=)
#   1B-Step 2 and 3:
oneway.test(ProductionRate~Processor, assembly.lines, var.equal = TRUE)
#   1C-Steps 4 and 5:
#     The mean production rates were not significantly different across all 3
#     processor assemble lines [F(2,57)=2.3041;p=0.1091], indicating the changes
#     over time have not impacted the initial target production rates.
#   1D-Step 6:
#     Post-Hoc tests are not required because the one-way ANOVA for all 3 lines
#     was not statistically significant.

################################################################################
# Task 2 One-Way ANOVA
# Load the score.csv data
sections.final <- read.csv(file = "C:/Users/jasst/OneDrive/Desktop/William & Mary/Prob and Stats with R/data/score.csv",
                           stringsAsFactors = TRUE)
#   2A-Step 1:
#     H0: The mean scores across the 3 sections is the same. (=)
#     HA: There are differences in the mean score of the 3 sections (!=)
#   2B-Step 2 and 3:
oneway.test(Score~Section, sections.final, var.equal = TRUE)
#   2C-Steps 4 and 5:
#     The mean final exam scores were significantly different across the 3
#     sections {F(2,87)=23.884;p<0.05}.
#   2D-Step 6:
pairwise.t.test(sections.final$Score, sections.final$Section, p.adj="bonf")
#     The Bonferroni Post-Hoc test indicates statistically significant
#     differences in all pairing comparisons of the sections and their mean
#     final exam scores

################################################################################
# Task 3 Two-Way ANOVA
# Load the gymnast.csv data set
meet.scores <- read.csv(file = "C:/Users/jasst/OneDrive/Desktop/William & Mary/Prob and Stats with R/data/gymnast.csv",
                           stringsAsFactors = TRUE)
#   3A-Step 1:
#     Hypothesis 1:
#       H0: The mean scores across the judges are the same (=)
#       HA: There is a difference in mean scores across judges (!=)
#     Hypothesis 2:
#       H0: The mean scores across the gymnasts are the same (=)
#       HA: There is a difference in mean scores across the gymnasts (!=)
#   3B-Step 2 and 3:
meet.scores.2way<-aov(Score~Judge+Gymnast, data=meet.scores)
summary(meet.scores.2way)
#   3C-Steps 4 and 5:
#     There is no significant difference between scores across judges with a
#     F score of 0.804 and p-value of 0.47 (p>0.05). However, there is a
#     significant difference of scores across gymnasts with a F score of 69.100
#     and a p-value considerably less than 5% (p<0.05). Post-Hoc Tests would
#     normally be ran on gymnast to further investigate differences.

################################################################################
# Task 4 Selecting Number of Pairs
#   4A-Pairs for 3 judges
#       result: 3 p-values
choose(3, 2)
#   4B-Pairs for 7 gymnasts
#       result: 21 p-values
choose(7,2)

################################################################################
# Task 5 Type I and Type II Error
#   5A: What is the situation when we did not reject null and it was the correct
#       decision?
#     The individual did not have the disease and it was correctly predicted.
#   5B: What is the probability of a Type I error?
#     We predicted they had the disease when they actually didnt, alpha=0.026
#   5C: What is the probability of a Type II error?
#     We predicted the individual doesnt have the disease but they actually did
#     have the disease, beta=0.042
#   5D: What is the situation where the null hypothesis was false and it was the
#       correct decision?
#     It was predicted they had the disease and they did have the disease.
#     1-beta or 0.958
