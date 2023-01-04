#enter data
data <- read.csv("cleaned_data_Model_3.txt", header = TRUE, sep = ",", quote="\"", dec=".")

"n_arms" <- data$n_arms


# the response
y <- cbind(data$PostHbA1cmeanGroup1, data$PostHbA1cmeanGroup2, data$PostHbA1cmeanGroup3, data$PostHbA1cmeanGroup4)
se <- cbind(data$PostHbA1cSEGroup1, data$PostHbA1cSEGroup2, data$PostHbA1cSEGroup3, data$PostHbA1cSEGroup4)

#for cluster adjustments
avg_cluster_size <- cbind(data$avg_cluster_size, data$avg_cluster_size, data$avg_cluster_size, data$avg_cluster_size)
ICC <-cbind(data$ICCGroup1, data$ICCGroup2, data$ICCGroup3, data$ICCGroup4)

Baseline_risk_sample <- data$sample_average_baseline_HbA1c

Baseline_risk_study <- data$study_average_baseline_HbA1c

#component interventions
#AF <- cbind(data$AF1, data$AF2, data$AF3, data$AF4)

CM <- cbind(data$CM1, data$CM2, data$CM3, data$CM4)

TC  <- cbind(data$TC1, data$TC2, data$TC3, data$TC4)

EPR  <- cbind(data$EPR1, data$EPR2, data$EPR3, data$EPR4)

CE  <- cbind(data$CE1, data$CE2, data$CE3, data$CE4)

#CR  <- cbind(data$CR1, data$CR2, data$CR3, data$CR4)

FR  <- cbind(data$FR1, data$FR2, data$FR3, data$FR4)

PE  <- cbind(data$PE1, data$PE2, data$PE3, data$PE4)

PSM  <- cbind(data$PSM1, data$PSM2, data$PSM3, data$PSM4)

PR <- cbind(data$PR1, data$PR2, data$PR3, data$PR4)

# CQI  <- cbind(data$Continuousqualityimprove1, data$Continuousqualityimprove2, data$Continuousqualityimprove3, data$Continuousqualityimprove4)

# FI <- cbind(data$Financialincentives1, data$Financialincentives2, data$Financialincentives3, data$Financialincentives4)

Other <- cbind(data$QI_other1, data$QI_other2, data$QI_other3, data$QI_other4)

