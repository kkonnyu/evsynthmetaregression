#enter data
data <- read.csv("cleaned_data_Model_2.txt", header = TRUE, sep = ",", quote="\"", dec=".")

"n_arms" <- data$n_arms

# the response
y <- cbind(data$PostHbA1cmeanGroup1, data$PostHbA1cmeanGroup2)
se <- cbind(data$PostHbA1cSEGroup1, data$PostHbA1cSEGroup2)

#for cluster adjustments
avg_cluster_size <- cbind(data$avg_cluster_size, data$avg_cluster_size)
ICC <-cbind(data$ICCGroup1, data$ICCGroup2)

#component interventions
#AF <- cbind(data$AF1, data$AF2)

CM <- cbind(data$CM1, data$CM2)

TC  <- cbind(data$TC1, data$TC2)

EPR  <- cbind(data$EPR1, data$EPR2)

CE  <- cbind(data$CE1, data$CE2)

#CR  <- cbind(data$CR1, data$CR2)

FR  <- cbind(data$FR1, data$FR2)

PE  <- cbind(data$PE1, data$PE2)

PSM  <- cbind(data$PSM1, data$PSM2)

PR <- cbind(data$PR1, data$PR2)

# CQI  <- cbind(data$Continuousqualityimprove1, data$Continuousqualityimprove2)

# FI <- cbind(data$Financialincentives1, data$Financialincentives2)

Other <- cbind(data$QI_other1, data$QI_other2)

