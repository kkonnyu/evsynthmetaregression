setwd("~/Dropbox/PhD/8-Thesis/Publications/2-Methods/Analysis")

# libraries
library(lattice)
library(coda)
library(rjags)

how_many_runs <- 100000

#CM
model <- jags.model("04_model_Model_3a1_CM.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)  
summary(posteriors)
	results <- summary(posteriors)
		c1 <- results$quantiles[,1]
		c2 <- results$quantiles[,3]
		c3 <- results$quantiles[,5]
	summary.table <- data.frame(cbind(c1,c2,c3))	
	write.table(summary.table, file = paste("model_3a1",".csv", sep=""), quote = F, row.names = T)
	
	#PPC 
	summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
	print(summary.table2)
	write.table(summary.table2, file= paste("04_model_Model_3a1_p_new", ".csv", sep=""), quote= F, row.names = T)
	
	gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
	for (v in 1:nvar(posteriors)) {
	        gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
	}
	gelman_diag
	diagnostics = paste("gelman_diag_", "04_model_Model_3a1", sep="")
	write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

#TC
model <- jags.model("04_model_Model_3a2_TC.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)  
summary(posteriors)
        results <- summary(posteriors)
                c1 <- results$quantiles[,1]
                c2 <- results$quantiles[,3]
                c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a2",".csv", sep=""), quote = F, row.names = T)
        
        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a2_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a2", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

#EPR        
model <- jags.model("04_model_Model_3a3_EPR.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)  
summary(posteriors)
        results <- summary(posteriors)
        c1 <- results$quantiles[,1]
        c2 <- results$quantiles[,3]
        c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a3",".csv", sep=""), quote = F, row.names = T)
        
        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a3_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a3", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

#CE        
model <- jags.model("04_model_Model_3a4_CE.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)  
summary(posteriors)
        results <- summary(posteriors)
                c1 <- results$quantiles[,1]
                c2 <- results$quantiles[,3]
                c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a4",".csv", sep=""), quote = F, row.names = T)

        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a4_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a4", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

#FR        
model <- jags.model("04_model_Model_3a5_FR.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)   
summary(posteriors)
        results <- summary(posteriors)
        c1 <- results$quantiles[,1]
        c2 <- results$quantiles[,3]
        c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a5",".csv", sep=""), quote = F, row.names = T)
        
        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a5_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a5", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

#PE       
model <- jags.model("04_model_Model_3a6_PE.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)  
summary(posteriors)
        results <- summary(posteriors)
                c1 <- results$quantiles[,1]
                c2 <- results$quantiles[,3]
                c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a6",".csv", sep=""), quote = F, row.names = T)

        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a6_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a6", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

#PSM
model <- jags.model("04_model_Model_3a7_PSM.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)    
summary(posteriors)
        results <- summary(posteriors)
                c1 <- results$quantiles[,1]
                c2 <- results$quantiles[,3]
                c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a7",".csv", sep=""), quote = F, row.names = T)

        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a7_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a7", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)
 
#PR      
model <- jags.model("04_model_Model_3a8_PR.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)  
summary(posteriors)
        results <- summary(posteriors)
                c1 <- results$quantiles[,1]
                c2 <- results$quantiles[,3]
                c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a8",".csv", sep=""), quote = F, row.names = T)

        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a8_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a8", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

#Other       
model <- jags.model("04_model_Model_3a9_Other.bug", data=read.jagsdata("03_data_jags_Model_3a.R"), n.chains=3)
update(model, how_many_runs)
posteriors <- coda.samples(model, c("mu_bar", "p.crossval"), n.iter=how_many_runs, thin=1)  
summary(posteriors)
        results <- summary(posteriors)
                c1 <- results$quantiles[,1]
                c2 <- results$quantiles[,3]
                c3 <- results$quantiles[,5]
        summary.table <- data.frame(cbind(c1,c2,c3))	
        write.table(summary.table, file = paste("model_3a9",".csv", sep=""), quote = F, row.names = T)
        
        #PPC 
        summary.table2 <- data.frame(cbind(results$statistics[19:259,1]))
        print(summary.table2)
        write.table(summary.table2, file= paste("04_model_Model_3a9_p_new", ".csv", sep=""), quote= F, row.names = T)
        
        gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
        for (v in 1:nvar(posteriors)) {
                gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
        }
        gelman_diag
        diagnostics = paste("gelman_diag_", "04_model_Model_3a9", sep="")
        write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)
        
	
