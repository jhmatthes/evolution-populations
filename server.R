library(shiny)
library(ggplot2)
library(reshape2)

new.p <- function(p, w11, w12, w22){
  wbar <- w11*p^2 + 2*w12*p*(1-p) + w22*(1-p)^2
  p.change <- (p*(1-p)/wbar)*(p*(w11-w12) - (1-p)*(w22-w12))
  return(p + p.change)
}

pheno.freqs <- function(p){
  return(c(p^2, 2*p*(1-p), (1-p)^2))
}


shinyServer(
  function(input, output){
    
    theme_set(theme_minimal(base_size = 16))
    
    # Random genetic drift panel
    output$driftplot <- renderPlot({
      # Set up parameters
      N = input$N # number of diploid individuals
      N.chrom = 2*N # number of chromosomes
      p = input$p; q = 1-p
      N.gen = input$N.gen # number of generations
      N.sim = input$N.sim  # number of simulations
      
      # Simulation
      X <- array(0, dim=c(N.gen,N.sim))
      X[1,] = rep(N.chrom*p,N.sim) # initialize number of A1 alleles in first generation
      for(j in 1:N.sim){
        for(i in 2:N.gen){
          X[i,j] = rbinom(1,N.chrom,prob=X[i-1,j]/N.chrom)
        }  
      }
      X <- data.frame(X/N.chrom)
      colnames(X) <- seq(1:N.sim)
      
      # Reshape data and plot the 5 simulations
      sim_data <- melt(X)
      ggplot(sim_data, 
             aes(x = rep(c(1:N.gen), N.sim), y = value, color = variable)) + 
        geom_line() + 
        xlab("Generation") + 
        ylab("A Allele Frequency") + ylim(0,1) + 
        labs(colour = "Simulations")
    })
    
    output$island1 <- renderPlot({
      
      # CONNECTED ISLAND MODEL
      sim.island <- function(q, migration.rate, num.gens){
        num.islands <- length(q)
        q.history <- data.frame(generation=numeric(), island=numeric(), q=numeric())
        q.history[1:num.islands, ] <- cbind(rep(0, num.islands), 1:num.islands, q)
        
        for (n in 1:num.gens){
          q.last <- q
          for(i in 1:num.islands){
            q[i] <- (1-migration.rate[n])*q.last[i] + migration.rate[n]*(mean(q.last[-i]))
          }
          q.history[n*num.islands+(1:num.islands),] <- cbind(rep(n, num.islands), 1:num.islands, q)
        }
        q.history$island <- as.factor(q.history$island)
        return(q.history)
      }
      q0 <- c(input$af1, input$af2)
      migration.rate <- c(rep(0, input$t_connect),rep(input$migration, 50-input$t_connect))
      results <- sim.island(q=q0, migration.rate = migration.rate, num.gens = 50)
      ggplot(results, aes(generation, q, group=island)) +
        geom_line(aes(color=island)) +
        labs(y = "A Allele Frequency", x = "Generation") +
        scale_color_discrete(name = "Island")
    })
    
    output$natselect <- renderPlot({
      # DIPLOID SELECTION
      plot.allele.history <- function(p, w11, w12, w22, gens=100){
        
        history <- data.frame(generation=numeric(), 
                              cat=character(), Type=character(), frequency=numeric())
        
        for (i in 1:gens){
          phenos <- data.frame(generation=rep(i,3), cat=rep("Phenotypes",3), 
                               Type=c("AA", "Aa", "aa"), freq=pheno.freqs(p))
          alleles <- data.frame(generation=rep(i,2), cat=rep("Alleles",2), 
                                Type=c("A", "a"), freq=c(p, 1-p))
          history <- rbind(history, phenos,alleles)
          p <- new.p(p=p, w11=w11, w12=w12, w22=w22)
        }
        
        g <- ggplot(history, aes(generation, freq, color=Type)) +
          geom_line()+ylim(c(0,1)) +
          facet_grid(cat~.) + 
          labs(y = "Frequency", x = "Generation")
        
        return(g)
      }
      plot.allele.history(p=input$p_ns, 
                          w11=input$w11, w12=input$w12, 
                          w22=input$w22)
    })
    
    output$all <- renderPlot({
      # Set up parameters
      N <- c(input$popsize_1, input$popsize_2) # number of diploid individuals
      N.chrom <- 2*N # number of chromosomes
      
      sim.island <- function(q, w11, w12, w22, migration.rate, num.gens){
        num.islands <- length(q)
        history <- data.frame(generation=numeric(), island=character(), 
                              cat=character(), Type=character(), frequency=numeric())
        
        for (n in 1:num.gens){
          q.last <- q
          for(i in 1:num.islands){
            q.drift <- rbinom(1, N.chrom[i], prob=q.last[i])/N.chrom[i]
            q.flow <- (1-migration.rate[n])*q.drift + migration.rate[n]*(mean(q.last[-i]))
            q.select <- new.p(p=q.flow, w11=w11[i], w12=w12[i], w22=w22[i])
            q[i] <- q.select
          }
          
          phenos <- data.frame(generation=rep(n,3*num.islands),
                               island=rep(paste("Island",as.character(1:num.islands),sep=" "), 3),
                               cat=rep("Phenotypes",3*num.islands), 
                               Type=rep(c("AA", "Aa", "aa"), each = num.islands), 
                               freq=pheno.freqs(q))
          
          alleles <- data.frame(generation=rep(n,2*num.islands), 
                                island=rep(paste("Island",as.character(1:num.islands),sep=" "), 2),
                                cat=rep("Alleles",2*num.islands), 
                                Type=rep(c("A", "a"), each = num.islands), 
                                freq=c(q, 1-q))
          
          history <- rbind(history, phenos, alleles)
        }
        return(history)
      }
      
      w11 <- c(input$w11_1, input$w11_2, input$w11_3)
      w12 <- c(input$w12_1, input$w12_2, input$w12_3)
      w22 <- c(input$w22_1, input$w22_2, input$w22_3)
      q0  <- c(input$p1_all, input$p2_all, input$p3_all)
      
      migration_rate <- c(rep(0, input$t_connect2),rep(input$migration2, 100-input$t_connect2))
      results <- sim.island(q=q0, w11=w11, w12=w12, w22=w22,
                            migration.rate = migration_rate, num.gens = 100)
      
      ggplot(results, mapping = aes(generation, freq, color=Type)) +
        geom_line() +
        ylim(c(0,1)) +
        facet_grid(cat~island) + 
        labs(y = "Frequency", x = "Generation")
    })
      
    
  }
)

