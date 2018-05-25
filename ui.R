library(shiny)

shinyUI(fluidPage(
  
  h3("Modeling the Mechanisms of Evolution"),
  
  tabsetPanel(
    tabPanel("Description",
             br(),
             p("This app highlights mathematical models that describe the population-level evolutionary processes of genetic drift, gene flow, and natural selection. Together with random mutation, which happens within individuals, these are the mechanisms that cause populations to evolve through time.",
               "The last simulation - 'All Mechanisms' - represents the combined effects of these processes on populations within two islands."),
             p("The models simulate changes in the frequency of two alleles, A and a, of a diploid organism through many generations of a population. Diploid combinations of these two alleles produce three possible phenotypes, described by the genotypes AA, Aa, and aa."),
             p("These models, like all models, are simplified versions of reality, but are nonetheless useful tools for understanding complex mechanisms.",
               "The models all assume non-overlapping generations, a constant population size (once the simulation starts), random mating, an equal sex ratio, and no crossing-over (recombination) during meiosis."),
    tags$hr(),
    tags$footer("Shiny app developed by: "),
    tags$footer("Jackie Hatala Matthes, Assistant Professor of Biological Sciences, Wellesley College (2018) ",
                a("Website", href="http://matthesecolab.com", target="_blank"),
                p(" "),
                tags$img(alt="Creative Commons License", style="border-width:0", src="https://i.creativecommons.org/l/by/4.0/88x31.png"),
                tags$body("This work is licensed under a", a("Creative Commons Attribution 4.0 International License", href="http://creativecommons.org/licenses/by/4.0/"),"."))),
    
tabPanel("Genetic Drift",
             fluidRow(
               column(3, 
                      sliderInput("N", label = h6("Population size"), min = 10, 
                                  max = 500, value = 100)),
               column(3,
                      sliderInput("p", label = h6("Frequency of A allele (a = 1-A)"), min = 0, 
                                  max = 1, value = 0.5)),
               column(3,
                      sliderInput("N.gen", label = h6("Number of generations"), min = 10, 
                                  max = 200, value = 50)),
               column(3, 
                      sliderInput("N.sim", label = h6("Number of simulations"), min = 1, 
                                  max = 20, value = 5)),
             fluidRow(
               column(12, plotOutput("driftplot"))
             )),
             p(" "),
             p("The figure above shows results from the Wright-Fisher (WF) model of genetic drift.", 
             a("Genetic drift", href="https://en.wikipedia.org/wiki/Genetic_drift"),
             "describes random changes in allele frequencies (A and a) due to the fact that not all 
               individuals in a population necessarily reproduce, and/or that some individuals have more than one offspring."),
             p("The WF model simulates genetic drift of two alleles, A and a, and assumes that generations do not 
             overlap within a population of size N of diploid individuals (2N alleles). 
             The WF model simulates the allele frequency of A as a", 
             a("binomial random variable", href = "https://en.wikipedia.org/wiki/Binomial_distribution"),
             ", which is analogous to a coin toss for whether or not an organism passes on an A allele, which is repeated for the whole population.
             The allele frequency of a is 1 minus the A allele frequency."),
             p("The multiple lines in the figure above show different iterations of the WF model simulation
               with the same set of starting conditions. The lines differ because genetic drift is a probablistic (i.e., random) process,
               which means that each result is unique.")),
    
    tabPanel("Gene Flow",
             fluidRow(
               column(3,
                      sliderInput("af1", label = h6("Allele Frequency, Island 1"), min = 0, 
                                  max = 1, value = 1.0)),
               column(3,
                      sliderInput("af2", label = h6("Allele Frequency, Island 2"), min = 0, 
                                  max = 1, value = 0.2)),
               column(3,
                      sliderInput("t_connect", label = h6("Connect Populations at Generation"), min = 0, 
                                  max = 50, value = 0.1)),
               column(3, 
                        sliderInput("migration", label = h6("Migration Rate After Connection"), min = 0, 
                                    max = 0.5, value = 0.1))),
               fluidRow(
                 column(12, plotOutput("island1"))),
                 p(" "),
                 p("The figure above shows a simulation from a model of", a("gene flow", href = "https://en.wikipedia.org/wiki/Gene_flow"),
                   ", which occurs when individuals from two (or more) formerly genetically separate populations migrate and reproduce.
                   You may control the time at which the two populations are connected by the 'Connect Populations at Generation' slider, and 
                   after the populations are connected, the rate of reproduction across the two populations is determined by the 'Migration Rate'.
                   This model is deterministic, which means that each simulation with the same starting conditions and parameter values yields the same result.")),
    
    tabPanel("Natural Selection",
             fluidRow(
               column(3,
                      sliderInput("p_ns", label = h6("Frequency of A allele (a = 1-A)"), min = 0, 
                                  max = 1, value = 0.9)),
               column(3,
                      sliderInput("w11", label = h6("AA Phenotype Fitness"), min = 0, 
                                  max = 1, value = 0.8)),
               column(3,
                      sliderInput("w12", label = h6("Aa Phenotype Fitness"), min = 0, 
                                  max = 1, value = 0.8)),
               column(3, 
                      sliderInput("w22", label = h6("aa Phenotype Fitness"), min = 0, 
                                  max = 1, value = 1.0))),
               fluidRow(
                 column(12, plotOutput("natselect"))),
               p(" "),
               p("The two figures above show results from a simulation from a model of", a("natural selection", href = "https://en.wikipedia.org/wiki/Natural_selection"),
                 ". Natural selection operates on the traits of organisms, which are represented as the phenotype and are coded as the genotype (i.e., two alleles in a diploid organism)."),
               p("In this model, the survival of offspring with different phenotypes (AA, Aa, or aa) in each generation is scaled by a fitness value between zero (no survival) and 1 (all survive).
                 Over time, phenotypes with lower fitness are more likely to disappear from the population as their population size is reduced due to lower offspring survival.
                 This model is deterministic, which means that each simulation with the same starting conditions and parameter values yields the same result.")
               ),
    
    tabPanel("All Mechanisms",
             fluidRow(
               column(12, plotOutput("all"))),
             tags$br(),
             fluidRow(
               column(4,
                      sliderInput("t_connect2", label = h6("Connect Islands at Generation"),
                                  min = 0, max = 100, value = 0)),
               column(4,
                      sliderInput("migration2", label = h6("Migration Rate [proportion popn/gen]"),
                                  min = 0, max = 0.5, value = 0.05))
             ),
             h5("Island 1:"),
             fluidRow(
               column(2,
                      sliderInput("p1_all", label = h6("Frequency of A allele (a = 1-A)"), 
                                  min = 0, max = 1, value = 0.9)),
               column(2, 
                      sliderInput("popsize_1", label = h6("Population Size"), min = 0, 
                                  max = 500, value = 500)),
               column(2,
                      sliderInput("w11_1", label = h6("AA Phenotype Fitness"), 
                                  min = 0, max = 1, value = 0.8)),
               column(2,
                      sliderInput("w12_1", label = h6("Aa Phenotype Fitness"), 
                                  min = 0, max = 1, value = 0.9)),
               column(2, 
                      sliderInput("w22_1", label = h6("aa Phenotype Fitness"), min = 0, 
                                  max = 1, value = 1.0))),
             h5("Island 2:"),
              fluidRow(
               column(2,
                      sliderInput("p2_all", label = h6("Frequency of A allele (a = 1-A)"), 
                                  min = 0, max = 1, value = 0.8)),
               column(2, 
                      sliderInput("popsize_2", label = h6("Population Size"), min = 0, 
                                  max = 500, value = 50)),
               column(2,
                      sliderInput("w11_2", label = h6("AA Phenotype Fitness"), min = 0, 
                                  max = 1, value = 0.9)),
               column(2,
                      sliderInput("w12_2", label = h6("Aa Phenotype Fitness"), min = 0, 
                                  max = 1, value = 0.5)),
               column(2, 
                      sliderInput("w22_2", label = h6("aa Phenotype Fitness"), min = 0, 
                                  max = 1, value = 0.2))),
             p(" "),
             p("This model combines all three population-level evolutionary processes - genetic drift, gene flow, and natural selection - to simulate their 
               concurrent effects on allele frequencies of two islands. You can read more about each process on the individual process model tabs.")
               
    ))

  
))
