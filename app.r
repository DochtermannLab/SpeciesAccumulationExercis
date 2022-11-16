library(shiny)
#### Ant Data ####
genera<-c("Acanthognathus",
          "Acanthoponera",
          "Acromyrmex",
          "Acropyga",
          "Adelomyrmex",
          "Amblyopone",
          "Anochetus",
          "Aphaenogaster",
          "Apterostigma",
          "Atta",
          "Azteca",
          "Basiceros",
          "Belonopelta",
          "Brachymyrmex",
          "Camponotus",
          "Cardiocondyla",
          "Carebarella",
          "Centromyrmex",
          "Cephalotes",
          "Cerapachys",
          "Crematogaster",
          "Cylindromyrmex",
          "Cyphomyrmex",
          "Dendromyrmex",
          "Discothyrea",
          "Dolichoderus",
          "Eciton",
          "Ectatomma",
          "Eurhopalothrix",
          "Forelius",
          "Gnamptogenys",
          "Hylomyrma",
          "Hypoponera",
          "Labidus",
          "Lachnomyrmex",
          "Leptogenys",
          "Leptothorax",
          "Linepithema",
          "Megalomyrmex",
          "Monomorium",
          "Mycocepurus",
          "Myrmelachista",
          "Myrmicocrypta",
          "Neivamyrmex",
          "Nomamyrmex",
          "Octostruma",
          "Odontomachus",
          "Oligomyrmex",
          "Pachycondyla",
          "Paraponera",
          "Paratrechina",
          "Pheidole",
          "Platythyrea",
          "Prionopelta",
          "Proceratium",
          "Procryptocerus",
          "Pseudomyrmex",
          "Pyramica",
          "Rhopalothrix",
          "Rogeria",
          "Sericomyrmex",
          "Solenopsis",
          "Stegomyrmex",
          "Stenamma",
          "Strumigenys",
          "Tapinoma",
          "Tatuidris",
          "Tetramorium",
          "Thaumatomyrmex",
          "Trachymyrmex",
          "Tranopelta",
          "Typhlomyrmex",
          "Wasmannia",
          "Xenomyrmex")
prob<-c(0.003669028	,
        0.004428138	,
        0.002277328	,
        0.004175101	,
        0.002403846	,
        0.000506073	,
        0.004807692	,
        0.003036437	,
        0.010754049	,
        0.001518219	,
        0.027834008	,
        0.000885628	,
        0.000126518	,
        0.010880567	,
        0.10159413	,
        0.000759109	,
        0.004428138	,
        0.000126518	,
        0.019230769	,
        0.001771255	,
        0.047950405	,
        0.000253036	,
        0.012145749	,
        0.001771255	,
        0.005440283	,
        0.010501012	,
        0.005313765	,
        0.00923583	,
        0.001138664	,
        0.000126518	,
        0.010374494	,
        0.006325911	,
        0.019483806	,
        0.000759109	,
        0.005566802	,
        0.004175101	,
        0.010754049	,
        0.000126518	,
        0.002277328	,
        0.001012146	,
        0.000126518	,
        0.004554656	,
        0.000126518	,
        0.000759109	,
        0.000253036	,
        0.003922065	,
        0.021508097	,
        0.004428138	,
        0.042889676	,
        0.008729757	,
        0.033906883	,
        0.18041498	,
        0.002530364	,
        0.003162955	,
        0.000506073	,
        0.018598178	,
        0.055921053	,
        0.032894737	,
        0.0013917	,
        0.008097166	,
        0.004934211	,
        0.121077935	,
        0.000126518	,
        0.002024291	,
        0.016700405	,
        0.028087045	,
        0.000506073	,
        0.000379555	,
        0.00215081	,
        0.007970648	,
        0.007338057	,
        0.000506073	,
        0.021887652	,
        0.001644737	
)
ants <- data.frame(cbind(genera,prob))
names(ants) <- c("Genera","prob")
#### Species Accumulation Curve ####

#### UI ####
ui <- fluidPage(
  titlePanel("Estimating Community Diversity"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Ant Sampling"),
      actionButton("action",label="Sample!"),
      h4("Ant Genera Found"),
      textOutput("ants_samp")
    ),
    mainPanel(
      h4(" "),
      h4("Sampling Summary"),
      h4(" "),
      h5("Total Samples:"),
      textOutput("samp_total"),
      h5("Total Genera:"),
      textOutput("unique_total"),
      h3(" "),
      h4("Species Accumulation Curve"),
      plotOutput("spec_curve",width= "100%",height="400px"),
      h3(" "),
      h3(" "),
      h3(" "),
      h3(" "),
      p("Data from Longino, Coddington, & Colwell (2002) The ant fauna of a tropical rain forest: estimating species richness three different ways. Ecology. 83(3):689-702 "),
      p("R and Shiny code available from Ned Dochtermann @ https://github.com/DochtermannLab"),
      p("Code modifiable according to CC BY-NC-ND 4.0")
    )
  )
)

#### server ####
server <- function(input, output) {
  sims <- reactiveValues(ants_samp=NULL,sampled=NULL)
  sims$samples <- 0
  observeEvent(input$action,{
  sims$samples <- sims$samples+1
  sims$ants_samp <- c(sims$ants_samp,sample(ants[,1],size=1,replace=TRUE,prob=ants[,2]))
  sims$sampled <- c(sims$sampled,length(unique(sims$ants_samp)))
  
  #output$ants_samp <- sims$ants_samp
    output$samp_total <- renderText({
    paste(sims$samples)
  })
  output$unique_total <- renderText({
    paste(sims$sampled)
  })
  output$ants_samp <- renderText({paste(sims$ants_samp)})
  output$spec_curve <- renderPlot({
    plot(sims$sampled, type='b',xlab="Samples", ylab="Genera Sampled")
  })
})  
}


# Run the app ####
shinyApp(ui = ui, server = server)
