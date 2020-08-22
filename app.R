library(tidyverse)
library(glue)
library(shiny)
library(shinyBS)
library(scales)
library(ggrepel)
library(DT)

RSV_wheeze <- readr::read_csv("./www/scenarios.csv")
RSV_wheeze <- RSV_wheeze %>% mutate(user.added = F, Scenario = 0)

updated <- readr::read_csv("./www/updated_scenarios.csv")

ui <- fixedPage(
  titlePanel('Sample size and number needed to treat estimates for the impact of RSV-LRTI prevention on childhood recurrent wheeze'),
  navbarPage("",
             tabPanel("Overview",
                      mainPanel(
                        tags$head( HTML( '<script async src="https://www.googletagmanager.com/gtag/js?id=UA-100510105-3"></script>
                        <script>
                                                        window.dataLayer = window.dataLayer || [];
                                                        function gtag(){dataLayer.push(arguments);}
                                                        gtag("js", new Date());

                                                        gtag("config", "UA-100510105-3");
                        </script>') ),
                        # END Google Analytics
                      HTML(paste("<b>Overview</b>",
                                 
                                 'There is interest in assessing the relationship 
                      between interventions to prevent acute respiratory 
                      syncytial virus (RSV) lower respiratory tract infection
                      (LRTI) and long-term childhood outcomes, including 
                      childhood recurrent wheeze. In 2018, we published a paper 
                      on behalf of a WHO working group to estimate the minimum 
                      enrollment in clinical trials of maternal RSV vaccination
                      to detect a significant effect on childhood recurrent 
                      wheeze using literature-supported estimates. In July 2020,
                      two trials were published on a long-acting, monoclonal 
                      antibody (mAb) and a maternal RSV F protein (RSV-F) 
                      vaccine. These trials provide an opportunity to 
                      recalibrate our estimates and place them alongside our
                      previously published findings.',  
                                 
                                  "<b>Purpose</b>",
                                 
                                  'The purpose of the tool is to display results 
                      from our paper alongside the updated estimates.
                      In the original study, we combined WHO preferred minimums 
                      for vaccine efficacy with literature-supported estimates 
                      for the RSV attack rate, the baseline risk of childhood 
                      recurrent wheeze, and the risk ratio (RR) for the causal 
                      association between early, severe RSV illness (defined as 
                      RSV-related lower respiratory tract infection or 
                      hospitalization among infants under the age of six months) 
                      and childhood recurrent wheeze in order to estimate: a) 
                      the required minimal sample size to detect an effect of 
                      maternal RSV vaccination on childhood recurrent wheeze, 
                      and b) the number of mother-infant pairs that need to be 
                      vaccinated (NNT) to prevent one case of childhood 
                      recurrent wheeze.',
                                 
                                 'For the updated study, we used estimates of 
                      efficacy and RSV attack rate from one or both of two recently 
                       published trials. We also updated the estimate of the RR 
                       for the causal association between early, severe RSV 
                       illness and childhood recurrent wheeze based on a new
                       meta-analysis and used the three estimates of childhood
                       recurrent wheeze from the original study.',
                                 
                                 "<b>Sample size calculator</b>",
                                 
                       'This application allows you to view our sample size and
                       NNT estimates for these evidence-informed scenarios and 
                       to calculate sample size and NNT for scenarios you would
                       like to add. If you are interested in another outcome 
                       (e.g., childhood asthma) or another exposure (e.g., RSV
                       monoclonals) you can supply estimates most relevant to your 
                       exposure and outcome of interest.',
                       sep = "<br><br>"
                      )
                      )

                      ),
                       sidebarPanel(
                         HTML(paste(
                                    "<b>Please use the following citation:</b>",
                                    
                                    '<a href=https://www.sciencedirect.com/science/article/pii/S0264410X18314075>Riddell CA, Bhat N, Bont LJ, Dupont WD, Feikin DR, Fell DB, Gebretsadik T, Hartert TV, Hutcheon JA, Karron RA, Nair H. Informing randomized clinical trials of respiratory syncytial virus vaccination during pregnancy to prevent recurrent childhood wheezing: a sample size analysis. <i>Vaccine</i>. 2018;36(52):8100-9.</a>',

                                    "<b>Bug reports</b>",
                                    
                                    'Submit any bug reports to: c [dot] riddell [at] berkeley.edu or open an issue on <a href=https://github.com/corinne-riddell/WHO-app/issues>Github</a>.',
                                    sep = "<br><br>")
                         )
                       )
                      
             ),
             tabPanel("Calculator",
                      sidebarPanel(width = 3,
                                   HTML("<b>Outcome graphed:</b>"),
                                   radioButtons(inputId = "outcome_var", label = NA, 
                                                choices = c("Sample size", 
                                                            "Number needed to vaccinate (NNT)"), 
                                                selected = "Sample size"),
                                   hr(),
                                   HTML("<b>Add a scenario:</b><br><br>"),
                                   #sliderInput(inputId = "add_efficacy", label = "Vaccine efficacy (%)", value = 1, min = 1, max = 100, post = " %"),
                                   bsTooltip(id = "add_efficacy", "WHO minimum preferred efficacy is 50%.", placement = "right", trigger = "hover",
                                             options = NULL),
                                   numericInput(inputId = "add_efficacy", label = "Vaccine efficacy (%)", value = 1, min = 0.01, max = 99.9),
                                   numericInput(inputId = "add_attack", label = "Severe RSV attack rate (%)", value = 1, min = 0.01, max = 99.9),
                                   numericInput(inputId = "add_wheeze", label = "Baseline risk of childhood recurrent wheeze (%)", value = 1, min = 0.01, max = 99.9),
                                   numericInput(inputId = "add_RR", label = "RR for RSV-childhood recurrent wheeze", value = 1.1, min = 1, max = 10),
                                   #sliderInput(inputId = "add_attack", label = "RSV attack rate (%)", value = 1, min = 1, max = 100, post = " %", step = 0.5),
                                   bsTooltip(id = "add_attack", "Maternal vaccination is only expected to confer protection to infants against RSV illness in the first weeks or months of life. This attack rate reflects the proportion of infants infected during this early time frame with RSV that can be linked to later childhood recurrent wheeze.", placement = "right", trigger = "hover",
                                             options = list(container = "body")),
                                   #sliderInput(inputId = "add_wheeze", label = "Baseline risk of childhood recurrent wheeze (%)", value = 1, min = 0, max = 100, post = " %", step = 0.5),
                                   bsTooltip(id = "add_wheeze", "Rates of childhood recurrent wheeze are highly variable across countries.", placement = "right", trigger = "hover",
                                             options = list(container = "body")),
                                   #sliderInput(inputId = "add_RR", label = "Risk ratio for RSV-childhood recurrent wheeze", value = 1.1, min = 1, max = 10, step = 0.1),
                                   bsTooltip(id = "add_RR", "Assuming a causal relationship, this risk ratio (RR) estimates the increase in risk of childhood recurrent wheeze associated with having an early, severe RSV illness (defined as an RSV-related lower respiratory tract infection or hospitalization among infants under the age of six months).", placement = "right", trigger = "hover",
                                             options = list(container = "body")),
                                   actionButton(inputId = "ready_to_add", label = "Add scenario"),
                                   actionButton(inputId = "remove_scenarios", label = "Remove additions")
                      ),
                      mainPanel(
                        HTML("<b>Original Findings</b><br>"),
                        HTML(glue("This graphic displays the sample size (per trial arm) 
                         required to detect an effect of maternal vaccination on 
                         childhood recurrent wheeze across 81 pre-defined scenarios.<br><br>
                         
                         Use the control panel to plot the number needed to 
                         vaccinate to prevent one child from developing recurrent 
                         wheeze as the outcome. Add your own scenario to the 
                         plot by setting all the parameter values and clicking
                         'add scenario'.")),
                        tableOutput("table"),
                        hr(),
                        dataTableOutput("data1"),
                        plotOutput("sampleSizePlot2"),
                        hr(),
                        HTML(glue("<b>Data for updated scenarios</b>")),
                        dataTableOutput("updatedEsts"),
                        HTML(glue("<b>User-added scenarios</b>")),
                        dataTableOutput("addedRows"),
                        uiOutput("message"),
                        hr(),
                        HTML(glue("<b>Data for original scenarios</b>")),
                        dataTableOutput("original")
                      ) 
             )  #close tabPanel
  )
)

server <- function(input, output) {     
  
  # The important part of reactiveValues()
  values <- reactiveValues()
  values$df <- RSV_wheeze
  
  observeEvent(input$remove_scenarios, {
    values$df <- RSV_wheeze
  })
  
  addData <- observe({
    if(input$ready_to_add > 0) {
      
      # req(input$add_efficacy, input$add_wheeze, input$add_attack, input$add_RR)
      # if ( is.na( input$add_efficacy ) | is.na( input$add_wheeze ) | is.na( input$add_attack ) | is.na( input$add_RR )) return("Please set all parameter values.")
      # validate(
      #   need(input$add_efficacy != 0),
      #   need(input$add_wheeze != 0),
      #   need(input$add_RR != 1),
      #   need(input$add_attack != 0)
      # )
      
      add.percent.altered <- isolate((input$add_efficacy/100)*(input$add_attack/100))
      
      risk.wheeze.unvacc <- isolate(((input$add_wheeze/100)*(1-input$add_attack/100)) + 
                                      (input$add_wheeze/100*input$add_RR*(input$add_attack/100)))
      
      risk.wheeze.vacc <- isolate(((input$add_wheeze/100)*(1-(input$add_attack/100))) + 
                                    ((input$add_wheeze/100)*add.percent.altered) + 
                                    ((input$add_wheeze/100)*input$add_RR*((input$add_attack/100)-add.percent.altered)))
      
      RR.wheeze.vacc <- isolate(risk.wheeze.vacc/risk.wheeze.unvacc)
      
      samp.size <- isolate(power.prop.test(p1 = risk.wheeze.unvacc,
                                           p2 = risk.wheeze.vacc,
                                           power = 0.8, sig.level = 0.05, 
                                           alternative = "two.sided")$n)
      
      RD.wheeze.vacc <- isolate(risk.wheeze.unvacc - risk.wheeze.vacc)
      
      NNT <- isolate(1/RD.wheeze.vacc)
      
      newLine <- isolate(data.frame("vaccine.efficacy" = input$add_efficacy/100,
                                    "rsv.attack.rate" = input$add_attack/100,
                                    "RR.wheeze.rsvh" = input$add_RR,
                                    "baseline.risk.wheeze" = input$add_wheeze/100,
                                    "percent.altered" = add.percent.altered,
                                    "risk.wheeze.unvacc" = risk.wheeze.unvacc,
                                    "risk.wheeze.vacc" = risk.wheeze.vacc,
                                    "RR.wheeze.vacc" = RR.wheeze.vacc,
                                    "size.one.arm.equal" = samp.size,
                                    "NNT" = NNT,
                                    "RD.wheeze.vacc" = RD.wheeze.vacc,
                                    "Vaccine.efficacy" = paste0(input$add_efficacy, "%"),
                                    "user.added" = T, 
                                    "Scenario" = input$ready_to_add)) 
      names(newLine)[12] <- "Vaccine efficacy"
      
      isolate(values$df <- plyr::rbind.fill(values$df, newLine))
    }
    
  })
  
  figure.1 <- reactive({
    
    if(input$outcome_var == "Sample size"){
      y.min <- min(floor(values$df$size.one.arm.equal), 100)
      y.max <- max(ceiling(values$df$size.one.arm.equal), 4700000)
      
      figure.1 <- ggplot(values$df, aes(y = size.one.arm.equal, x = RR.wheeze.vacc)) +
        geom_line(aes(linetype = factor(baseline.risk.wheeze)), col = "#969696") +
        geom_point(alpha = 0.8, shape = 21, aes(col = user.added, fill = as.factor(rsv.attack.rate), size = RR.wheeze.rsvh)) +
        scale_color_manual(values = c("black", "red")) +
        facet_wrap(~`Vaccine efficacy`, labeller = label_both) +
        guides(fill = guide_legend(title = "RSV attack rate", title.position = "top", override.aes = list(size = 5)),
               linetype = guide_legend(title = "Baseline risk of childhood\nrecurrent wheeze", title.position = "top"),
               size = guide_legend(title = "Risk ratio for childhood\nrecurrent wheeze-RSV", title.position = "top"),
               col = F) +
        scale_size_continuous(breaks = c(1.6, 2.6, 4)) +
        scale_y_log10(breaks = c(y.min, 1000, 10000, 100000, 1000000),
                      labels = c(as.character(y.min), "1,000", "10,000", "100,000", "1,000,000"),
                      limits = c(y.min, y.max)) +
        ylab("Sample size required (per trial arm)") +
        xlab("Risk ratio between vaccination and childhood recurrent wheeze") +
        theme(strip.background = element_rect(fill="white"),
              strip.text.x = element_text(size = 12),
              strip.text.y = element_text(size = 12)) +
        theme_minimal() +
        theme(legend.position = "top") +
        geom_text_repel(data = values$df %>% filter(Scenario > 0), aes(label = Scenario), col = "red")
    }else{
      figure.1 <- ggplot(values$df, aes(y = NNT, x = RR.wheeze.vacc)) +
        geom_line(aes(linetype = factor(baseline.risk.wheeze)), col = "#969696") +
        geom_point(alpha = 0.8, shape = 21, aes(col = user.added, fill = as.factor(rsv.attack.rate), size = RR.wheeze.rsvh)) +
        scale_color_manual(values = c("black", "red")) +
        facet_wrap(~`Vaccine efficacy`, labeller = label_both) +
        guides(fill = guide_legend(title = "RSV attack rate", title.position = "top", override.aes = list(size = 5)),
               linetype = guide_legend(title = "Baseline risk of childhood\nrecurrent wheeze", title.position = "top"),
               size = guide_legend(title = "Risk ratio for childhood\nrecurrent wheeze-RSV", title.position = "top"),
               col = F) +
        scale_size_continuous(breaks = c(1.6, 2.6, 4)) +
        scale_y_log10() + 
        # scale_y_log10(breaks = c(y.min, 1000, 10000, 100000, 1000000),
        #               labels = c(as.character(y.min), "1,000", "10,000", "100,000", "1,000,000"),
        #               limits = c(y.min, y.max)) +
        ylab("Number needed to vaccinate") +
        xlab("Risk ratio between vaccination and childhood recurrent wheeze") +
        theme(strip.background = element_rect(fill="white"),
              strip.text.x = element_text(size = 12),
              strip.text.y = element_text(size = 12)) +
        theme_minimal() +
        theme(legend.position = "top") +
        geom_text_repel(data = values$df %>% filter(Scenario > 0), aes(label = Scenario), col = "red")
    }
    
    return(figure.1)
  })
  
  output$sampleSizePlot2 <- renderPlot({
    figure.1()     
  })
  
  output$updatedEsts <- renderDataTable({
    updated2 <- updated %>% select(vaccine.efficacy, rsv.attack.rate, RR.wheeze.rsvh, 
                                   baseline.risk.wheeze, RR.wheeze.vacc, NNT, size.one.arm.equal) %>% 
      mutate(RR.wheeze.vacc = round(RR.wheeze.vacc, 3)) %>%
      arrange(vaccine.efficacy, baseline.risk.wheeze)
    
    datatable(updated2 %>% mutate(NNT = ceiling(NNT), 
                                  size.one.arm.equal = ceiling(size.one.arm.equal)), 
              colnames = c("Vaccine efficacy", "RSV attack rate", 
                           "RR for RSV-wheeze", "Baseline risk of childhood recurrent wheeze",
                           "RR vaccine-wheeze", "NNT", "Sample size (per arm)"),
              rownames = F) %>% 
      formatCurrency("size.one.arm.equal", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("vaccine.efficacy", 0) %>%
      formatCurrency("NNT", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("rsv.attack.rate", 1) %>%
      formatPercentage("baseline.risk.wheeze", 1)     
  })
  
  output$addedRows <- renderDataTable({
    validate(
     need(sum(values$df$user.added)!=0, message = "Please add a scenario to calculate sample size." )
    )
    
      modified.table <- values$df %>% 
        filter(user.added == T) %>% 
        select(Scenario, vaccine.efficacy, rsv.attack.rate, RR.wheeze.rsvh, 
               baseline.risk.wheeze, RR.wheeze.vacc, NNT, size.one.arm.equal) %>% 
        mutate(RR.wheeze.vacc = round(RR.wheeze.vacc, 3)) 
      
      datatable(modified.table, 
                colnames = c("Scenario", "Vaccine efficacy", "RSV attack rate", 
                             "RR for RSV-wheeze", "Baseline risk of childhood recurrent wheeze",
                             "RR vaccine-wheeze", "NNT", "Sample size (per arm)"),
                rownames = F) %>% 
        formatCurrency("size.one.arm.equal", currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatPercentage("vaccine.efficacy", 0) %>%
        formatCurrency("NNT", currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatPercentage("rsv.attack.rate", 1) %>%
        formatPercentage("baseline.risk.wheeze", 1)
  })
  
  output$message <- renderUI({
    req(input$ready_to_add)
    
    HTML("RR=Risk ratio, NNT=Number needed to treat (vaccine) to prevent one case of childhood recurrent wheeze")
  })
  
  output$original <- renderDataTable(
    values$df %>% 
      filter(user.added == F) %>%
      select(vaccine.efficacy, rsv.attack.rate, RR.wheeze.rsvh, 
             baseline.risk.wheeze, RR.wheeze.vacc, NNT, size.one.arm.equal) %>% 
      mutate(RR.wheeze.vacc = round(RR.wheeze.vacc, 3)) %>%
      datatable(colnames = c("Vaccine efficacy", "RSV attack rate", 
                             "RR for RSV-wheeze", "Baseline risk of childhood recurrent wheeze",
                             "RR vaccine-wheeze", "NNT", "Sample size (per arm)"),
                rownames = F) %>% 
      formatCurrency("size.one.arm.equal", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("vaccine.efficacy", 0) %>%
      formatCurrency("NNT", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("rsv.attack.rate", 1) %>%
      formatPercentage("baseline.risk.wheeze", 1)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

