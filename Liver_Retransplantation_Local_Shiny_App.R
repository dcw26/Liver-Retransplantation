### Add the required libraries
library(randomForestSRC) 
library(shinydashboard)
library(pec)
library(tableHTML)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(htmltools)
library(ggrepel)

# V7 update: modifying percentages on 1-donor plots

### Read in saved data
file.dir <- "C:/Users/wall0518/Documents/Chinnakotla/Liver Retransplantation/Shiny app/"

load(paste0(file.dir,'tx'))
load(paste0(file.dir,'RSF_object_partial')) # Model m2
load(paste0(file.dir,'RSF_object_for_prediction_retx_full')) # Model m1


############################
# Start of ui.R file
############################

modelvarwidth <- 10

ui <- shinyUI(
  fluidPage(
    ##### JS code
    tags$script(src = "source/html5shiv-printshiv.js"),
    tags$script(src = "source/umnhf-2015.js"),
    ##### Creates search bar on top of page
    titlePanel(
      includeCSS("C:/Users/wall0518/Documents/Shiny/MELD40-no RSF object/source/shiny.css"),
      includeHTML("C:/Users/wall0518/Documents/Shiny/MELD40-no RSF object/source/umn_header.html")
    ),
    useShinyjs(),
    navbarPage(title=HTML("<a href='https://shiny.biostat.umn.edu/Transplant/'
                          style='color: black; font-size: 11pt;'> &#8592 Back to Home</a>"), 
               ##### "About" tab
               tabPanel("About",
                        h1(id="main-title", "Liver Re-Transplantation Risk Models"),
                        #h2("About"),
                        #HTML("<a href='https://shiny.biostat.umn.edu/Transplant/'> &#8592 Back to Home </a>"),
                        fluidRow(
                          tags$style(make_css(list('.box #box1', 
                                                   c('border', 'border-radius', 'text-align', 'box-shadow', 'background-color', 'color', 'padding'), 
                                                   c('2px solid #7A0019', '10px', 'left', '5px 6px #FFD75F', "white", 'black', '5px')))),
                          box(width=12, id="box1",
                              HTML('<b><span style="font-size:20px">About this Application:</span></b><br> This app predicts 1-year and 5-year survival for liver transplant 
                     	recipients receiving at least their second liver transplant.
                     	To get started, click on the Risk Model Inputs tab. First, choose to enter variable data limited to only the variables with highest calculated importance
                     	(in other words, variables with the strongest influence on the prediction model), or choose to enter variable data for all information utilized in the study.
                     	Note that variables not included in the list of "highest importance variables" will have little impact on the survival curve when modified.
                     	Select either "Highest importance variables" or "All variables" at the top of the screen to make this selection. Next, select whether to plot 1-year or 5-year
                     	survival of the recipient which will be plotted on the "Survival Plot" tab. Finally, enter characteristics for the potential donor(s) and the recipient. 
                     	The application allows the user to compare multiple potential donors at once.	To view the computed results, click on the "Survival Plot" tab.'),
                              br(),
                              br(),
                              HTML('<b><span style="font-size:20px">Project Abstract:</span></b><br>
                                    <b>Background:</b> Liver re-transplantation (rLT) may be the only option for patients with acute or chronic liver graft failure. 
				         However, rLT remains a formidable technical challenge, and the results are inferior compared to primary liver transplantation (LT). 
				 	 In addition, each liver graft allocated to rLT means another patient on the waitlist is not transplanted. 
					 We sought to identify and develop a prognostic model for 1-year and 5-year survival outcomes in rLT. <br> 
                                    <b>Methods:</b> Data were obtained from the Scientific Registry of Transplant Recipients, focusing on adult rLT procedures (patient age >18 years) from 2010 through 2022. 
					 37 recipient, donor, and perioperative factors were identified as potential predictors of transplant outcomes. 
					 The relationships between these factors and patient survival were estimated using Random Survival Forests. <br>
                                    <b>Results:</b> Strongest predictors of futility include patient on life-support, number of previous liver transplants, recipient Karnofsky score, 
           donor / recipient age, recipient BMI, and cold ischemia time for the liver.
 					 Overall adjusted 1-year patient survival improved from 80.2% in 2010 to a peak of 83.3% in 2019; however, survival has since decreased to 79.5% in 2022.
					 <br> 
                                    <b>Conclusions:</b> rLT continues to increase in prevalence, and recent years have seen a decrease in adjusted rLT patient survival. 
					 Applying ensemble learning methods, recipient and donor factors available before transplant can predict survival probabilities for future rLT cases.
					 This information can be used to facilitate donor and recipient selection to improve patient survival after rLT.
                                  '),
                              br(),
                              br(),
                              HTML('<b><span style="font-size:20px">Notes:</span></b><br> The data used to create this app comes from the SRTR, utilizing outcomes from 2010 through 2022. 
                        The original dataset of 3760 recipients was 63% male and 66% white with a 
                        median age of 53 years old. </u>'
                                   #      <br><br><p><b>Key References:</b><ul> tbd'
                                   #     <li><a href = "https://pubmed.ncbi.nlm.nih.gov/32740239/">Evans et al. (2020) </a></li>
                              )
                          )
                        ),
                        br(),
                        fluidRow(
                          tags$style(make_css(list('.box #box2', 
                                                   c('border', 'border-radius', 'text-align', 'box-shadow', 'background-color', 'color', 'padding'), 
                                                   c('2px solid #7A0019', '10px', 'center', '5px 6px #FFD75F', "white", '#7A0019', '5px')))),
                          box(width = 6, id="box2",
                              h4("Disclaimer"),
                              HTML('<p><span style="color:black;font-size:15px">This app was created as a reference but does not 
                                           constitute medical advice. Please consult your healthcare provider 
                                           for more information.</span></p>'))
                        ),
                        HTML("<hr><span style='font-size:12px;color:#606060'> <b>Author Contributions</b><br>
                                     <p style='margin-bottom: 0px'> Models and content based on work conducted by Vock D, Chinnakotla S, and Waller D <br>
                                     Web development by Waller D with assistance from Evans M<br> <br>
                                     All applications are created and maintained by the transplant
                                     surgery research group comprised of</p>
                                     <p style='margin-left: 40px; margin-top: 0px'>Matas A, Chinnakotla S, Helgeson E, Vock D, Evans M, Palzer EF, and Waller D</p></span>"),
                        includeHTML("C:/Users/wall0518/Documents/Shiny/MELD40-no RSF object/source/umn_footer.html")
               ),
               
               ##### "Risk Model Inputs" panel
               tabPanel("Risk Model Inputs",
                        h1(id="tab-title", "Entering Characteristics for Donor and Recipient"),
                        fluidRow(selectInput("vars_used", "Enter values for highest importance variables only, or values for all study variables?",
                                             c("Highest importance variables", "All variables"))),
                        fluidRow(selectInput("one_or_five_year", "Plot 1-year survival or 5-year survival?",
                                             c("1-Year Survival", "5-Year Survival"))),
                        column(2,
                               fluidRow(id = "limdonor",
                                        fluidRow(
                                          HTML('<span style="font-size:18px"><b><u>Donor Characteristics</b></u></span>'),
                                          selectInput("don_num", "Select the number of donors you want to compare", c(1,2,3))),
                                        fluidRow(id = "donor1",
                                                 column(modelvarwidth,
                                                        HTML('<span style="font-size:16px"><b><u>Donor 1:</u></b></span>'),
                                                        numericInput("don_age1", label = "Age (years)", value = 30, min = 18),
                                                        selectInput("don_type1", "Death Category", levels(tx$DON_NON_HR_BEAT_F))
                                                 )),
                                        fluidRow(id = "donor2",
                                                 column(modelvarwidth,
                                                        HTML('<span style="font-size:16px"><b><u>Donor 2:</u></b></span>'),
                                                        numericInput("don_age2", label = "Age (years)", value = 30, min = 18),
                                                        selectInput("don_type2", "Death Category", levels(tx$DON_NON_HR_BEAT_F))
                                                 )),
                                        fluidRow(id = "donor3",
                                                 column(modelvarwidth,
                                                        HTML('<span style="font-size:16px"><b><u>Donor 3:</u></b></span>'),
                                                        numericInput("don_age3", label = "Age (years)", value = 30, min = 18),
                                                        selectInput("don_type3", "Death Category", levels(tx$DON_NON_HR_BEAT_F))
                                                 ))),
                               fluidRow(id = "alldonor",
                                        fluidRow(
                                          HTML('<span style="font-size:18px"><b><u>Donor Characteristics</b></u></span>'),
                                          selectInput("don_num2", "Select the number of donors you want to compare", c(1,2,3))),
                                        fluidRow(id = "donor11",
                                                 column(modelvarwidth,
                                                        HTML('<span style="font-size:16px"><b><u>Donor 1:</u></b></span>'),
                                                        numericInput("don_age1a", label = "Age (years)", value = 30, min = 18),
                                                        selectInput("don_gender1a", "Gender", levels(tx$DON_GENDER_F)),
                                                        numericInput("donhgt1a", label = "Height (cm)", value = 172, min = 0),
                                                        selectInput("don_type1a", "Death Category", levels(tx$DON_NON_HR_BEAT_F)),
                                                        selectInput("don_cod1a", "Cause of Death", levels(tx$DON_CAD_DON_COD_F)),
                                                        selectInput("don_diab1a", "Diabetes", levels(tx$DON_INSULIN_ALT_F))
                                                 )),
                                        fluidRow(id = "donor22",
                                                 column(modelvarwidth,
                                                        HTML('<span style="font-size:16px"><b><u>Donor 2:</u></b></span>'),
                                                        numericInput("don_age2a", label = "Age (years)", value = 30, min = 18),
                                                        selectInput("don_gender2a", "Gender", levels(tx$DON_GENDER_F)),
                                                        numericInput("donhgt2a", label = "Height (cm)", value = 172, min = 0),
                                                        selectInput("don_type2a", "Death Category", levels(tx$DON_NON_HR_BEAT_F)),
                                                        selectInput("don_cod2a", "Cause of Death", levels(tx$DON_CAD_DON_COD_F)),
                                                        selectInput("don_diab2a", "Diabetes", levels(tx$DON_INSULIN_ALT_F))
                                                 )),
                                        fluidRow(id = "donor33",
                                                 column(modelvarwidth,
                                                        HTML('<span style="font-size:16px"><b><u>Donor 3:</u></b></span>'),
                                                        numericInput("don_age3a", label = "Age (years)", value = 30, min = 18),
                                                        selectInput("don_gender3a", "Gender", levels(tx$DON_GENDER_F)),
                                                        numericInput("donhgt3a", label = "Height (cm)", value = 172, min = 0),
                                                        selectInput("don_type3a", "Death Category", levels(tx$DON_NON_HR_BEAT_F)),
                                                        selectInput("don_cod3a", "Cause of Death", levels(tx$DON_CAD_DON_COD_F)),
                                                        selectInput("don_diab3a", "Diabetes", levels(tx$DON_INSULIN_ALT_F))
                                                 ))                                    
                               )
                        ),
                        column(1,fluidRow()), # Create space between data entry columns
                        column(2, 
                               fluidRow(id = "limvar1",
                                        HTML('<span style="font-size:18px"><b><u>Recipient Characteristics</b></u></span>'),
                                        numericInput("rec_age", label = "Age (years)", value = 30, min = 18),
                                        numericInput("rec_bmi", label = "BMI (kg / m2)", value = 27, min = 15),
                                        selectInput("bltype", "Blood Type", levels(tx$CAN_ABO_ALT_F)),
                                        selectInput("ltcount", "Number of Liver Transplants", levels(tx$LTX_NUMBER)),
                                        selectInput("disgrp", "Disease Group", levels(tx$REC_DGN_CAT_F)),
                                        selectInput("lsmc", "Life Support / ICU / Hospital Status", levels(tx$REC_LS_MC))
                               ),
                               fluidRow(id = "allvar1",
                                        HTML('<span style="font-size:18px"><b><u>Recipient Characteristics</b></u></span>'),
                                        numericInput("rec_agea", label = "Age (years)", value = 30, min = 18),
                                        selectInput("rec_gendera", "Gender", levels(tx$CAN_GENDER_F)),
                                        numericInput("rec_hgta", label = "Height (cm)", value = 172, min = 50),
                                        numericInput("rec_bmia", label = "BMI (kg / m2)", value = 27, min = 15),
                                        selectInput("rec_edua", "Education", levels(tx$CAN_EDUCATION_COLLEGE_F)),
                                        selectInput("rec_insa", "Insurance", levels(tx$REC_PRIVATE_INS_F)),
                                        selectInput("bltypea", "Blood Type", levels(tx$CAN_ABO_ALT_F)),
                                        selectInput("ltcounta", "Number of Liver Transplants", levels(tx$LTX_NUMBER)),
                                        selectInput("disgrpa", "Disease Group", levels(tx$REC_DGN_CAT_F)),
                                        selectInput("lsmca", "Life Support / ICU / Hospital Status", levels(tx$REC_LS_MC)),
                                        selectInput("melda", "MELD Category", levels(tx$MELD_cat_F)),
                               )),
                        column(1,fluidRow()), # Create space between data entry columns
                        column(2,
                               fluidRow(id = "limvar2",
                                        HTML('<span style="font-size:18px"><b><u>Recipient Characteristics (Continued)</b></u></span>'),
                                        selectInput("meld", "MELD Category", levels(tx$MELD_cat_F)),           
                                        selectInput("diab", "Diabetes", levels(tx$CAN_DIAB_TY_ALT_F)),
                                        selectInput("asc", "Ascites", levels(tx$CAN_LAST_ASCITES_ALT_F)),
                                        selectInput("pvthromb", "Portal Vein Thrombosis", levels(tx$REC_PORTAL_VEIN_ALT_F)),
                                        selectInput("tipss", "TIPSS", levels(tx$REC_TIPSS_ALT_F)),
                                        selectInput("egfrdial", "eGFR / Dialysis Status", levels(tx$REC_EGFR_DIAL))
                               ),
                               fluidRow(id = "allvar2",
                                        HTML('<span style="font-size:18px"><b><u>Recipient Characteristics (Continued)</b></u></span>'),
                                        selectInput("diaba", "Diabetes", levels(tx$CAN_DIAB_TY_ALT_F)),
                                        selectInput("asca", "Ascites", levels(tx$CAN_LAST_ASCITES_ALT_F)),
                                        selectInput("pvthromba", "Portal Vein Thrombosis", levels(tx$REC_PORTAL_VEIN_ALT_F)),
                                        selectInput("tipssa", "TIPSS", levels(tx$REC_TIPSS_ALT_F)),
                                        selectInput("prevmaliga", "Previous Malignancy", levels(tx$CAN_MALIG_ALT_F)),
                                        selectInput("egfrdiala", "eGFR / Dialysis Status", levels(tx$REC_EGFR_DIAL)),
                                        selectInput("karna", "Karnofsky Score", levels(tx$REC_FUNCTN_STAT_F)),
                                        selectInput("survtimea", "Previous Graft Survival Time (days)", levels(tx$prev_graft_surv_category)),
                                        numericInput("ischa", label = "Cold Ischemia Time (hours)", value = 5, min = 0),
                                        numericInput("txyeara", label = "Transplant Year", value = 2020, min = 2010, max = 2022),
                                        selectInput("wholespla", "Whole / Split Liver", levels(tx$REC_TX_PROCEDURE_TY_F)),
                                        selectInput("orgtxa", "Organs Transplanted", levels(tx$REC_TX_ORG_TY))
                               )),
                        column(1,fluidRow()), # Create space between data entry columns
                        column(2, 
                               fluidRow(id = "limvar3",
                                        HTML('<span style="font-size:18px"><b><u>Recipient Characteristics (Continued)</b></u></span>'),
                                        selectInput("karn", "Karnofsky Score", levels(tx$REC_FUNCTN_STAT_F)),
                                        selectInput("survtime", "Previous Graft Survival Time (days)", levels(tx$prev_graft_surv_category)),
                                        numericInput("isch", label = "Cold Ischemia Time (hours)", value = 5, min = 0),
                                        selectInput("wholespl", "Whole / Split Liver", levels(tx$REC_TX_PROCEDURE_TY_F)),        
                                        selectInput("infect", "Previous Liver Failure Due To Infection", levels(tx$TFL_FAIL_INFECT_Y_F)),
                                        selectInput("noncomp", "Previous Liver Failure Due To Noncompliance", levels(tx$TFL_FAIL_PX_NONCOMP_Y_F))
                               ),
                               fluidRow(id = "allvar3",
                                        HTML('<span style="font-size:18px"><b><u>Recipient Previous Liver Failure Causes</b></u></span>'),
                                        selectInput("biliarya", "Biliary", levels(tx$TFL_FAIL_BILIARY_Y_F)),
                                        selectInput("denovoa", "Hepatitis De Novo", levels(tx$TFL_FAIL_HEP_DENOVO_Y_F)),
                                        selectInput("hrecura", "Hepatitis Recurrence", levels(tx$TFL_FAIL_HEP_RECUR_Y_F)),
                                        selectInput("infecta", "Infection", levels(tx$TFL_FAIL_INFECT_Y_F)),
                                        selectInput("primea", "Primary Graft Failure", levels(tx$pnf_or_14dayfail)),
                                        selectInput("noncompa", "Noncompliance", levels(tx$TFL_FAIL_PX_NONCOMP_Y_F)),
                                        selectInput("drecura", "Disease Recurrence", levels(tx$TFL_FAIL_RECUR_DISEASE_Y_F)),
                                        selectInput("acutea", "Acute Rejection", levels(tx$TFL_FAIL_REJ_ACUTE_Y_F)),
                                        selectInput("chronica", "Chronic Rejection", levels(tx$TFL_FAIL_REJ_CHRONIC_Y_F)),
                                        selectInput("thromba", "Vascular Thrombosis", levels(tx$TFL_FAIL_VASC_THROMB_Y_F))
                               )),
                        fluidRow(
                          column(modelvarwidth,
                                 plotOutput("donorselect"),
                                 plotOutput("donorselect2"),
                                 plotOutput("varselect")
                          )),
                        
                        br(),
                        includeHTML("C:/Users/wall0518/Documents/Shiny/MELD40-no RSF object/source/umn_footer.html")
                        
               ), # End of tabPanel for the "Risk Model Inputs" panel
               
               #### Survival Plot panel
               tabPanel("Survival Plot",
                        h1("Survival After Re-Transplant"),
                        mainPanel(width=12,
                                  
                                  fluidRow(
                                    column(12, plotOutput("survcurve")),
                                  ),
                                  HTML('<b>About this model:</b><br> All covariates listed in the Risk Model Inputs tab were adjusted for in this model. 
				This model was constructed from a random survival forest of 1000 trees with 40 candidate predictors. 
				The selected covariate values in the Risk Model Inputs tab were utilized by this random survival forest model to calculate the estimated survival shown here.'),
                        ),
                        br(),
                        br(),
                        includeHTML("C:/Users/wall0518/Documents/Shiny/MELD40-no RSF object/source/umn_footer.html")
                        
               ) # End of tabPanel for the "Survival Plot" panel
               
    ) # End of navbarPage
  ) # End of fluidPage
) # End of ShinyUI

############################
# End of ui.R file
############################

############################
# Start of server.R file
############################


server <- function(input, output) {
  
  output$donorselect <- renderPlot({
    
    observeEvent(input$don_num, {
      if(input$don_num == 1){
        show(id="donor1")
        hide(id="donor2")
        hide(id="donor3")
      }else if(input$don_num == 2){
        show(id="donor1")
        show(id="donor2")
        hide(id="donor3")
      }else if(input$don_num == 3){
        show(id="donor1")
        show(id="donor2")
        show(id="donor3")
      }
    })
    
  })
  
  output$donorselect2 <- renderPlot({
    
    observeEvent(input$don_num2, {
      if(input$don_num2 == 1){
        show(id="donor11")
        hide(id="donor22")
        hide(id="donor33")
      }else if(input$don_num2 == 2){
        show(id="donor11")
        show(id="donor22")
        hide(id="donor33")
      }else if(input$don_num2 == 3){
        show(id="donor11")
        show(id="donor22")
        show(id="donor33")
      }
    })
    
  })
  
  output$varselect <- renderPlot({
    
    observeEvent(input$vars_used, {
      if(input$vars_used == "Highest importance variables"){
        show(id="limdonor")
        show(id="limvar1")
        show(id="limvar2")
        show(id="limvar3")
        hide(id="alldonor")
        hide(id="allvar1")
        hide(id="allvar2")
        hide(id="allvar3")
      }else if(input$vars_used == "All variables"){
        hide(id="limdonor")
        hide(id="limvar1")
        hide(id="limvar2")
        hide(id="limvar3")
        show(id="alldonor")
        show(id="allvar1")
        show(id="allvar2")
        show(id="allvar3")
      }
    })
    
  })
  
  output$survcurve <- renderPlot({
    
    
    if(input$vars_used == "Highest importance variables"){
      
      dfnew1 <- data.frame(REC_AGE_IN_YEARS_AT_TX = input$rec_age,
                           CAN_ABO_ALT_F = input$bltype,
                           REC_DGN_CAT_F = input$disgrp,
                           REC_LS_MC = input$lsmc,
                           MELD_cat_F = input$meld,
                           REC_BMI_CLEAN = input$rec_bmi,
                           CAN_DIAB_TY_ALT_F = input$diab,
                           CAN_LAST_ASCITES_ALT_F = input$asc,
                           REC_PORTAL_VEIN_ALT_F = input$pvthromb,
                           REC_TIPSS_ALT_F = input$tipss, 
                           DON_AGE = input$don_age1,
                           DON_NON_HR_BEAT_F  = input$don_type1,
                           REC_COLD_ISCH_TM = input$isch,
                           REC_TX_PROCEDURE_TY_F = input$wholespl,
                           TFL_FAIL_INFECT_Y_F = input$infect,
                           TFL_FAIL_PX_NONCOMP_Y_F = input$noncomp,
                           LTX_NUMBER = input$ltcount,
                           prev_graft_surv_category = input$survtime,
                           REC_EGFR_DIAL = input$egfrdial,
                           REC_FUNCTN_STAT_F = input$karn)
      
      dfnew2 <- data.frame(REC_AGE_IN_YEARS_AT_TX = input$rec_age,
                           CAN_ABO_ALT_F = input$bltype,
                           REC_DGN_CAT_F = input$disgrp,
                           REC_LS_MC = input$lsmc,
                           MELD_cat_F = input$meld,
                           REC_BMI_CLEAN = input$rec_bmi,
                           CAN_DIAB_TY_ALT_F = input$diab,
                           CAN_LAST_ASCITES_ALT_F = input$asc,
                           REC_PORTAL_VEIN_ALT_F = input$pvthromb,
                           REC_TIPSS_ALT_F = input$tipss, 
                           DON_AGE = input$don_age2,
                           DON_NON_HR_BEAT_F  = input$don_type2,
                           REC_COLD_ISCH_TM = input$isch,
                           REC_TX_PROCEDURE_TY_F = input$wholespl,
                           TFL_FAIL_INFECT_Y_F = input$infect,
                           TFL_FAIL_PX_NONCOMP_Y_F = input$noncomp,
                           LTX_NUMBER = input$ltcount,
                           prev_graft_surv_category = input$survtime,
                           REC_EGFR_DIAL = input$egfrdial,
                           REC_FUNCTN_STAT_F = input$karn)
      
      dfnew3 <- data.frame(REC_AGE_IN_YEARS_AT_TX = input$rec_age,
                           CAN_ABO_ALT_F = input$bltype,
                           REC_DGN_CAT_F = input$disgrp,
                           REC_LS_MC = input$lsmc,
                           MELD_cat_F = input$meld,
                           REC_BMI_CLEAN = input$rec_bmi,
                           CAN_DIAB_TY_ALT_F = input$diab,
                           CAN_LAST_ASCITES_ALT_F = input$asc,
                           REC_PORTAL_VEIN_ALT_F = input$pvthromb,
                           REC_TIPSS_ALT_F = input$tipss, 
                           DON_AGE = input$don_age3,
                           DON_NON_HR_BEAT_F  = input$don_type3,
                           REC_COLD_ISCH_TM = input$isch,
                           REC_TX_PROCEDURE_TY_F = input$wholespl,
                           TFL_FAIL_INFECT_Y_F = input$infect,
                           TFL_FAIL_PX_NONCOMP_Y_F = input$noncomp,
                           LTX_NUMBER = input$ltcount,
                           prev_graft_surv_category = input$survtime,
                           REC_EGFR_DIAL = input$egfrdial,
                           REC_FUNCTN_STAT_F = input$karn)
      
      if (input$don_num == 1){
        
        model_prediction <- predict(m2, newdata = dfnew1)
        
        risk_results <- data.frame(time = model_prediction$time.interest/365.25,
                                   pred = 100*t(model_prediction$survival))
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results)){
          if (risk_results$time[i] <= 12 && risk_results$time[i+1] > 12){
            twelvemonth <- risk_results[i,2]
          }
        }
        risk_results[nrow(risk_results)+1, ] <- c(12, twelvemonth)
        
        #plot
        
        if (input$one_or_five_year == "1-Year Survival"){
          
          predtxt <- paste(round(100*t(model_prediction$survival)),"%")
          vector <- seq(1,151,1)
          incl <- c(8,15,21,25,28,32,35,38,40,42)
          predtxt[!vector %in% incl] <- NA 
          
          risk_results$predtext <- predtxt

        g<-ggplot(data = risk_results, aes(x = time, y = pred)) +
          geom_step(linewidth = 1, col= "#619CFF", na.rm = T) + 
          labs(x = 'Time Post-Transplant (Years)',
               y = 'Predicted Survival (%)') +
          theme_bw() +
          theme(text = element_text(size = 15)) +
          scale_x_continuous(breaks=seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
          scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
          guides(y.sec = "axis") + 
          geom_text(aes(label = predtext), nudge_y = 2,nudge_x = 0)
        
        g
        
        } else {
          
          predtxt <- paste(round(100*t(model_prediction$survival)),"%")
          vector <- seq(1,151,1)
          incl <- c(29,43,56,64,73,81,88,95,101,105)
          predtxt[!vector %in% incl] <- NA 
          
          risk_results$predtext <- predtxt
          
          g<-ggplot(data = risk_results, aes(x = time, y = pred)) +
            geom_step(linewidth = 1, col= "#619CFF", na.rm = T) + 
            labs(x = 'Time Post-Transplant (Years)',
                 y = 'Predicted Survival (%)') +
            theme_bw() +
            theme(text = element_text(size = 15)) +
            scale_x_continuous(breaks=seq(0,5,1), limits = c(0,5), expand = c(0,0)) +
            scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
            guides(y.sec = "axis") + 
            geom_text(aes(label = predtext), nudge_y = 2,nudge_x = 0)
          
          g
          
        }
        
      } else if (input$don_num == 2){
        
        model_prediction <- predict(m2, newdata = dfnew1)
        model_prediction2 <- predict(m2, newdata = dfnew2)
        
        risk_results1 <- data.frame(time = model_prediction$time.interest/365.25,
                                    pred = 100*t(model_prediction$survival),
                                    group = 1)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results1)){
          if (risk_results1$time[i] <= 12 && risk_results1$time[i+1] > 12){
            twelvemonth <- risk_results1[i,2]
          }
        }
        risk_results1[nrow(risk_results1)+1, ] <- c(12, twelvemonth, 1)
        
        risk_results2 <- data.frame(time = model_prediction2$time.interest/365.25,
                                    pred = 100*t(model_prediction2$survival),
                                    group = 2)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results2)){
          if (risk_results2$time[i] <= 12 && risk_results2$time[i+1] > 12){
            twelvemonth <- risk_results2[i,2]
          }
        }
        risk_results2[nrow(risk_results2)+1, ] <- c(12, twelvemonth, 2)
        
        risk_results <- rbind(risk_results1, risk_results2)
        
        
        #plot
        
        if (input$one_or_five_year == "1-Year Survival"){
        
        g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
          geom_step(linewidth=1, na.rm=T) + 
          labs(x = 'Time Post-Transplant (Years)',
               y = 'Predicted Survival (%)') +
          theme_bw() +
          theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
          scale_x_continuous(breaks=seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
          scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
          scale_color_discrete("Donor") + guides(y.sec = "axis")
        
        g
        
        } else {
          
          g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
            geom_step(linewidth=1, na.rm=T) + 
            labs(x = 'Time Post-Transplant (Years)',
                 y = 'Predicted Survival (%)') +
            theme_bw() +
            theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
            scale_x_continuous(breaks=seq(0,5,1), limits = c(0,5), expand = c(0,0)) +
            scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
            scale_color_discrete("Donor") + guides(y.sec = "axis")
          
          g
          
        }
        
      } else if (input$don_num == 3){
        
        model_prediction <- predict(m2, newdata = dfnew1)
        model_prediction2 <- predict(m2, newdata = dfnew2)
        model_prediction3 <- predict(m2, newdata = dfnew3)
        
        risk_results1 <- data.frame(time = model_prediction$time.interest/365.25,
                                    pred = 100*t(model_prediction$survival),
                                    group = 1)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results1)){
          if (risk_results1$time[i] <= 12 && risk_results1$time[i+1] > 12){
            twelvemonth <- risk_results1[i,2]
          }
        }
        risk_results1[nrow(risk_results1)+1, ] <- c(12, twelvemonth, 1) 
        
        risk_results2 <- data.frame(time = model_prediction2$time.interest/365.25,
                                    pred = 100*t(model_prediction2$survival),
                                    group = 2)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results2)){
          if (risk_results2$time[i] <= 12 && risk_results2$time[i+1] > 12){
            twelvemonth <- risk_results2[i,2]
          }
        }
        risk_results2[nrow(risk_results2)+1, ] <- c(12, twelvemonth, 2) 
        
        risk_results3 <- data.frame(time = model_prediction3$time.interest/365.25,
                                    pred = 100*t(model_prediction3$survival),
                                    group = 3)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results3)){
          if (risk_results3$time[i] <= 12 && risk_results3$time[i+1] > 12){
            twelvemonth <- risk_results3[i,2]
          }
        }
        risk_results3[nrow(risk_results3)+1, ] <- c(12, twelvemonth, 3) 
        
        risk_results <- rbind(risk_results1, risk_results2, risk_results3)
        
        
        #plot
        
        if (input$one_or_five_year == "1-Year Survival"){        
        
        g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
          geom_step(linewidth=1, na.rm=T) + 
          labs(x = 'Time Post-Transplant (Years)',
               y = 'Predicted Survival (%)') +
          theme_bw() +
          theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
          scale_x_continuous(breaks=seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
          scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
          scale_color_discrete("Donor") + guides(y.sec = "axis") 

        g
        
        } else {
          
          g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
            geom_step(linewidth=1, na.rm=T) + 
            labs(x = 'Time Post-Transplant (Years)',
                 y = 'Predicted Survival (%)') +
            theme_bw() +
            theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
            scale_x_continuous(breaks=seq(0,5,1), limits = c(0,5), expand = c(0,0)) +
            scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
            scale_color_discrete("Donor") + guides(y.sec = "axis") 
          
          g
          
        }
        
      }
      
    } else if(input$vars_used == "All variables"){
      
      dfnew1 <- data.frame(REC_AGE_IN_YEARS_AT_TX = input$rec_agea,
                           CAN_GENDER_F  = input$rec_gendera,
                           CAN_ABO_ALT_F = input$bltypea,
                           REC_PRIVATE_INS_F = input$rec_insa,
                           CAN_EDUCATION_COLLEGE_F = input$rec_edua,
                           REC_DGN_CAT_F = input$disgrpa,
                           REC_LS_MC = input$lsmca,
                           MELD_cat_F = input$melda,
                           REC_HGT_CM = input$rec_hgta,
                           REC_BMI_CLEAN = input$rec_bmia,
                           CAN_DIAB_TY_ALT_F = input$diaba,
                           CAN_LAST_ASCITES_ALT_F = input$asca,
                           REC_PORTAL_VEIN_ALT_F = input$pvthromba,
                           REC_TIPSS_ALT_F = input$tipssa,
                           CAN_MALIG_ALT_F = input$prevmaliga,
                           DON_AGE = input$don_age1a,
                           DON_GENDER_F = input$don_gender1a,
                           DON_NON_HR_BEAT_F  = input$don_type1a,
                           DON_CAD_DON_COD_F = input$don_cod1a,
                           DON_INSULIN_ALT_F = input$don_diab1a,
                           REC_COLD_ISCH_TM = input$ischa,
                           REC_TX_PROCEDURE_TY_F = input$wholespla,
                           DON_HGT_CM = input$donhgt1a,
                           REC_TX_YEAR = input$txyeara,
                           TFL_FAIL_BILIARY_Y_F = input$biliarya,
                           TFL_FAIL_HEP_DENOVO_Y_F = input$denovoa,
                           TFL_FAIL_HEP_RECUR_Y_F = input$hrecura,
                           TFL_FAIL_INFECT_Y_F = input$infecta,
                           pnf_or_14dayfail = input$primea,
                           TFL_FAIL_PX_NONCOMP_Y_F = input$noncompa,
                           TFL_FAIL_RECUR_DISEASE_Y_F = input$drecura,
                           TFL_FAIL_REJ_ACUTE_Y_F = input$acutea,
                           TFL_FAIL_REJ_CHRONIC_Y_F = input$chronica,
                           TFL_FAIL_VASC_THROMB_Y_F = input$thromba,
                           LTX_NUMBER = input$ltcounta,
                           REC_TX_ORG_TY = input$orgtxa,
                           prev_graft_surv_category = input$survtimea,
                           REC_EGFR_DIAL = input$egfrdiala,
                           REC_FUNCTN_STAT_F = input$karna)
      
      dfnew2 <- data.frame(REC_AGE_IN_YEARS_AT_TX = input$rec_agea,
                           CAN_GENDER_F  = input$rec_gendera,
                           CAN_ABO_ALT_F = input$bltypea,
                           REC_PRIVATE_INS_F = input$rec_insa,
                           CAN_EDUCATION_COLLEGE_F = input$rec_edua,
                           REC_DGN_CAT_F = input$disgrpa,
                           REC_LS_MC = input$lsmca,
                           MELD_cat_F = input$melda,
                           REC_HGT_CM = input$rec_hgta,
                           REC_BMI_CLEAN = input$rec_bmia,
                           CAN_DIAB_TY_ALT_F = input$diaba,
                           CAN_LAST_ASCITES_ALT_F = input$asca,
                           REC_PORTAL_VEIN_ALT_F = input$pvthromba,
                           REC_TIPSS_ALT_F = input$tipssa,
                           CAN_MALIG_ALT_F = input$prevmaliga,
                           DON_AGE = input$don_age2a,
                           DON_GENDER_F = input$don_gender2a,
                           DON_NON_HR_BEAT_F  = input$don_type2a,
                           DON_CAD_DON_COD_F = input$don_cod2a,
                           DON_INSULIN_ALT_F = input$don_diab2a,
                           REC_COLD_ISCH_TM = input$ischa,
                           REC_TX_PROCEDURE_TY_F = input$wholespla,
                           DON_HGT_CM = input$donhgt2a,
                           REC_TX_YEAR = input$txyeara,
                           TFL_FAIL_BILIARY_Y_F = input$biliarya,
                           TFL_FAIL_HEP_DENOVO_Y_F = input$denovoa,
                           TFL_FAIL_HEP_RECUR_Y_F = input$hrecura,
                           TFL_FAIL_INFECT_Y_F = input$infecta,
                           pnf_or_14dayfail = input$primea,
                           TFL_FAIL_PX_NONCOMP_Y_F = input$noncompa,
                           TFL_FAIL_RECUR_DISEASE_Y_F = input$drecura,
                           TFL_FAIL_REJ_ACUTE_Y_F = input$acutea,
                           TFL_FAIL_REJ_CHRONIC_Y_F = input$chronica,
                           TFL_FAIL_VASC_THROMB_Y_F = input$thromba,
                           LTX_NUMBER = input$ltcounta,
                           REC_TX_ORG_TY = input$orgtxa,
                           prev_graft_surv_category = input$survtimea,
                           REC_EGFR_DIAL = input$egfrdiala,
                           REC_FUNCTN_STAT_F = input$karna)
      
      dfnew3 <- data.frame(REC_AGE_IN_YEARS_AT_TX = input$rec_agea,
                           CAN_GENDER_F  = input$rec_gendera,
                           CAN_ABO_ALT_F = input$bltypea,
                           REC_PRIVATE_INS_F = input$rec_insa,
                           CAN_EDUCATION_COLLEGE_F = input$rec_edua,
                           REC_DGN_CAT_F = input$disgrpa,
                           REC_LS_MC = input$lsmca,
                           MELD_cat_F = input$melda,
                           REC_HGT_CM = input$rec_hgta,
                           REC_BMI_CLEAN = input$rec_bmia,
                           CAN_DIAB_TY_ALT_F = input$diaba,
                           CAN_LAST_ASCITES_ALT_F = input$asca,
                           REC_PORTAL_VEIN_ALT_F = input$pvthromba,
                           REC_TIPSS_ALT_F = input$tipssa,
                           CAN_MALIG_ALT_F = input$prevmaliga,
                           DON_AGE = input$don_age3a,
                           DON_GENDER_F = input$don_gender3a,
                           DON_NON_HR_BEAT_F  = input$don_type3a,
                           DON_CAD_DON_COD_F = input$don_cod3a,
                           DON_INSULIN_ALT_F = input$don_diab3a,
                           REC_COLD_ISCH_TM = input$ischa,
                           REC_TX_PROCEDURE_TY_F = input$wholespla,
                           DON_HGT_CM = input$donhgt3a,
                           REC_TX_YEAR = input$txyeara,
                           TFL_FAIL_BILIARY_Y_F = input$biliarya,
                           TFL_FAIL_HEP_DENOVO_Y_F = input$denovoa,
                           TFL_FAIL_HEP_RECUR_Y_F = input$hrecura,
                           TFL_FAIL_INFECT_Y_F = input$infecta,
                           pnf_or_14dayfail = input$primea,
                           TFL_FAIL_PX_NONCOMP_Y_F = input$noncompa,
                           TFL_FAIL_RECUR_DISEASE_Y_F = input$drecura,
                           TFL_FAIL_REJ_ACUTE_Y_F = input$acutea,
                           TFL_FAIL_REJ_CHRONIC_Y_F = input$chronica,
                           TFL_FAIL_VASC_THROMB_Y_F = input$thromba,
                           LTX_NUMBER = input$ltcounta,
                           REC_TX_ORG_TY = input$orgtxa,
                           prev_graft_surv_category = input$survtimea,
                           REC_EGFR_DIAL = input$egfrdiala,
                           REC_FUNCTN_STAT_F = input$karna)
      
      dfnew1$HEIGHT_RATIO_DONTOREC <- dfnew1$DON_HGT_CM / dfnew1$REC_HGT_CM
      dfnew2$HEIGHT_RATIO_DONTOREC <- dfnew2$DON_HGT_CM / dfnew2$REC_HGT_CM
      dfnew3$HEIGHT_RATIO_DONTOREC <- dfnew3$DON_HGT_CM / dfnew3$REC_HGT_CM
      
      if (input$don_num2 == 1){
        
        model_prediction <- predict(m1, newdata = dfnew1)
        
        risk_results <- data.frame(time = model_prediction$time.interest/365.25,
                                   pred = 100*t(model_prediction$survival))
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results)){
          if (risk_results$time[i] <= 12 && risk_results$time[i+1] > 12){
            twelvemonth <- risk_results[i,2]
          }
        }
        risk_results[nrow(risk_results)+1, ] <- c(12, twelvemonth)
        
        #plot
        
        if (input$one_or_five_year == "1-Year Survival"){ 
          
          predtxt <- paste(round(100*t(model_prediction$survival)),"%")
          vector <- seq(1,151,1)
          incl <- c(8,15,21,25,28,32,35,38,40,42)
          predtxt[!vector %in% incl] <- NA 
          
          risk_results$predtext <- predtxt

        g<-ggplot(data = risk_results, aes(x = time, y = pred)) +
          geom_step(linewidth = 1, col= "#619CFF", na.rm = T) + 
          labs(x = 'Time Post-Transplant (Years)',
               y = 'Predicted Survival (%)') +
          theme_bw() +
          theme(text = element_text(size = 15)) +
          scale_x_continuous(breaks=seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
          scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
          guides(y.sec = "axis") +
          geom_text(aes(label = predtext), nudge_y = 2,nudge_x = 0)
        
        g
        
        } else {
          
          predtxt <- paste(round(100*t(model_prediction$survival)),"%")
          vector <- seq(1,151,1)
          incl <- c(29,43,56,64,73,81,88,95,101,105)
          predtxt[!vector %in% incl] <- NA 
          
          risk_results$predtext <- predtxt
          
          g<-ggplot(data = risk_results, aes(x = time, y = pred)) +
            geom_step(linewidth = 1, col= "#619CFF", na.rm = T) +
            labs(x = 'Time Post-Transplant (Years)',
                 y = 'Predicted Survival (%)') +
            theme_bw() +
            theme(text = element_text(size = 15)) +
            scale_x_continuous(breaks=seq(0,5,1), limits = c(0,5), expand = c(0,0)) +
            scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
            guides(y.sec = "axis") +
            geom_text(aes(label = predtext), nudge_y = 2,nudge_x = 0)
          
          g
          
        }
        
      } else if (input$don_num2 == 2){
        
        model_prediction <- predict(m1, newdata = dfnew1)
        model_prediction2 <- predict(m1, newdata = dfnew2)
        
        risk_results1 <- data.frame(time = model_prediction$time.interest/365.25,
                                    pred = 100*t(model_prediction$survival),
                                    group = 1)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results1)){
          if (risk_results1$time[i] <= 12 && risk_results1$time[i+1] > 12){
            twelvemonth <- risk_results1[i,2]
          }
        }
        risk_results1[nrow(risk_results1)+1, ] <- c(12, twelvemonth, 1) 
        
        risk_results2 <- data.frame(time = model_prediction2$time.interest/365.25,
                                    pred = 100*t(model_prediction2$survival),
                                    group = 2)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results2)){
          if (risk_results2$time[i] <= 12 && risk_results2$time[i+1] > 12){
            twelvemonth <- risk_results2[i,2]
          }
        }
        risk_results2[nrow(risk_results2)+1, ] <- c(12, twelvemonth, 2) 
        
        risk_results <- rbind(risk_results1, risk_results2)
        
        
        #plot
        
        if (input$one_or_five_year == "1-Year Survival"){ 
        
        g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
          geom_step(linewidth=1, na.rm=T) + 
          labs(x = 'Time Post-Transplant (Years)',
               y = 'Predicted Survival (%)') +
          theme_bw() +
          theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
          scale_x_continuous(breaks=seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
          scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
          scale_color_discrete("Donor") + guides(y.sec = "axis")

        g
        
        } else {
          
          g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
            geom_step(linewidth=1, na.rm=T) + 
            labs(x = 'Time Post-Transplant (Years)',
                 y = 'Predicted Survival (%)') +
            theme_bw() +
            theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
            scale_x_continuous(breaks=seq(0,5,1), limits = c(0,5), expand = c(0,0)) +
            scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
            scale_color_discrete("Donor") + guides(y.sec = "axis")
          
          g
          
        }
        
      } else if (input$don_num2 == 3){
        
        model_prediction <- predict(m1, newdata = dfnew1)
        model_prediction2 <- predict(m1, newdata = dfnew2)
        model_prediction3 <- predict(m1, newdata = dfnew3)
        
        risk_results1 <- data.frame(time = model_prediction$time.interest/365.25,
                                    pred = 100*t(model_prediction$survival),
                                    group = 1)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results1)){
          if (risk_results1$time[i] <= 12 && risk_results1$time[i+1] > 12){
            twelvemonth <- risk_results1[i,2]
          }
        }
        risk_results1[nrow(risk_results1)+1, ] <- c(12, twelvemonth, 1) 
        
        risk_results2 <- data.frame(time = model_prediction2$time.interest/365.25,
                                    pred = 100*t(model_prediction2$survival),
                                    group = 2)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results2)){
          if (risk_results2$time[i] <= 12 && risk_results2$time[i+1] > 12){
            twelvemonth <- risk_results2[i,2]
          }
        }
        risk_results2[nrow(risk_results2)+1, ] <- c(12, twelvemonth, 2) 
        
        risk_results3 <- data.frame(time = model_prediction3$time.interest/365.25,
                                    pred = 100*t(model_prediction3$survival),
                                    group = 3)
        
        # Code to extend the last survival calculated before 12 months out to the 12 month point, for plotting
        twelvemonth <- c()
        for (i in 1:nrow(risk_results3)){
          if (risk_results3$time[i] <= 12 && risk_results3$time[i+1] > 12){
            twelvemonth <- risk_results3[i,2]
          }
        }
        risk_results3[nrow(risk_results3)+1, ] <- c(12, twelvemonth, 3) 
        
        risk_results <- rbind(risk_results1, risk_results2, risk_results3)
        
        
        #plot
        
        if (input$one_or_five_year == "1-Year Survival"){ 
        
        g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
          geom_step(linewidth=1, na.rm=T) + 
          labs(x = 'Time Post-Transplant (Years)',
               y = 'Predicted Survival (%)') +
          theme_bw() +
          theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
          scale_x_continuous(breaks=seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
          scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
          scale_color_discrete("Donor") + guides(y.sec = "axis")
        
        g
        
        } else {
          
          g<-ggplot(data = risk_results, aes(x = time, y = pred, color = factor(group))) +
            geom_step(linewidth=1, na.rm=T) + 
            labs(x = 'Time Post-Transplant (Years)',
                 y = 'Predicted Survival (%)') +
            theme_bw() +
            theme(text = element_text(size = 15), legend.position = c(0.5, 0.8)) +
            scale_x_continuous(breaks=seq(0,5,1), limits = c(0,5), expand = c(0,0)) +
            scale_y_continuous(limits = c(40,100), expand = c(0,0)) +
            scale_color_discrete("Donor") + guides(y.sec = "axis")
          
          g
          
        }
        
      }
      
      
    }
    
    
  }, res = 96) # End of RenderPlot
  
}

############################
# End of server.R file
############################

shinyApp(ui, server)