# SMOG Analytics Dashboard - Integrated Team Project
# Horizontal top navigation. Requires cleaned_smog.csv created by 01_data_cleaning.R.

library(shiny)
library(ggplot2)
library(DT)
source("02_descriptive_statistics.R")
source("03_visualizations.R")
source("04_inferential_statistics.R")
source("05_correlation_regression.R")

if (!file.exists("cleaned_smog.csv")) stop("cleaned_smog.csv is missing. Run source('01_data_cleaning.R') first.")
smog_data <- read.csv("cleaned_smog.csv", stringsAsFactors=FALSE, check.names=FALSE, na.strings=c("", "NA", "N/A"))
smog_data <- prepare_smog_data(smog_data)
numeric_vars <- names(smog_data)[vapply(smog_data,is.numeric,logical(1))]
if (!length(numeric_vars)) stop("No numeric columns found in cleaned_smog.csv.")
cat_vars <- names(smog_data)[vapply(smog_data,function(x)is.character(x)||is.factor(x)||is.logical(x),logical(1))]
pm25_name <- if("pm2_5_ug_m3" %in% numeric_vars) "pm2_5_ug_m3" else numeric_vars[1]
other_numeric <- setdiff(numeric_vars,pm25_name)
time_default <- if("timestamp" %in% names(smog_data)) "timestamp" else names(smog_data)[1]

ui <- navbarPage(
  title="SMOG Analytics Dashboard", id="page", collapsible=TRUE,
  header=tags$head(tags$style(HTML("
    body { background:#f5f7fb; }
    .navbar { margin-bottom:20px; border-radius:0; }
    .navbar-default { background:#18324d; border-color:#18324d; }
    .navbar-default .navbar-brand,.navbar-default .navbar-nav>li>a { color:white; }
    .navbar-default .navbar-brand:hover,.navbar-default .navbar-nav>li>a:hover { color:#d9e9f5; background:#244765; }
    .navbar-default .navbar-nav>.active>a,.navbar-default .navbar-nav>.active>a:hover { color:white; background:#2486B9; }
    h2,h3 { color:#18324d; } .tab-content { padding-top:8px; }
  "))),
  tabPanel("Overview", value="Overview", h3("Dataset Overview"), verbatimTextOutput("overview"), plotOutput("overview_plot",height=350)),
  tabPanel("Dataset", value="Dataset", h3("Cleaned Dataset"), p("Browse and filter the cleaned records."), DTOutput("dataset_table")),
  tabPanel("Descriptive Statistics", value="Descriptive Statistics", h3("Descriptive Statistics"),
    selectInput("desc_var","Numeric variable",choices=numeric_vars,selected=pm25_name), DTOutput("desc_table")),
  tabPanel("Visualizations", value="Visualizations", h3("Visualizations"),
    fluidRow(column(4,selectInput("plot_type","Chart type",choices=c("Histogram","Box plot","Density plot","Bar chart","Pie chart","Line graph","Scatter plot","Regression plot","City comparison","Monthly comparison","Hazardous-event comparison"))),
      column(4,selectInput("plot_var","Variable",choices=names(smog_data),selected=pm25_name)),
      column(4,selectInput("plot_y","Second numeric variable (scatter/regression)",choices=numeric_vars,selected=if(length(numeric_vars)>1)numeric_vars[2] else numeric_vars[1]))),
    fluidRow(column(4,selectInput("time_var","Time/order column",choices=names(smog_data),selected=time_default))),
    plotOutput("chosen_plot",height=430)),
  tabPanel("Exploratory Analysis", value="Exploratory Analysis", h3("Exploratory Data Analysis"),
    h4("Numeric summary"), DTOutput("eda_numeric"), h4("Missing values by column"), DTOutput("eda_missing"),
    h4("Potential outliers (IQR rule)"), DTOutput("eda_outliers")),
  tabPanel("Correlation", value="Correlation", h3("Correlation Analysis"),
    fluidRow(column(4,selectInput("corr_x","Variable X",choices=numeric_vars,selected=numeric_vars[1])),
      column(4,selectInput("corr_y","Variable Y",choices=numeric_vars,selected=if(length(numeric_vars)>1)numeric_vars[2] else numeric_vars[1])),
      column(4,br(),actionButton("do_corr","Calculate correlation",class="btn-primary"))),
    verbatimTextOutput("corr_results"), plotOutput("corr_plot",height=350)),
  tabPanel("Regression", value="Regression", h3("Regression Analysis"),
    fluidRow(column(4,selectInput("response","Response variable (Y)",choices=numeric_vars,selected=pm25_name)),
      column(5,selectizeInput("predictors","Predictor variable(s) (X)",choices=other_numeric,selected=if(length(other_numeric))other_numeric[1] else character(0),multiple=TRUE)),
      column(3,br(),actionButton("do_regression","Run regression",class="btn-primary"))),
    verbatimTextOutput("reg_results"), plotOutput("reg_plot",height=350)),
  tabPanel("Inferential Statistics", value="Inferential Statistics", h3("Inferential Statistics"),
    p("Choose a numeric variable to estimate its mean and run a one-sample t-test against a chosen reference value."),
    fluidRow(column(4,selectInput("inf_var","Numeric variable",choices=numeric_vars,selected=pm25_name)),
      column(4,numericInput("null_mean","Hypothesized mean",value=0)),
      column(4,selectInput("confidence","Confidence level",choices=c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95))),
    actionButton("run_inference","Run analysis",class="btn-primary"), verbatimTextOutput("inference_results"),
    p("The included reusable functions also support two-sample and paired t-tests, confidence intervals, chi-square tests, F-tests, Z-tests, and one-way ANOVA. Select the test that matches your study design and assumptions.")),
  tabPanel("Auto Insights", value="Auto Insights", h3("Automatic Dataset Insights"),
    p("A concise, data-driven interpretation. Domain-specific good/bad labels are only made when suitable thresholds are known."),
    verbatimTextOutput("auto_conclusion")),
  footer=tags$div(style="padding:12px 15px;color:#64748b;font-size:12px;","SMOG Analytics Dashboard | Integrated team project | Data source: cleaned_smog.csv")
)

server <- function(input,output,session) {
  output$overview <- renderPrint({
    cat("Observations:",nrow(smog_data),"\nVariables:",ncol(smog_data),"\nMissing cells:",sum(is.na(smog_data)),"\nDuplicate rows:",sum(duplicated(smog_data)),"\n")
    if("timestamp" %in% names(smog_data)) cat("Timestamp range:",as.character(min(smog_data$timestamp,na.rm=TRUE)),"to",as.character(max(smog_data$timestamp,na.rm=TRUE)),"\n")
    if(pm25_name %in% names(smog_data)) {x<-smog_data[[pm25_name]];x<-x[is.finite(x)];if(length(x))cat("Selected numeric variable:",pm25_name,"\nMean:",round(mean(x),3),"\nMedian:",round(median(x),3),"\nMaximum:",max(x),"\n")}
  })
  output$overview_plot <- renderPlot({
    x<-smog_data[[pm25_name]];validate(need(any(is.finite(x)),"No valid values to plot."))
    ggplot(data.frame(value=x),aes(value))+geom_histogram(bins=30,fill="#2486B9",color="white",na.rm=TRUE)+labs(title=paste("Distribution of",pm25_name),x=pm25_name,y="Frequency")+theme_minimal()
  })
  output$dataset_table <- renderDT({datatable(smog_data,filter="top",options=list(pageLength=10,scrollX=TRUE))})
  output$desc_table <- renderDT({req(input$desc_var); datatable(descriptive_summary(smog_data[[input$desc_var]]),rownames=FALSE,options=list(dom="t",pageLength=25)) |> formatRound("Value",digits=4)})

  output$chosen_plot <- renderPlot({
    req(input$plot_type,input$plot_var)
    tryCatch({
      t<-input$plot_type; v<-input$plot_var
      if(t=="Histogram") plot_histogram(smog_data,v)
      else if(t=="Box plot") {if(is.numeric(smog_data[[v]])) plot_boxplot(smog_data,v) else plot_bar(smog_data,v)}
      else if(t=="Density plot") plot_density(smog_data,v)
      else if(t=="Bar chart") plot_bar(smog_data,v)
      else if(t=="Pie chart") plot_pie(smog_data,v)
      else if(t=="Line graph") {vv<-if(is.numeric(smog_data[[v]]))v else pm25_name; plot_line(smog_data,vv,time_unit="day")}
      else if(t=="Scatter plot") plot_scatter(smog_data,input$plot_y,v)
      else if(t=="Regression plot") plot_regression(smog_data,v,input$plot_y)
      else if(t=="City comparison") plot_city_comparison(smog_data,if(is.numeric(smog_data[[v]]))v else pm25_name)
      else if(t=="Monthly comparison") plot_month_comparison(smog_data,if(is.numeric(smog_data[[v]]))v else pm25_name)
      else plot_hazardous_comparison(smog_data,if(is.numeric(smog_data[[v]]))v else pm25_name)
    },error=function(e){plot.new();text(.5,.5,paste("Cannot create this chart:\n",conditionMessage(e)))})
  })
  output$eda_numeric <- renderDT({datatable(eda_numeric_summary(smog_data),options=list(pageLength=10,scrollX=TRUE)) |> formatRound(columns=intersect(c("mean","median","sd","min","Q1","Q3","max"),names(eda_numeric_summary(smog_data))),digits=3)})
  output$eda_missing <- renderDT({datatable(eda_missing_summary(smog_data),options=list(dom="t",pageLength=30)) |> formatRound("percentage",digits=3)})
  output$eda_outliers <- renderDT({datatable(eda_outlier_summary(smog_data),options=list(pageLength=15,scrollX=TRUE)) |> formatRound("percentage",digits=3)})

  corr_result <- eventReactive(input$do_corr, {
    req(input$corr_x,input$corr_y);validate(need(input$corr_x!=input$corr_y,"Choose two different variables."))
    p<-clean_pair(smog_data[[input$corr_x]],smog_data[[input$corr_y]])
    validate(need(length(p$x)>=3,"At least three complete pairs are required."),need(length(unique(p$x))>1&&length(unique(p$y))>1,"Both variables must vary."))
    list(pair=p,covariance=covariance_analysis(smog_data[[input$corr_x]],smog_data[[input$corr_y]]),pearson=pearson_correlation(smog_data[[input$corr_x]],smog_data[[input$corr_y]]),spearman=spearman_correlation(smog_data[[input$corr_x]],smog_data[[input$corr_y]]))
  })
  output$corr_results <- renderPrint({r<-corr_result();cat("X:",input$corr_x,"\nY:",input$corr_y,"\n\nCovariance:",round(r$covariance,4),"\nPearson r:",round(r$pearson$estimate,4),"\nSpearman rho:",round(r$spearman$estimate,4),"\nPearson p-value:",format.pval(r$pearson$p_value),"\nSpearman p-value:",format.pval(r$spearman$p_value),"\nCorrelation does not prove causation.\n")})
  output$corr_plot <- renderPlot({r<-corr_result();ggplot(data.frame(X=r$pair$x,Y=r$pair$y),aes(X,Y))+geom_point(alpha=.35)+geom_smooth(method="lm",formula=y~x,se=TRUE)+labs(x=input$corr_x,y=input$corr_y,title="Relationship between selected variables")+theme_minimal()})
  reg_model <- eventReactive(input$do_regression,{req(input$response,input$predictors);validate(need(length(input$predictors)>0,"Choose at least one predictor."),need(!(input$response%in%input$predictors),"Response cannot also be a predictor."));multiple_regression(smog_data,input$response,input$predictors)})
  output$reg_results <- renderPrint({m<-reg_model();print(summary(m));cat("\nFormula:",paste(deparse(formula(m)),collapse=""),"\n")})
  output$reg_plot <- renderPlot({m<-reg_model();validate(need(length(input$predictors)==1,"Select exactly one predictor to display the regression plot."));plot_regression(smog_data,input$predictors[1],input$response)})
  output$inference_results <- renderPrint({req(input$run_inference);x<-smog_data[[input$inf_var]];x<-x[is.finite(x)];validate(need(length(x)>=2,"At least two valid observations are required."));conf<-as.numeric(input$confidence);cat("Variable:",input$inf_var,"\nSample size:",length(x),"\nMean:",mean(x),"\nConfidence interval:\n");print(mean_confidence_interval(x,confidence=conf));cat("\nOne-sample t-test against mean",input$null_mean,"\n");print(t.test(x,mu=input$null_mean,conf.level=conf))})

  output$auto_conclusion <- renderPrint({
    d<-smog_data; cat("AUTOMATIC DATASET CONCLUSION\n============================\n")
    cat("The dataset contains",nrow(d),"rows and",ncol(d),"columns. Missing cells:",sum(is.na(d)),"; exact duplicate rows:",sum(duplicated(d)),".\n\n")
    nums<-names(d)[vapply(d,is.numeric,logical(1))]
    if(length(nums)) {
      stats<-lapply(nums,function(nm){x<-d[[nm]];x<-x[is.finite(x)];if(!length(x))return(NULL);data.frame(variable=nm,n=length(x),mean=mean(x),median=median(x),min=min(x),max=max(x),sd=if(length(x)>1)sd(x) else NA_real_)})
      stats<-Filter(Negate(is.null),stats)
      if(length(stats)) {
        s<-do.call(rbind,stats); print(transform(s,mean=round(mean,3),median=round(median,3),min=round(min,3),max=round(max,3),sd=round(sd,3)),row.names=FALSE)
        delta<-abs(s$mean-s$median);if(any(is.finite(delta))) {i<-which.max(ifelse(is.finite(delta),delta,-Inf));cat("\nLargest mean-versus-median gap:",s$variable[i],". This may indicate skewness or extreme values.\n")}
        out<-eda_outlier_summary(d,nums);out<-out[order(out$outliers,decreasing=TRUE),];if(nrow(out)&&any(out$outliers>0)){cat("Variables with the most IQR-flagged potential outliers:\n");print(head(out[out$outliers>0,],5),row.names=FALSE)} else cat("No IQR-based outliers detected in numeric columns with sufficient observations.\n")
        if(length(nums)>=2) {cm<-suppressWarnings(cor(d[nums],use="pairwise.complete.obs"));diag(cm)<-NA_real_;cm[!is.finite(cm)]<-NA_real_;if(any(is.finite(cm))){ij<-which(abs(cm)==max(abs(cm),na.rm=TRUE),arr.ind=TRUE)[1,];rv<-cm[ij[1],ij[2]];cat("Strongest absolute numeric correlation:",rownames(cm)[ij[1]],"and",colnames(cm)[ij[2]],"=",round(rv,3),".");if(rv>0)cat(" Positive association.\n") else cat(" Negative association.\n")}}
      }
    } else cat("No numeric columns were found, so numerical summaries and correlations are unavailable.\n")
    if(length(cat_vars)) {cat("\nMost frequent category for selected categorical columns:\n");for(nm in head(cat_vars,5)){x<-as.character(d[[nm]]);x<-x[!is.na(x)&nzchar(trimws(x))];if(length(x)){tb<-sort(table(x),decreasing=TRUE);cat(nm,":",names(tb)[1],"(",as.integer(tb[1]),"records)\n")}}}
    cat("\nINTERPRETATION AND NEXT STEPS\n")
    if(sum(is.na(d))>0) cat("• Investigate missing values before modeling; decide whether to impute, exclude, or retain them based on the analysis.\n") else cat("• No missing cells were detected in the loaded data.\n")
    cat("• Review IQR-flagged observations; an outlier may be valid and should not be removed automatically.\n")
    cat("• A correlation indicates association, not causation. Check plots, domain context, and model assumptions before making decisions.\n")
    if(any(c("pm2_5_ug_m3","pm10_ug_m3","european_aqi") %in% names(d))) cat("• This dataset includes air-quality fields. Assess air quality using the appropriate AQI method, averaging period, and official local thresholds; this generic summary does not classify every record as safe or unsafe.\n") else cat("• Whether values are good or bad depends on variable meaning, units, and suitable domain-specific targets or thresholds.\n")
  })
}
shinyApp(ui,server)
