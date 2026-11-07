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
    p("Only three visualizations are available: correlation heatmap, box plot, and time-series line plot."),
    fluidRow(
      column(4, selectInput("plot_type", "Visualization",
        choices=c("Correlation heatmap", "Box plot", "Time-series line plot"),
        selected="Correlation heatmap")),
      column(4, uiOutput("visual_variable_ui")),
      column(4, uiOutput("visual_group_ui"))
    ),
    plotOutput("chosen_plot", height=500)
  ),
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
  tabPanel("Air Quality Assessment", value="Air Quality Assessment",
    h3("Air Quality Assessment & Remedies"),
    p("Assessment uses the European AQI field in the loaded dataset. The three dashboard labels group the European AQI bands: Good (0-20), Moderate (>20-60), and Poor (>60)."),
    fluidRow(
      column(4, selectInput("aq_city", "City", choices=c("All cities"), selected="All cities")),
      column(4, dateRangeInput("aq_dates", "Date range", start=Sys.Date()-30, end=Sys.Date())),
      column(4, helpText("The selected-period mean is classified; the peak AQI is also reported so short pollution episodes are not hidden."))
    ),
    uiOutput("aq_assessment")),
  footer=tags$div(style="padding:12px 15px;color:#64748b;font-size:12px;","SMOG Analytics Dashboard | Integrated team project | Data source: cleaned_smog.csv")
)

server <- function(input,output,session) {
  observe({
    if ("city" %in% names(smog_data)) {
      cities <- sort(unique(as.character(smog_data$city[!is.na(smog_data$city)])))
      updateSelectInput(session, "aq_city", choices=c("All cities", cities), selected="All cities")
    }
    if ("timestamp" %in% names(smog_data)) {
      dates <- as.Date(smog_data$timestamp)
      dates <- dates[!is.na(dates)]
      if (length(dates)) updateDateRangeInput(session, "aq_dates", start=min(dates), end=max(dates), min=min(dates), max=max(dates))
    }
  })

  aq_filtered <- reactive({
    d <- smog_data
    if ("city" %in% names(d) && !is.null(input$aq_city) && input$aq_city != "All cities") {
      d <- d[as.character(d$city) == input$aq_city, , drop=FALSE]
    }
    if ("timestamp" %in% names(d) && !is.null(input$aq_dates) && length(input$aq_dates)==2 && all(!is.na(input$aq_dates))) {
      dd <- as.Date(d$timestamp)
      d <- d[!is.na(dd) & dd >= as.Date(input$aq_dates[1]) & dd <= as.Date(input$aq_dates[2]), , drop=FALSE]
    }
    d
  })

  output$aq_assessment <- renderUI({
    d <- aq_filtered()
    if (!nrow(d)) return(tags$div(class="alert alert-warning", "No records match the selected city and date range. Change the filters."))

    if (!("european_aqi" %in% names(d)) || !is.numeric(d$european_aqi)) {
      return(tags$div(class="alert alert-danger",
        tags$strong("Air-quality classification unavailable. "),
        "The dataset must contain a numeric european_aqi column. Check the source file and cleaning step."))
    }

    aqi <- d$european_aqi
    valid <- aqi[is.finite(aqi) & aqi >= 0]
    invalid_n <- sum(!is.finite(aqi) | aqi < 0)
    if (!length(valid)) return(tags$div(class="alert alert-warning",
      "No valid non-negative European AQI measurements are available for this selection."))

    classify_aqi <- function(x) {
      if (!is.finite(x) || x < 0) return("Unavailable")
      if (x <= 20) return("Good")
      if (x <= 60) return("Moderate")
      return("Poor")
    }
    mean_aqi <- mean(valid)
    peak_aqi <- max(valid)
    status <- classify_aqi(mean_aqi)
    peak_status <- classify_aqi(peak_aqi)
    badge_class <- if (status=="Good") "success" else if (status=="Moderate") "warning" else "danger"

    pollutant_lines <- list()
    pollutant_names <- c("pm2_5_ug_m3","pm10_ug_m3","nitrogen_dioxide_ug_m3",
                         "carbon_monoxide_ug_m3","ozone_ug_m3","dust_ug_m3")
    for (nm in pollutant_names) {
      if (nm %in% names(d) && is.numeric(d[[nm]])) {
        xx <- d[[nm]][is.finite(d[[nm]]) & d[[nm]] >= 0]
        if (length(xx)) pollutant_lines[[length(pollutant_lines)+1]] <-
          tags$li(paste0(nm, ": mean ", round(mean(xx),2), "; maximum ", round(max(xx),2),
                         " (", if (grepl("_ug_m3$", nm)) "µg/m³" else "", ")"))
      }
    }

    if (status == "Good") {
      summary_text <- "The selected-period average falls in the Good band of the grouped European AQI scale."
      causes_text <- "No elevated AQI signal is indicated by the selected-period average. Individual pollutant sources cannot be identified from AQI alone."
      remedy_title <- "Maintain good air quality"
      remedies <- c("Continue low-emission travel options such as walking, cycling, and public transport where practical.",
                    "Avoid open burning of waste and leaves.",
                    "Maintain vehicle servicing and support clean energy and green spaces.",
                    "Continue monitoring because conditions can change over time.")
      health_text <- "The selected-period average is in the Good band. Continue following local public-health guidance."
    } else if (status == "Moderate") {
      summary_text <- "The selected-period average falls in the Moderate band. Review the pollutant measurements and the peak AQI for periods of higher pollution."
      causes_text <- "Potential contributors can include road traffic, fuel combustion, industrial emissions, and resuspended dust. These are possibilities, not causes proven by this dataset."
      remedy_title <- "Reduce exposure and prevent further emissions"
      remedies <- c("Reduce unnecessary vehicle trips and avoid engine idling.",
                    "Avoid open waste burning; control dust at roads and construction sites.",
                    "Maintain industrial emission-control equipment and investigate local sources.",
                    "People who are unusually sensitive to air pollution should monitor symptoms and follow local advisories, especially during peak periods.")
      health_text <- "Some sensitive people may experience effects during elevated pollution episodes. Use local advisories and consider the reported peak AQI, not only the average."
    } else {
      summary_text <- "The selected-period average falls in the Poor band. Treat this as an air-quality concern and check the peak value and pollutant measurements."
      causes_text <- "Potential contributors include traffic and other combustion emissions, industrial sources, and dust. AQI values alone cannot establish which source caused the result."
      remedy_title <- "Priority actions to reduce exposure and emissions"
      remedies <- c("Follow current local air-quality and public-health advisories.",
                    "Reduce strenuous outdoor activity during high-pollution periods, particularly for children, older adults, and people with heart or lung conditions.",
                    "Keep doors and windows closed when outdoor pollution is high; use an appropriately sized indoor air purifier if available.",
                    "Avoid indoor and outdoor burning, reduce vehicle idling, and strengthen dust and industrial emission controls.",
                    "Seek medical advice if experiencing significant or persistent breathing difficulty.")
      health_text <- "Poor air quality can increase health risks, especially for sensitive groups. This dashboard is informational and does not replace official advisories or medical advice."
    }

    tags$div(
      class="panel panel-default",
      tags$div(class="panel-heading", tags$h3(class="panel-title", "Selected data assessment")),
      tags$div(class="panel-body",
        tags$div(class=paste("alert", paste0("alert-",badge_class)),
          tags$h2(paste("Overall assessment:", status)),
          tags$p(summary_text)),
        fluidRow(
          column(4, tags$div(class="well", tags$strong("Mean European AQI"),
                             tags$h3(round(mean_aqi,1)))),
          column(4, tags$div(class="well", tags$strong("Peak European AQI"),
                             tags$h3(round(peak_aqi,1)), tags$p(paste("Peak band:",peak_status)))),
          column(4, tags$div(class="well", tags$strong("Valid observations"),
                             tags$h3(length(valid)), tags$p(paste("Invalid/missing AQI:",invalid_n))))
        ),
        tags$h4("Pollutant measurements"),
        if (length(pollutant_lines)) tags$ul(pollutant_lines) else tags$p("No supported pollutant concentration columns were found for this selection."),
        tags$h4("Possible contributors"),
        tags$p(causes_text),
        tags$h4("Health context"),
        tags$p(health_text),
        tags$h4(remedy_title),
        tags$ul(lapply(remedies, tags$li)),
        if (invalid_n > 0) tags$div(class="alert alert-warning",
          paste(invalid_n, "AQI records were missing, non-finite, or negative and were excluded from the classification.")),
        tags$p(style="color:#64748b;font-size:12px;",
          "Method note: European AQI bands are grouped for this dashboard as Good (0-20), Moderate (>20-60), and Poor (>60). Classification is based on the mean of valid selected-period European AQI values; the peak is shown separately. This three-band grouping is a simplified presentation, not a substitute for the full European AQI categories or local regulatory standards.")
      )
    )
  })

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

  output$visual_variable_ui <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "Correlation heatmap") {
      helpText("The heatmap automatically uses all numeric variables.")
    } else {
      selectInput("plot_var", "Numeric variable", choices=numeric_vars,
                  selected=pm25_name)
    }
  })

  output$visual_group_ui <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "Box plot") {
      selectInput("box_group", "Group by (optional)",
                  choices=c("No grouping", cat_vars),
                  selected=if ("city" %in% cat_vars) "city" else "No grouping")
    } else if (input$plot_type == "Time-series line plot") {
      time_choices <- names(smog_data)[grepl("date|time|timestamp", names(smog_data), ignore.case=TRUE)]
      if (!length(time_choices)) time_choices <- time_default
      selectInput("time_var", "Timestamp column", choices=time_choices,
                  selected=if (time_default %in% time_choices) time_default else time_choices[1])
    } else {
      helpText("No additional option is needed.")
    }
  })

  output$chosen_plot <- renderPlot({
    req(input$plot_type)
    tryCatch({
      t <- input$plot_type

      if (t == "Correlation heatmap") {
        validate(need(length(numeric_vars) >= 2,
                      "At least two numeric variables are required for a correlation heatmap."))
        cm <- suppressWarnings(cor(smog_data[numeric_vars],
                                   use="pairwise.complete.obs", method="pearson"))
        validate(need(any(is.finite(cm)), "Not enough valid data to calculate correlations."))
        heat <- as.data.frame(as.table(cm), stringsAsFactors=FALSE)
        names(heat) <- c("Variable 1", "Variable 2", "Correlation")
        heat <- heat[is.finite(heat$Correlation), , drop=FALSE]
        ggplot(heat, aes(x=`Variable 1`, y=`Variable 2`, fill=Correlation)) +
          geom_tile(color="white") +
          geom_text(aes(label=sprintf("%.2f", Correlation)), size=3) +
          scale_fill_gradient2(low="#B64B4B", mid="white", high="#238B75",
                               midpoint=0, limits=c(-1,1), name="Pearson r") +
          coord_equal() +
          labs(title="Correlation Heatmap",
               subtitle="Pearson correlation across numeric variables; association does not prove causation",
               x=NULL, y=NULL) +
          theme_minimal(base_size=11) +
          theme(axis.text.x=element_text(angle=45, hjust=1),
                panel.grid=element_blank())

      } else if (t == "Box plot") {
        req(input$plot_var)
        v <- input$plot_var
        validate(need(v %in% numeric_vars, "Select a numeric variable."))
        plot_data <- smog_data
        plot_data$.value <- plot_data[[v]]
        group <- input$box_group
        validate(need(any(is.finite(plot_data$.value)), "No valid numeric observations are available."))
        if (!is.null(group) && group != "No grouping" && group %in% names(plot_data)) {
          plot_data$.group <- as.factor(plot_data[[group]])
          ggplot(plot_data, aes(x=.group, y=.value, fill=.group)) +
            geom_boxplot(na.rm=TRUE, outlier.alpha=.45) +
            labs(title=paste("Box Plot of", v, "by", group), x=group, y=v) +
            theme_minimal(base_size=12) +
            theme(legend.position="none", axis.text.x=element_text(angle=35, hjust=1))
        } else {
          ggplot(plot_data, aes(x="", y=.value)) +
            geom_boxplot(fill="#2486B9", na.rm=TRUE, outlier.alpha=.45) +
            labs(title=paste("Box Plot of", v), x=NULL, y=v) +
            theme_minimal(base_size=12)
        }

      } else if (t == "Time-series line plot") {
        req(input$plot_var, input$time_var)
        v <- input$plot_var
        tv <- input$time_var
        validate(need(v %in% numeric_vars, "Select a numeric variable."))
        time_values <- smog_data[[tv]]
        if (!inherits(time_values, c("POSIXct", "POSIXt", "Date"))) {
          time_values <- suppressWarnings(as.POSIXct(time_values, tz="UTC"))
        }
        plot_data <- data.frame(time=time_values, value=smog_data[[v]])
        plot_data <- plot_data[!is.na(plot_data$time) & is.finite(plot_data$value), , drop=FALSE]
        validate(need(nrow(plot_data) >= 2, "At least two valid time-stamped measurements are required."))
        plot_data$period <- as.Date(plot_data$time)
        daily <- aggregate(value ~ period, data=plot_data, FUN=mean, na.rm=TRUE)
        validate(need(nrow(daily) >= 2, "At least two dates are required for a time-series line plot."))
        ggplot(daily, aes(x=period, y=value)) +
          geom_line(color="#2486B9", linewidth=.8) +
          geom_point(color="#18324D", size=1.3) +
          labs(title=paste("Daily Time-Series of", v),
               subtitle="Daily average of valid measurements",
               x="Date", y=v) +
          theme_minimal(base_size=12)
      }
    }, error=function(e) {
      plot.new()
      text(.5, .5, paste("Cannot create this visualization:\n", conditionMessage(e)))
    })
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
