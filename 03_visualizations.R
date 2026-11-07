# PERSON 2: VISUALIZATION AND EXPLORATORY DATA ANALYSIS
# Functions are defined here; no plots run automatically when this file is sourced.

check_variable <- function(data, variable) {
  if (length(variable) != 1L || is.na(variable) || !variable %in% names(data))
    stop("Variable not found in dataset: ", variable)
  invisible(TRUE)
}
check_numeric <- function(data, variable) {
  check_variable(data, variable)
  if (!is.numeric(data[[variable]])) stop("Variable must be numeric: ", variable)
  invisible(TRUE)
}
prepare_smog_data <- function(data) {
  if ("timestamp" %in% names(data) && !inherits(data$timestamp, "POSIXct")) {
    if (!requireNamespace("lubridate", quietly=TRUE)) stop("Install lubridate to parse timestamps.")
    data$timestamp <- suppressWarnings(lubridate::parse_date_time(as.character(data$timestamp),
      orders=c("ymd HMS","ymd HM","ymd","dmy HMS","dmy HM","dmy","mdy HMS","mdy HM","mdy"), quiet=TRUE))
  }
  if ("city" %in% names(data)) data$city <- as.factor(data$city)
  # Keep hazardous_event as its original numeric 0/1 or categorical representation.
  data
}

plot_histogram <- function(data, variable, bins=30) {
  check_numeric(data, variable)
  ggplot2::ggplot(data, ggplot2::aes(x=.data[[variable]])) +
    ggplot2::geom_histogram(bins=bins, fill="#2486B9", color="white", na.rm=TRUE) +
    ggplot2::labs(title=paste("Histogram of",variable), x=variable, y="Frequency") + ggplot2::theme_minimal()
}
plot_density <- function(data, variable) {
  check_numeric(data, variable)
  if (length(unique(stats::na.omit(data[[variable]]))) < 2) stop("Density plot requires at least two distinct values.")
  ggplot2::ggplot(data, ggplot2::aes(x=.data[[variable]])) + ggplot2::geom_density(fill="#63B3D3", alpha=.5, na.rm=TRUE) +
    ggplot2::labs(title=paste("Density plot of",variable),x=variable,y="Density") + ggplot2::theme_minimal()
}
plot_boxplot <- function(data, variable, group=NULL) {
  check_numeric(data, variable)
  if (is.null(group)) return(ggplot2::ggplot(data,ggplot2::aes(y=.data[[variable]]))+ggplot2::geom_boxplot(fill="#63B3D3",na.rm=TRUE)+ggplot2::labs(title=paste("Boxplot of",variable),y=variable)+ggplot2::theme_minimal())
  check_variable(data,group)
  ggplot2::ggplot(data,ggplot2::aes(x=.data[[group]],y=.data[[variable]]))+ggplot2::geom_boxplot(fill="#63B3D3",na.rm=TRUE)+ggplot2::labs(title=paste(variable,"by",group),x=group,y=variable)+ggplot2::theme_minimal()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1))
}
plot_bar <- function(data, variable) {
  check_variable(data,variable)
  counts <- as.data.frame(table(data[[variable]], useNA="ifany")); names(counts)<-c("category","count")
  ggplot2::ggplot(counts,ggplot2::aes(x=category,y=count))+ggplot2::geom_col(fill="#2486B9")+ggplot2::labs(title=paste("Bar chart of",variable),x=variable,y="Count")+ggplot2::theme_minimal()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=40,hjust=1))
}
plot_pie <- function(data, variable) {
  check_variable(data,variable)
  counts <- as.data.frame(table(data[[variable]], useNA="ifany")); names(counts)<-c("category","count"); counts<-counts[counts$count>0,,drop=FALSE]
  ggplot2::ggplot(counts,ggplot2::aes(x="",y=count,fill=category))+ggplot2::geom_col(width=1)+ggplot2::coord_polar(theta="y")+ggplot2::labs(title=paste("Distribution of",variable),fill=variable)+ggplot2::theme_void()
}
plot_line <- function(data, variable, time_unit="day", group=NULL) {
  check_numeric(data,variable); check_variable(data,"timestamp")
  if (!inherits(data$timestamp,"POSIXct")) data <- prepare_smog_data(data)
  if (!time_unit %in% c("hour","day","month")) stop("time_unit must be hour, day, or month.")
  if (!requireNamespace("lubridate",quietly=TRUE)) stop("Install lubridate.")
  data$time_group <- lubridate::floor_date(data$timestamp,unit=time_unit)
  if (is.null(group)) {
    summary_data <- stats::aggregate(data[[variable]], list(time_group=data$time_group), function(x) if(all(is.na(x))) NA_real_ else mean(x,na.rm=TRUE))
    names(summary_data)[2]<-"value"
    return(ggplot2::ggplot(summary_data,ggplot2::aes(x=time_group,y=value))+ggplot2::geom_line(na.rm=TRUE,color="#2486B9")+ggplot2::labs(title=paste(variable,"trend by",time_unit),x="Time",y=paste("Mean",variable))+ggplot2::theme_minimal())
  }
  check_variable(data,group)
  summary_data <- aggregate(data[[variable]], list(time_group=data$time_group, group=data[[group]]), function(x) if(all(is.na(x))) NA_real_ else mean(x,na.rm=TRUE)); names(summary_data)[3]<-"value"
  ggplot2::ggplot(summary_data,ggplot2::aes(x=time_group,y=value,color=group,group=group))+ggplot2::geom_line(na.rm=TRUE)+ggplot2::labs(title=paste(variable,"trend by",group),x="Time",y=paste("Mean",variable),color=group)+ggplot2::theme_minimal()
}
plot_scatter <- function(data,x,y,group=NULL) {
  check_numeric(data,x); check_numeric(data,y)
  if (is.null(group)) return(ggplot2::ggplot(data,ggplot2::aes(x=.data[[x]],y=.data[[y]]))+ggplot2::geom_point(alpha=.25,na.rm=TRUE)+ggplot2::labs(title=paste(y,"vs",x),x=x,y=y)+ggplot2::theme_minimal())
  check_variable(data,group)
  ggplot2::ggplot(data,ggplot2::aes(x=.data[[x]],y=.data[[y]],color=.data[[group]]))+ggplot2::geom_point(alpha=.25,na.rm=TRUE)+ggplot2::labs(title=paste(y,"vs",x),x=x,y=y,color=group)+ggplot2::theme_minimal()
}
plot_regression <- function(data,x,y) {
  check_numeric(data,x); check_numeric(data,y)
  ggplot2::ggplot(data,ggplot2::aes(x=.data[[x]],y=.data[[y]]))+ggplot2::geom_point(alpha=.2,na.rm=TRUE)+ggplot2::geom_smooth(method="lm",formula=y~x,se=TRUE,na.rm=TRUE)+ggplot2::labs(title=paste("Regression:",y,"vs",x),x=x,y=y)+ggplot2::theme_minimal()
}
plot_city_comparison <- function(data,pollutant="pm2_5_ug_m3") {
  check_numeric(data,pollutant); check_variable(data,"city")
  d <- data.frame(city=data$city,value=data[[pollutant]])
  d <- d[!is.na(d$city)&is.finite(d$value),]
  if(!nrow(d)) stop("No valid city/pollutant rows.")
  means <- aggregate(value~city,d,mean)
  ggplot2::ggplot(means,ggplot2::aes(x=reorder(city,value),y=value))+ggplot2::geom_col(fill="#2486B9")+ggplot2::coord_flip()+ggplot2::labs(title=paste("Mean",pollutant,"by city"),x="City",y=paste("Mean",pollutant))+ggplot2::theme_minimal()
}
plot_month_comparison <- function(data,pollutant="pm2_5_ug_m3") {
  check_numeric(data,pollutant); if(!inherits(data$timestamp,"POSIXct")) data<-prepare_smog_data(data)
  if(!requireNamespace("lubridate",quietly=TRUE)) stop("Install lubridate.")
  d <- data.frame(month=lubridate::month(data$timestamp,label=TRUE,abbr=TRUE),value=data[[pollutant]])
  d <- d[!is.na(d$month)&is.finite(d$value),]
  if(!nrow(d)) stop("No valid month/pollutant rows.")
  means <- aggregate(value~month,d,mean)
  ggplot2::ggplot(means,ggplot2::aes(x=month,y=value,group=1))+ggplot2::geom_col(fill="#2486B9")+ggplot2::labs(title=paste("Mean",pollutant,"by month"),x="Month",y=paste("Mean",pollutant))+ggplot2::theme_minimal()
}
plot_hazardous_comparison <- function(data,pollutant="pm2_5_ug_m3") {
  check_numeric(data,pollutant); check_variable(data,"hazardous_event")
  ggplot2::ggplot(data,ggplot2::aes(x=as.factor(hazardous_event),y=.data[[pollutant]]))+ggplot2::geom_boxplot(fill="#63B3D3",na.rm=TRUE)+ggplot2::labs(title=paste(pollutant,"by hazardous event"),x="Hazardous event",y=pollutant)+ggplot2::theme_minimal()
}

# EDA summaries contributed by Person 2
eda_numeric_summary <- function(data) {
  vars<-names(data)[vapply(data,is.numeric,logical(1))]
  if(!length(vars)) return(data.frame())
  do.call(rbind,lapply(vars,function(nm){x<-data[[nm]];x<-x[is.finite(x)];data.frame(variable=nm,n=length(x),mean=if(length(x))mean(x) else NA_real_,median=if(length(x))median(x) else NA_real_,sd=if(length(x)>1)sd(x) else NA_real_,min=if(length(x))min(x) else NA_real_,Q1=if(length(x))unname(quantile(x,.25)) else NA_real_,Q3=if(length(x))unname(quantile(x,.75)) else NA_real_,max=if(length(x))max(x) else NA_real_)}))
}
eda_missing_summary <- function(data) data.frame(variable=names(data),missing=vapply(data,function(x)sum(is.na(x)),integer(1)),percentage=round(vapply(data,function(x)mean(is.na(x))*100,numeric(1)),4))
eda_outlier_summary <- function(data, variables=NULL) {
  vars<-names(data)[vapply(data,is.numeric,logical(1))]; if(is.null(variables)) variables<-vars; variables<-intersect(variables,vars)
  if(!length(variables)) return(data.frame(variable=character(),outliers=integer(),percentage=numeric()))
  do.call(rbind,lapply(variables,function(nm){x<-data[[nm]];x<-x[is.finite(x)];if(length(x)<4)return(data.frame(variable=nm,outliers=0,percentage=NA_real_));q<-quantile(x,c(.25,.75));iq<-q[2]-q[1];n<-sum(x<q[1]-1.5*iq|x>q[2]+1.5*iq);data.frame(variable=nm,outliers=n,percentage=100*n/length(x))}))
}
eda_group_summary <- function(data,group,variable) {
  check_variable(data,group);check_numeric(data,variable);d<-data.frame(group=data[[group]],value=data[[variable]]);d<-d[!is.na(d$group),]
  groups<-split(d$value,d$group,drop=TRUE)
  do.call(rbind,lapply(names(groups),function(g){x<-groups[[g]];x<-x[is.finite(x)];data.frame(group=g,n=length(x),mean=if(length(x))mean(x) else NA_real_,median=if(length(x))median(x) else NA_real_,sd=if(length(x)>1)sd(x) else NA_real_,min=if(length(x))min(x) else NA_real_,max=if(length(x))max(x) else NA_real_)}))
}
eda_correlation <- function(data,variables=NULL) {
  vars<-names(data)[vapply(data,is.numeric,logical(1))];if(is.null(variables))variables<-vars
  if(length(variables)<2||!all(variables%in%vars))stop("Choose at least two numeric variables.")
  cor(data[variables],use="pairwise.complete.obs")
}

# Compatibility wrappers used by the initial dashboard version
plot_bar_chart <- plot_bar
plot_pie_chart <- plot_pie
plot_line_graph <- function(data,time_variable,value_variable) {
  if (identical(time_variable,"timestamp")) return(plot_line(data,value_variable,time_unit="day"))
  check_variable(data,time_variable);check_numeric(data,value_variable)
  d<-data.frame(time=data[[time_variable]],value=data[[value_variable]]);d<-d[!is.na(d$time)&is.finite(d$value),];d<-d[order(d$time),]
  ggplot2::ggplot(d,ggplot2::aes(x=time,y=value))+ggplot2::geom_line(color="#2486B9",na.rm=TRUE)+ggplot2::labs(title=paste(value_variable,"by",time_variable),x=time_variable,y=value_variable)+ggplot2::theme_minimal()
}
