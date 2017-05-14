# work file for digitizing spe 7th comp sol data
# and preparing and running an OPM flow model to add to it
#------------------------------------------------------------------------------
# first get the spe paper
require(XML)
url <- ('www.ipt.ntnu.no/~kleppe/pub/SPE-COMPARATIVE/papers/seventh.pdf')
download.file(url, 'spe21221.pdf')
url <- ('www.ipt.ntnu.no/~kleppe/pub/SPE-COMPARATIVE/papers/second.pdf')
download.file(url, 'spe10489.pdf')


#  1. manually use gnome screenshot to save individual tables
#  2. use tesseract table1.png table1 to create table1.txt
#  3. cp table1.txt to csv and edit as needed
#  4. paste into spreadsheet
#  5. import results into R
#  6. use engauge to digiize the plots
#  7. paste into spreadsheet
#  8. import Results into R
#  8. Plot Results in R and compare to paper
#  9. write flow deck for 8 cases
# 10. run models from R and bring in results

#------------------------------------------------------------------------------
# thickness perm initial pressure and saturation
system("tesseract table1.png table1")
system("cp table1.txt table1.csv")

# saturated oil props
system("tesseract table2.png table2")
system("cp table2.txt table2.csv")

# water oil scal
system("tesseract table4a.png table4a")
system("cp table4a.txt table4a.csv")

# gas oil scal
system("tesseract table4b.png table4b")
system("cp table4b.txt table4b.csv")

# well defs
system("tesseract table6.png table6")
system("cp table6.txt table6.csv")

# Results:  cum oil at 1500 days
system("tesseract table8.png table8")
system("cp table8.txt table8.csv")

# Results:  bhp at 1500 days
system("tesseract table9.png table9")
system("cp table9.txt table9.csv")

# Results:  Wellbore delta p at 1500 days
system("tesseract table10.png table10")
system("cp table10.txt table10.csv")

# PVT from spe10489
tabdir <- "/home/gerw/gitrepos/spe_compar_soln7/tables/"
system(paste0("tesseract ", tabdir, "spe10489_table5_pvt.png ",
              tabdir, "spe10489_table5_pvt"))
system(paste0("cp ",tabdir, "spe10489_table5_pvt.txt ",
              tabdir, "spe10489_table5_pvt.csv"))
# edit csv files by hand
#------------------------------------------------------------------------------
# Create a dataframe with all of the digitized data suitable for plotting
# desired data frame format:
# Figure Case Time_in_days Parameter Units Value
#
# figs 3, 4, 5, 6, 7, 8, 18, and 19 have two Y values: opr and opc
# the second Y (opc) was digitized on the opr scale, and needs to be adjusted
# figs 15, 16, and 17 have two cases plotted on one graph
# figs 23 and 24 have a log scale
library(tidyverse)

base_dir <- "/home/gerw/gitrepos/spe_compar_soln7/figures/"
fig_data <- read.csv(file=paste0(base_dir, "figures.csv"),
                     stringsAsFactors=FALSE)
figs <- paste0(rep("fig",28), 3:30)
fns <- paste0(rep(base_dir,28), figs, rep(".csv",28))
df.col.names <- c("Figure", "Case", "Time_in_days", "Parameter",
                  "Units", "Value")
figs.df <- data.frame(Figure=numeric(),
                      Case=character(),
                      Time_in_days=numeric(),
                      Parameter=character(),
                      Units=character(),
                      Value=numeric(),
                      stringsAsFactors=FALSE)
#------------------------------------------------------------------------------
for(i in 1:length(figs)){
    figname <- figs[i]
    fignum <- strtoi(sub("fig", "", figname))
    data_row <- grep(paste0('^', figname, '$'),
                     fig_data$File_Name, perl = TRUE)
    case <- fig_data[data_row, "Case"]
    cases <- strsplit(case,'&',fixed=TRUE)[[1]]
    units <- fig_data[data_row, "Y1"]
    units <- strsplit(units,'_',fixed=TRUE)[[1]][2]
    df.in <- read.csv(file=fns[i],stringsAsFactors=FALSE)
    params <- names(df.in)[2:length(names(df.in))]
    df.out <- data.frame(Figure=rep(fignum, nrow(df.in)),
                         Case=rep(cases[1], nrow(df.in)),
                         Time_in_days=df.in[,"x"],
                         Parameter=rep(params[1], nrow(df.in)),
                         Units=rep(units, nrow(df.in)),
                         Value=df.in[,params[1]],
                         stringsAsFactors=FALSE)
    figs.df <- rbind(figs.df,df.out)
    #------------------------------------------------------------------------------
    # duplicate the entry with a new case name for figures with 2 cases
    if(length(cases)==2){
        df.out <- data.frame(Figure=rep(fignum, nrow(df.in)),
                             Case=rep(cases[2], nrow(df.in)),
                             Time_in_days=df.in[,"x"],
                             Parameter=rep(params[1], nrow(df.in)),
                             Units=rep(units, nrow(df.in)),
                             Value=df.in[,params[1]],
                             stringsAsFactors=FALSE)
        figs.df <- rbind(figs.df,df.out)
    }
    #------------------------------------------------------------------------------
    # if there are two Y axes, grab the second one
    if(length(params)==2){
        units <- fig_data[data_row, "Y2"]
        units <- strsplit(units,'_',fixed=TRUE)[[1]][2]
        vals <- df.in[,params[2]]
        y1 <- fig_data[data_row, "Y2_Min"]
        y2 <- fig_data[data_row, "Y2_Max"]
        x1 <- fig_data[data_row, "Y1_Min"]
        x2 <- fig_data[data_row, "Y1_Max"]
        m <- (y2-y1)/(x2-x1)
        b <- y1 - m * x1
        new_vals <- m * vals + b
        df.out <- data.frame(Figure=rep(fignum, nrow(df.in)),
                             Case=rep(cases[1], nrow(df.in)),
                             Time_in_days=df.in[,"x"],
                             Parameter=rep(params[2], nrow(df.in)),
                             Units=rep(units, nrow(df.in)),
                             Value=new_vals,
                             stringsAsFactors=FALSE)
        figs.df <- rbind(figs.df,df.out)
    }
}
#------------------------------------------------------------------------------
# str(figs.df)
figs.df <- mutate(figs.df, Case_Parameter = paste(Case, Parameter, sep="_"))
cps <- sort(unique(figs.df$Case_Parameter))

pdf(paste0(base_dir,"QC_digitizing.pdf"))
for (cp in cps){
    filt <- figs.df$Case_Parameter == cp
    mrow <- min(grep(TRUE,filt))
    fig <- figs.df$Figure[mrow]
    case <- figs.df$Case[mrow]
    param <- figs.df$Parameter[mrow]
    units <- figs.df$Units[mrow]
    x <- figs.df$Time_in_days[filt]
    y <- figs.df$Value[filt]
    plot(x,y,
         type = "p",
         main = paste0("Case is ", case, " from figure ", fig),
         ylab = paste0(param, ", ", units),
         xlab = "Time, in days"
         )
}
dev.off()
#------------------------------------------------------------------------------
# cps
#  [1] "1a_opc" "1a_opr" "1a_wor" "1a_wpc" "1b_opc" "1b_opr" "1b_wor" "1b_wpc" "2a_opc" "2a_opr" "2a_wor"
# [12] "2a_wpc" "2b_opc" "2b_opr" "2b_wor" "2b_wpc" "3a_opc" "3a_opr" "3a_wor" "3a_wpc" "3b_opc" "3b_opr"
# [23] "3b_wor" "3b_wpc" "4a_bhp" "4a_dp"  "4a_gor" "4a_gpc" "4a_opc" "4a_opr" "4a_wpc" "4a_wpr" "4b_bhp"
# [34] "4b_dp"  "4b_gor" "4b_gpc" "4b_opc" "4b_opr" "4b_wpc" "4b_wpr"
# the following are manual filters added by comparing the QC plots to the plots in the paper
data.filt <- data.frame("2a_opr" = figs.df$Case_Parameter != "2a_opr" | figs.df$Value < 3000)
data.filt <- data.frame(data.filt, "1a_opr" = figs.df$Case_Parameter != "1a_opr" | figs.df$Value > 100)
data.filt <- data.frame(data.filt, "2b_opc" = figs.df$Case_Parameter != "2b_opc" | figs.df$Value < 1350)
data.filt <- data.frame(data.filt, "3a_opr" = figs.df$Case_Parameter != "3a_opr" | figs.df$Value > 0)
data.filt <- data.frame(data.filt, "3b_opc" = figs.df$Case_Parameter != "3b_opc" | 
                            (figs.df$Value < 600 & figs.df$Time_in_days < 150) |
                            (figs.df$Time_in_days >= 150 & figs.df$Time_in_days <= 1450) |
                            (figs.df$Value > 1100 & figs.df$Time_in_days > 1450))
data.filt <- data.frame(data.filt, "4a_opr" = figs.df$Case_Parameter != "4a_opr" | 
                            (figs.df$Time_in_days >= 0 & figs.df$Time_in_days <= 1450) |
                            (figs.df$Value < 500 & figs.df$Time_in_days > 1450))
data.filt <- data.frame(data.filt, "4b_opc" = figs.df$Case_Parameter != "4b_opc" | figs.df$Value > 300)
data.filt <- data.frame(data.filt, "4b_bhp" = figs.df$Case_Parameter != "4b_bhp" | 
                            (figs.df$Time_in_days >= 100 & figs.df$Time_in_days <= 1600) |
                            (figs.df$Value >3000 & figs.df$Time_in_days < 100))
data.filt <- data.frame(data.filt, "all" = apply(data.filt,1,all))
# summary(data.filt)
#   X2a_opr         X3a_opr         X3b_opc         X4a_opr         X4b_opc           all         
# Mode :logical   Mode :logical   Mode :logical   Mode :logical   Mode :logical   Mode :logical  
# FALSE:2         FALSE:1         FALSE:5         FALSE:1         FALSE:3         FALSE:12       
# TRUE :2617      TRUE :2618      TRUE :2614      TRUE :2618      TRUE :2616      TRUE :2607     
# NA's :0         NA's :0         NA's :0         NA's :0         NA's :0         NA's :0        

#------------------------------------------------------------------------------
# filter out 12 bad digitized points
# after the sim runs, the data will be added here for plotting
plot.df <- figs.df[data.filt$all,]
plot.df <- mutate(plot.df, Data_Source = rep("SPE_Paper",nrow(plot.df)))
save(plot.df, file=paste0(base_dir,"plot_df.Rda"))

pdf(paste0(base_dir,"data_plot.pdf"))
for (cp in cps){
    filt <- plot.df$Case_Parameter == cp
    mrow <- min(grep(TRUE,filt))
    fig <- plot.df$Figure[mrow]
    case <- plot.df$Case[mrow]
    param <- plot.df$Parameter[mrow]
    units <- plot.df$Units[mrow]
    main = paste0("Case ", case, " from SPE21221 Figure ", fig)
    ylab = paste0(param, ", ", units)
    xlab = "Time, in days"
    
    ggp <- ggplot(plot.df[filt,], aes(x=Time_in_days, y=Value, color=Data_Source))
    ggp <- ggp + geom_point()
    ggp <- ggp + geom_smooth()
    ggp <- ggp + labs(title = main,
                      x = xlab,
                      y = ylab)
    print(ggp)
}
dev.off()
#------------------------------------------------------------------------------

case <-  "/home/gerw/gitrepos/spe_compar_soln7/sim_output/SPE7CASE1A/SPE7CASE1A"
ecl_sum <- function(case){
    library(rPython)
    python.exec("import sys")
    python.exec("import ert.ecl.ecl as ecl")
    python.assign("case",case)
    # need to add some logic to deal with the case when EclSum fails for some
    #   reason
    try_err <- try(python.exec("sum_data = ecl.EclSum(case)"),silent=TRUE)
    if(is.null(try_err)){
        python.exec("sum_data.exportCSV(case + '.csv', date_format='%d-%b-%Y',
                    sep=',')")
        python.assign("sum_data","None")
        rslts <- read.csv(file=paste0(case,".csv"))
        rslts$DATE <- as.Date(rslts$DATE, "%d-%b-%Y")
        casename <- basename(case)
        rslts <- data.frame(CASE=rep(casename,length(rslts$DATE)),rslts)}
    else{rslts <- data.frame(CASE=character(0), DAYS=numeric(0))}
    return(rslts)
}

test <- ecl_sum(case)


