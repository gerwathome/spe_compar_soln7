#------------------------------------------------------------------------------
# Create a dataframe with all of the digitized data suitable for plotting
# desired data frame format:
# Figure Case Time_in_days Parameter Units Value
#
# figs 3, 4, 5, 6, 7, 8, 18, and 19 have two Y values: opr and opc
# the second Y (opc) was digitized on the opr scale, and needs to be adjusted
# figs 15, 16, and 17 have two cases plotted on one graph
# figs 23 and 24 have a log scale

#-- format from eclipse data:
# data.frame(CASE=df[,1],
#            DAYS=df[,2],
#            DATE=df[,3],
#            WGNAME=rep(wgn[i],ndays),
#            KEYWORD=rep(kw[i],ndays), 
#            VALUE=df[,3+i])

# Combined format for paper and simulation data
# BASE VARIANT CASENAME DAYS DATE WGNAME KEYWORD VALUE UNITS COMMENT
#
# BASE SPE21221 SPE7 SPE7LGR
# VARIANT 1A 1B ... 4B
# CASENAME SPE21221_1A SPE7LGR_1A
# DAYS
# DATE
# WGNAME PROD INJ
# KEYWORD WOPR WOPT
# VALUE 12354
# UNITS STBD SCFSTB
# COMMENT Figure_3

dbg <- TRUE
library(tidyverse)

base_dir <- "/home/gerw/gitrepos/spe_compar_soln7/figures/"

#------------------------------------------------------------------------------
# this file was manually created while digitizing the plots
fig_data <- read.csv(file=paste0(base_dir, "figures.csv"),
                     stringsAsFactors=FALSE)
# names(fig_data)
# [1] "File_Name"    "Case"         "X"            "Y1"           "Y1_Min"      
# [6] "Y1_Max"       "Y1_Scale"     "Y2"           "Y2_Min"       "Y2_Max"      
# [11] "Y2_Scale"     "Base_Y_Scale" "Y2_Y1_Slope"  "Y2_Intercept"
#------------------------------------------------------------------------------
# this is for converting names in dig files to standard names
stdkw <- function(keyword){
    std <- switch(keyword,
                  "opr" = "WOPR",      # Oil Prod Rate
                  "wpr" = "WWPR",      # Water Prod Rate
                  "gpr" = "WGPR",      # Gas Prod Rate
                  "opc" = "WOPT",      # Cum Oil Prod
                  "wpc" = "WWPT",      # Cum Water Prod
                  "gpc" = "WGPT",      # Cum Gas Prod
                  "gor" = "WGOR",      # Gas Oil Ratio
                  "wor" = "WWOR",      # Water Oil Ratio
                  "bhp" = "WBHP",      # Bottom Hole Pressure
                  "dp"  = "WBDP"       # well bore pressure drop
    )
    return(std)
}

#------------------------------------------------------------------------------
# This puts together the logic to import the digitized files
fig_names <- paste0(rep("Figure_",28), 3:30)
figs <- paste0(rep("fig",28), 3:30)
fns <- paste0(rep(base_dir,28), figs, rep(".csv",28))
# df.col.names <- c("Figure", "Case", "Time_in_days", "Parameter",
#                   "Units", "Value")
figs.df <- data.frame(
    BASE=character(),
    VARIANT=character(),
    CASENAME=character(),
    DAYS=numeric(),
    DATE=as.Date(character()),
    WGNAME=character(),
    KEYWORD=character(),
    VALUE=numeric(),
    UNITS=character(),
    COMMENT=character(),
    stringsAsFactors=FALSE)
#------------------------------------------------------------------------------
for(i in 1:length(figs)){
    COMMENT <- fig_names[i]
    figname <- figs[i]
    data_row <- grep(paste0('^', figname, '$'),
                     fig_data$File_Name, perl = TRUE)
    VARIANT <- toupper(fig_data[data_row, "Case"])
    VARIANTS <- strsplit(VARIANT,'&',fixed=TRUE)[[1]]
    if(dbg){paste0("at i = ",i," VARIANTS = ",VARIANTS)}
    UNITS <- toupper(fig_data[data_row, "Y1"])
    UNITS <- strsplit(UNITS,'_',fixed=TRUE)[[1]][2]
    df.in <- read.csv(file=fns[i],stringsAsFactors=FALSE)
    params <- names(df.in)[2:length(names(df.in))]
    if(dbg){paste0("at i = ",i,"params[1] = ",params[1])}
    if(dbg){paste0("at i = ",i,"names(df.in) = ",names(df.in))}
    df.out <- data.frame(
        BASE=rep("SPE21221", nrow(df.in)),
        VARIANT=rep(VARIANTS[1], nrow(df.in)),
        CASENAME=rep(paste0("SPE21221_",VARIANTS[1]), nrow(df.in)),
        DAYS=df.in[,"x"],
        WGNAME=rep("PROD", nrow(df.in)),
        KEYWORD=rep(toupper(params[1]), nrow(df.in)),
        VALUE=df.in[,params[1]],
        UNITS=rep(UNITS, nrow(df.in)),
        COMMENT=rep(COMMENT, nrow(df.in)),
        stringsAsFactors=FALSE)
    figs.df <- rbind(figs.df,df.out)
    #------------------------------------------------------------------------------
    # duplicate the entry with a new case name for figures with 2 cases
    if(length(VARIANTS)==2){
        df.out <- data.frame(
            BASE=rep("SPE21221", nrow(df.in)),
            VARIANT=rep(VARIANTS[2], nrow(df.in)),
            CASENAME=rep(paste0("SPE21221_",VARIANTS[2]), nrow(df.in)),
            DAYS=df.in[,"x"],
            WGNAME=rep("PROD", nrow(df.in)),
            KEYWORD=rep(toupper(params[1]), nrow(df.in)),
            VALUE=df.in[,params[1]],
            UNITS=rep(UNITS, nrow(df.in)),
            COMMENT=rep(COMMENT, nrow(df.in)),
            stringsAsFactors=FALSE)
        figs.df <- rbind(figs.df,df.out)
    }
    #------------------------------------------------------------------------------
    # if there are two Y axes, grab the second one
    if(length(params)==2){
        UNITS <- toupper(fig_data[data_row, "Y2"])
        UNITS <- strsplit(UNITS,'_',fixed=TRUE)[[1]][2]
        vals <- df.in[,params[2]]
        y1 <- fig_data[data_row, "Y2_Min"]
        y2 <- fig_data[data_row, "Y2_Max"]
        x1 <- fig_data[data_row, "Y1_Min"]
        x2 <- fig_data[data_row, "Y1_Max"]
        m <- (y2-y1)/(x2-x1)
        b <- y1 - m * x1
        new_vals <- m * vals + b
        df.out <- data.frame(
            BASE=rep("SPE21221", nrow(df.in)),
            VARIANT=rep(VARIANTS[1], nrow(df.in)),
            CASENAME=rep(paste0("SPE21221_",VARIANTS[1]), nrow(df.in)),
            DAYS=df.in[,"x"],
            WGNAME=rep("PROD", nrow(df.in)),
            KEYWORD=rep(toupper(params[2]), nrow(df.in)),
            VALUE=new_vals,
            UNITS=rep(UNITS, nrow(df.in)),
            COMMENT=rep(COMMENT, nrow(df.in)),
            stringsAsFactors=FALSE)
        figs.df <- rbind(figs.df,df.out)
    }
}
#------------------------------------------------------------------------------
# summary(figs.df)
# head(figs.df)
# tail(figs.df)
# str(figs.df)
figs.df <- mutate(figs.df, VARIANT_KEYWORD = paste(VARIANT, KEYWORD, sep="_"))
cps <- sort(unique(figs.df$VARIANT_KEYWORD))

pdf(paste0(base_dir,"QC_digitizing.pdf"))
for (cp in cps){
    filt <- figs.df$VARIANT_KEYWORD == cp
    mrow <- min(grep(TRUE,filt))
    fig <- figs.df$COMMENT[mrow]
    case <- figs.df$VARIANT[mrow]
    param <- figs.df$KEYWORD[mrow]
    units <- figs.df$UNITS[mrow]
    x <- figs.df$DAYS[filt]
    y <- figs.df$VALUE[filt]
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
data.filt <- data.frame("2a_opr" = figs.df$VARIANT_KEYWORD != "2a_opr" | figs.df$VALUE < 3000)
data.filt <- data.frame(data.filt, "1a_opr" = figs.df$VARIANT_KEYWORD != "1a_opr" | figs.df$VALUE > 100)
data.filt <- data.frame(data.filt, "2b_opc" = figs.df$VARIANT_KEYWORD != "2b_opc" | figs.df$VALUE < 1350)
data.filt <- data.frame(data.filt, "3a_opr" = figs.df$VARIANT_KEYWORD != "3a_opr" | figs.df$VALUE > 0)
data.filt <- data.frame(data.filt, "3b_opc" = figs.df$VARIANT_KEYWORD != "3b_opc" | 
                            (figs.df$VALUE < 600 & figs.df$DAYS < 150) |
                            (figs.df$DAYS >= 150 & figs.df$DAYS <= 1450) |
                            (figs.df$VALUE > 1100 & figs.df$DAYS > 1450))
data.filt <- data.frame(data.filt, "4a_opr" = figs.df$VARIANT_KEYWORD != "4a_opr" | 
                            (figs.df$DAYS >= 0 & figs.df$DAYS <= 1450) |
                            (figs.df$VALUE < 500 & figs.df$DAYS > 1450))
data.filt <- data.frame(data.filt, "4b_opc" = figs.df$VARIANT_KEYWORD != "4b_opc" | figs.df$VALUE > 300)
data.filt <- data.frame(data.filt, "4b_bhp" = figs.df$VARIANT_KEYWORD != "4b_bhp" | 
                            (figs.df$DAYS >= 100 & figs.df$DAYS <= 1600) |
                            (figs.df$VALUE >3000 & figs.df$DAYS < 100))
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
    filt <- plot.df$VARIANT_KEYWORD == cp
    mrow <- min(grep(TRUE,filt))
    fig <- plot.df$COMMENT[mrow]
    case <- plot.df$VARIANT[mrow]
    param <- plot.df$KEYWORD[mrow]
    units <- plot.df$UNITS[mrow]
    main = paste0("Case ", case, " from SPE21221 COMMENT ", fig)
    ylab = paste0(param, ", ", units)
    xlab = "Time, in days"
    
    ggp <- ggplot(plot.df[filt,], aes(x=DAYS, y=VALUE, color=Data_Source))
    ggp <- ggp + geom_point()
    ggp <- ggp + geom_smooth()
    ggp <- ggp + labs(title = main,
                      x = xlab,
                      y = ylab)
    print(ggp)
}
dev.off()
#------------------------------------------------------------------------------



