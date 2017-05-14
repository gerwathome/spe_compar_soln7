# create functions to convert eclipse output to csv
# load csv files
# import plot data from SPE paper
# plot runs with data from paper

#------------------------------------------------------------------------------
# Load sim results using rPython
# This function uses the c and python libraries associated with ERT, which must
#   be installed.
# http://ert.nr.no/ert/index.php/Main_Page

# writes out csv file with unsmry data, and then reads it into a data frame
# taking it directly from python would be nice, but I don't know how, yet

# This function expects the "case" to be the full path to where the simulation 
#   output is, including the base name, but not the file extent.
ecl_sum <- function(case){
    library(rPython)
    python.exec("import sys")
    python.exec("import ert.ecl.ecl as ecl")
    python.assign("case",case)
    # need to add some logic to deal with the case when EclSum fails for some reason
    try_err <- try(python.exec("sum_data = ecl.EclSum(case)"),silent=TRUE)
    if(is.null(try_err)){
        python.exec("sum_data.exportCSV(case + '.csv', date_format='%d-%b-%Y', sep=',')")
        python.assign("sum_data","None")
        rslts <- read.csv(file=paste0(case,".csv"))
        rslts$DATE <- as.Date(rslts$DATE, "%d-%b-%Y")
        casename <- basename(case)
        rslts <- data.frame(CASE=rep(casename,length(rslts$DATE)),rslts)}
    else{rslts <- data.frame(CASE=character(0), DAYS=numeric(0))}
    return(rslts)
}
dbg <- FALSE
# testdf <- as.data.frame()
if(dbg){
    testdir <- "/home/gerw/gitrepos/spe_compar_soln7/sim_output/SPE7_1A/SPE7_1A"
    testdf <- ecl_sum(testdir)
    summary(testdf)
}

#------------------------------------------------------------------------------
# This retreives results from a bunch of runs
ecl_sum_cases <- function(cases){
    df <- data.frame(CASE=character(0), DAYS=numeric(0))
    library(dplyr)
    for(case in cases[1:length(cases)]){
        df <- suppressWarnings(suppressMessages(full_join(df, ecl_sum(case)))) 
    }
    return(df)
}

#------------------------------------------------------------------------------
# this isn't called directly, but used by add_gor and add_wor
wgnames <- function(df){
    pat <- "^\\w+\\.(\\w+)$";
    kw.wgn <- colnames(df)[grep(pat, colnames(df), perl=TRUE)]
    wgn <- sub(pat, "\\1", kw.wgn, perl=TRUE)
    return(unique(wgn))
}

#------------------------------------------------------------------------------
add_gor <- function(df){
    wells <- wgnames(df)
    for(well in wells){
        vars <- colnames(df)
        opr <- paste0("WOPR.",well)
        oprcol <- grep(opr, vars, fixed=TRUE)
        gpr <- paste0("WGPR.",well)
        gprcol <- grep(gpr, vars, fixed=TRUE)
        gor <- paste0("WGOR.",well)
        if(oprcol & gprcol){
            tmp <- df[,gprcol]/df[,oprcol]*1000
            tmp[df[,oprcol]==0] <- 0
            df <- cbind(df,tmp)
            colnames(df) <- c(vars,gor)
        }
    }
    return(df)
}

#------------------------------------------------------------------------------
add_wor <- function(df){
    wells <- wgnames(df)
    for(well in wells){
        vars <- colnames(df)
        opr <- paste0("WOPR.",well)
        oprcol <- grep(opr, vars, fixed=TRUE)
        wpr <- paste0("WWPR.",well)
        wprcol <- grep(wpr, vars, fixed=TRUE)
        wor <- paste0("WWOR.",well)
        if(oprcol & wprcol){
            tmp <- df[,wprcol]/df[,oprcol]
            tmp[df[,oprcol]==0] <- 0
            df <- cbind(df,tmp)
            colnames(df) <- c(vars,wor)
        }
    }
    return(df)
}

#------------------------------------------------------------------------------
# for easier ggplot usage    
ecl_sum_to_long <- function(df){
    dfl <- data.frame(
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
    pat <- "^(\\w+)\\.(\\w+)$";
    kw.wgn <- colnames(df)[grep(pat, colnames(df), perl=TRUE)]
    kw <- sub(pat, "\\1", kw.wgn, perl=TRUE)
    wgn <- sub(pat, "\\2", kw.wgn, perl=TRUE)
    ndays <- length(df$DAYS)
    for(i in 1:length(wgn)){
        if(all(df[,3+i]==0,na.rm = TRUE)){next}
        tdfl <- data.frame(
            BASE=cname2base_var(df[,1])[,"BASE"],
            VARIANT=cname2base_var(df[,1])[,"VARIANT"],
            CASENAME=df[,1],
            DAYS=df[,2],
            DATE=df[,3],
            WGNAME=rep(wgn[i],ndays),
            KEYWORD=rep(kw[i],ndays), 
            VALUE=df[,3+i],
            UNITS=rep(kw2units(kw[i]),ndays),
            COMMENT="",
            stringsAsFactors=FALSE)
        dfl <- rbind(dfl,tdfl)
    }
    dfl <- dfl[!is.na(dfl$VALUE),]
    return(dfl)
}

#------------------------------------------------------------------------------
find_decks <- function(indir=".", ext=c(".data", ".DATA")){
    decks <- character()
    for(pat in ext){
        deck <- normalizePath(list.files(path=indir,
                                         pattern=paste0("^.+",pat,"$"),
                                         full.names = TRUE,
                                         recursive=TRUE,
                                         include.dirs=TRUE))
        decks <- c(decks, deck)
    }
    return(decks)
}

#------------------------------------------------------------------------------
find_deckdirs <- function(decks){
    deckdirs <- gsub(pattern="[.][^.]*$",
                  replacement="",
                  x=decks,
                  perl=TRUE)
    return(deckdirs)
}

#------------------------------------------------------------------------------
find_ecl_sum <- function(basedir="."){
    decks <-find_decks(basedir, ext=c(".unsmry", ".UNSMRY"))
    sumdirs <- gsub(pattern="[.][^.]*$",
                  replacement="",
                  x=decks,
                  perl=TRUE)
    return(sumdirs)
}

#------------------------------------------------------------------------------
# This function assumes that the casename is made up of a BASE separated
# from a VARIANT by an "splt".  The BASE may have an internal "splt",
# but the VARIANT may not.
cname2base_var <- function(casename, splt = "_"){
    ncn <- length(casename)
    df <- data.frame(
        BASE=character(),
        VARIANT=character(),
        stringsAsFactors=FALSE)
    for(i in 1:ncn){
        nc <- nchar(casename[i])
        found <- as.vector(gregexpr(splt, casename[i], perl=TRUE)[[1]])
        nf <- length(found)
        if(found[1] < 0){
            temp <- data.frame(
                BASE = casename[i],
                VARIANT = "_",
                stringsAsFactors=FALSE)
            df <- rbind(df, temp)
        }else{
            nsplit <- found[nf]
            temp <- data.frame(
                BASE = substr(casename[i], 1, nsplit - 1),
                VARIANT = substr(casename[i], nsplit + 1, nc),
                stringsAsFactors=FALSE)
            df <- rbind(df, temp)
        }
    }
    return(df)
}

kw2units <- function(keyword){
    units <- switch(keyword,
                    "WOPR" = "STBD",      # Oil Prod Rate
                    "WWPR" = "STBD",      # Water Prod Rate
                    "WGPR" = "MSCFD",     # Gas Prod Rate
                    "WOPT" = "MSTB",      # Cum Oil Prod
                    "WWPT" = "MSTB",      # Cum Water Prod
                    "WGPT" = "MMSCF",     # Cum Gas Prod
                    "WGOR" = "SCF/STB",   # Gas Oil Ratio
                    "WWOR" = "STB/STB",   # Water Oil Ratio
                    "WOIR" = "STBD",      # Oil Inj Rate
                    "WWIR" = "STBD",      # Water Inj Rate
                    "WGIR" = "MSCFD",     # Gas Inj Rate
                    "WOIT" = "MSTB",      # Cum  Oil Inj
                    "WWIT" = "MSTB",      # Cum  Water Inj
                    "WGIT" = "MMSCF",     # Cum  Gas Inj
                    "WBHP" = "PSIA",      # Bottom Hole Pressure
                    "WBDP" = "PSIA"       # well bore pressure drop
    )
    return(units)
}

#------------------------------------------------------------------------------
# Below this we are using the functions created above

path <- getwd()
decks <- find_decks(path)
sumdirs <- find_ecl_sum(path)
deckdirs <- find_deckdirs(decks)
# to create an error for testing
# cases <- c("xxxxx", cases)
# cases

# valid case
# case <- "SPE1CASE1" 
# missing case for testing
# case <- "SPE1CASE11" 
# dfc <- ecl_sum(case)
# dfc

df <- ecl_sum_cases(sumdirs)
df <- add_gor(df)
df <- add_wor(df)
# summary(df)

dfl <- ecl_sum_to_long(df)
# summary(dfl)

wpr_filter <- grepl("W..R",dfl$KEYWORD,perl=TRUE) & dfl$WGNAME=="PROD"
wpt_filter <- grepl("W.PT",dfl$KEYWORD,perl=TRUE)

or_filter <- dfl$KEYWORD=="WOPR" & dfl$WGNAME=="PROD"

library(ggplot2)
ggp <- ggplot(dfl[or_filter,],aes(x=DATE,y=VALUE, color=CASENAME))
ggp <- ggp + geom_line()
ggp <- ggp + facet_grid(CASENAME~WGNAME,scales="free_y")
ggp

ggp1 <- ggplot(dfl[wpr_filter,],aes(x=DATE,y=VALUE, color=CASENAME))
ggp1 <- ggp1 + geom_line()
ggp1 <- ggp1 + facet_grid(KEYWORD~.,scales="free_y")
ggp1

