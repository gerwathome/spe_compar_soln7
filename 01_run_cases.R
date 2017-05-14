# create a function to run simulations
# run a specific simulation
# run all of the simulations in sim_decks
#------------------------------------------------------------------------------
run_flow <- function(deck, outdir=".",  sim_exec="flow", restart=FALSE,
                     sim_version = "stable"){
    # sim_exec is one of c("flow", "flow_mpi", "flow_polymer",
    # "flow_sequential", "flow_solvent")
    # sim_version is one of c("stable", "latest")
    
    exec_path = "/usr/bin/"
    if(sim_version=="latest"){exec_path <- "/usr/local/bin/"}
    # which gives an old version due to my current path
    #    exec <- Sys.which(sim_exec)
    exec <- paste0(exec_path, sim_exec)
    casename <- basename(deck)
    casename <- sub("[.][^.]*$", "", casename, perl=TRUE)
    output_dir <- file.path(outdir, casename)
    dir.create(output_dir, showWarnings = FALSE)
    if(!restart){
        old <- list.files(output_dir,full.names = TRUE)
        file.remove(old)
    }
    args <- c(deck, paste0("output_dir=", output_dir))
    sout <- paste0(casename,".OUT")
    sout <- file.path(output_dir, sout)
    serr <- paste0(casename,".OUT")
    serr <- file.path(output_dir, serr)
    system2(exec, args=args, stdout=sout, stderr=serr)
}
dbg <- FALSE
if(dbg){
    # deck <- "./sim_decks/SPE7CASE1A.DATA"
    deck <- "./sim_decks/SPE7_1A.DATA"
    outdir <- "./sim_output" 
    
    err <- run_flow(deck, outdir)
    print(err)
}
#------------------------------------------------------------------------------
# with a non-default pattern, it can find other types of decks
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
run_all <- function(indir= ".", outdir=".", ext=c(".data", ".DATA"),
                    sim_exec="flow", restart=FALSE, sim_version = "stable"){
    decks <- find_decks(indir=indir, ext=ext)
    errs <- numeric()
    for(case in decks){
        err <- run_flow(case, outdir=outdir, sim_exec=sim_exec,
                        restart=restart, sim_version=sim_version)
        errs <- c(errs, err)
    }
    return(errs)
}

dbg <- FALSE
if(dbg){
   indir <- "./sim_decks"
   outdir <- "./sim_output" 
   errs <- run_all(indir=indir, outdir=outdir)
   print(errs)
}
#------------------------------------------------------------------------------
