# version 0.01

encl_in_quotes <- function(s) paste("\"",s,"\"",sep="")

# > my.gui.fun("read.table", alist(file=,header=FALSE,sep=""))

# todo: drugi tipove poleta: file_choice, list ??? !!!

my.gui.fun <- function(f, anames, fname=".f",title=f
                       , getFix
                       , posArg = 0 # first arg normally is positional
                                    # so posArg=1 seems natural, no kogato tova ne e taka
                                    # stavat trudno otkrivaemi greshki.
                                    #
                                    # posArg is a count of the positional args.
                       , encinquotes
                       , helpsFunc = f
                       , truefunname = f
                       ){#anames is created with alist()

    #print a message in the console window.
    #for some reason the message printed by the gui() function (and my modification)
    # appears only after the window is closed, so is pretty useless.
    # Also, that message is too technical for my purposes.

    cat( "Close '", title, "' window to allow entering commands in the R console.\n",
        "Note that the '", title, "' window may be hidden from view\n",
        "and you may have to find it in the taskbar and close it.\n",
        sep="" # 2013-10-22 added sep
        )

    flush.console() # hopefully will print the message now...

    if(missing(anames)){
        anames <- formals(f)
        anames <- anames[names(anames) != "..."]
    }

    posArg
    # funname <- f
    funname <- truefunname   # vizh psi.rep

    # todo:
    # tuk e chubavo da se napravi avtomatichno machane na trailing .default of funname.

    fname <- function(){
        # co <- ""
        nonposargs <- rep(TRUE,length(anames))
        names(nonposargs) <- names(anames)
        if(posArg>0)
            nonposargs[1:posArg] <- FALSE
        # res <- paste(funname, "(", sep="")

        # wrk <- alist(as.name(f))   # vector("list", length(names(formals())))
        # wrk <- alist(funname)   # vector("list", length(names(formals())))
        # wrk <- alist(f)   # vector("list", length(names(formals())))
        wrk <- list(as.name(funname))   # vector("list", length(names(formals())))
        for(s in names(formals())){
            # print(s)


            s1 <- eval(as.name(s))
            if(opttf[s])
                s1 <- as.logical(s1)

            # print(s1)
            #
            # s1 <- if(qadd[[s]]) encl_in_quotes(s1) else s1
            #
            # if(nonposargs[s]){
            #     # res <- paste(res, co, s, "=", s1, sep="")
            # }else{
            #     res <- paste(res, co,         s1, sep="")
            # }
            # co <- ", "

            # wrk <- c(wrk, s1)  #    alist(s1)  maybe:  wrk <- c(wrk, s1)  ???
            # if(nonposargs[s]){
            #     names(wrk)[length(wrk)] <- s
            # }else names(wrk)[length(wrk)] <- NULL

            # wrk <- c(wrk, alist(s1))  #    alist(s1)  maybe:  wrk <- c(wrk, s1)  ???

            wrk <- c(wrk, list(s1))  #    alist(s1)  maybe:  wrk <- c(wrk, s1)  ???
            wrk <- c(wrk, list())  #    alist(s1)  maybe:  wrk <- c(wrk, s1)  ???
            if(nonposargs[s]){
                names(wrk)[length(wrk)] <- s
            }else{
                names(wrk)[length(wrk)] <- ""
            }# else names(wrk)[length(wrk)] <- NULL

        }
        # res <- paste(res, ")", sep="")
        res <- as.call(wrk)
        res <- paste( capture.output(print(res)), collapse="\n" ) # may be more than 1 line.

        # res <- paste(res, "\n\n"
        #              ,  paste ( capture.output(print( formals() )), collapse="\n")
        #              , sep="")

        res

        # print(match.call())
        # sys.call()
        # match.call()
    }
    # formals(fname,envir=envir)  <- anames
    formals(fname)  <- anames
    fname

    opt <- list()
    opttf <- rep(FALSE,length(anames))
    names(opttf) <- names(anames)
    for(s in names(anames)){
        val <- anames[[s]] # izglezhda che e missing ako nyama default.
                           # need to check with is.logical as TRUE==1 gives TRUE.
        if( !missing(val) && !is.null(val) && is.logical(val) && val %in% c(TRUE,FALSE) ){
            opttf[s] <- TRUE
            opt[[s]] <- as.character(c(TRUE==isTRUE(val),FALSE==isTRUE(val)))
        }
    }
    # print(opt)

    qadd <- list()
    fw <- character(0)
    for(s in names(anames)){
        val <- anames[[s]] # izglezhda che e missing ako nyama default.
                           # need to check with is.logical as TRUE==1 gives TRUE.
        qadd[[s]] <- !missing(val) && !is.null(val) && is.character(val) && length(val)==1
        if(s == "file") # krapka
            qadd[[s]] <- TRUE   # file names need quotes in  command line commands.

        if(s == "file") # krapka
            fw <-  c(fw,s)      # file names need quotes in  command line commands.
        # else                  # but they better be in filename widgets.
        #     fw[[s]] <- FALSE
    }
    # print(qadd)

    fwlist <- vector(mode="list",length=length(fw))
    if(length(fw)>0)
        names(fwlist) <- fw


    fix <- as.list(rep(TRUE,length(anames)))   # getFix all by default
    names(fix) <- names(anames)
    if(!missing(getFix)){
        # print("mmmmmmmmmm")
        if(!is.null(names(getFix))){
            # print("haa!")
            fix[names(getFix)] <- getFix[names(getFix)] # change only those in getFix.
                                                     # todo: needs check for consistency here
        }else{   # if no names in getFix, copy sequentially
            # print("hoo!")
            fix[1:length(getFix)] <- getFix  # todo: check getFix no longer than fix.
        }
    }

    # print("fix is:")
    # print(fix)

    # for(i in 1:length(anames)){
    #
    # }

    lblexec <- "Show command"

                                              # arg <- guiBosh(myread2,exec=NULL,getFix=FALSE)
    if(length(fwlist)>0){
    arg <- guiBosh(fname, title = title
                   , argOption = opt  # check if  list() is ok?
                   , getFix = fix
                   , argFilename = fwlist
                   , helpsFunc = helpsFunc # "read.table"
                   , exec = lblexec
                   )
}else{
    arg <- guiBosh(fname, title = title
                   , argOption = opt  # check if  list() is ok?
                   , getFix = fix
                   # , argFilename = fwlist
                   # , helpsFunc = f # "read.table"
                   , helpsFunc = helpsFunc # "read.table"
                   , exec = lblexec
                   )
}

    cat("\nWindow '", title, "' is now closed.", "\n\n",
        sep="" # 2013-10-22 added sep
        )

    do.call(f,arg)
}
