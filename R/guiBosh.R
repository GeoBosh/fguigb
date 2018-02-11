# Bosh: modification  of functions gui() and guiExec from  package fgui.
#
# 2013-10-19 otkomentiam 'require()', ima go na nyakolko mesta po-dolu.
# require(fgui)

# krapka, ponezhe parseHelp of fgui ne e update-vana za novata Rd-sistema.
parseHelp <- function(func){
    # return(list(file=paste("class(func) is ",class(func),sep="")
    #             , header=paste("func is ", func , sep="")
    #             ,sep="tralala"))

    if(is.character(func)){
        f <- func
    }else{
        f <- deparse(substitute(func))  # dava li imeto na fuktsiyata?
    }
    # print(f)
    Rdo_args2txt_list(f)
}

# 2013-10-20 Remove since it does not work reliably.
#
# # ponezhe tekustata versiya na fguiu ne e oobnovena za novara Help sistema.
# psi_gui <- function(func,helps=NULL,...){   # razreshavay samo character name za func.
#     func_args <- Rdo_args2txt_list(func)
#     gui(func,helps=func_args,...)
# }



## *** EXPORT ***
## Aside: mention to the user the power of getFromNamespace
##
## func - the function that should be called upon execution
##
## argType - list (unspecified is auto-detected)
##  (unspecified): auto-detect
##       - if in options, assumes is an option box
##       - if in selections, assumes is a selection box
##       - if an integer,
##  't': text entry
##  's': slider
##  'f': input for filenames
##  'o': options box (options are put in argOption)
##  'l': list box (lists are put in argList, which is 'set', and can be modified by user)
##  'c': command button
##  'm': multi-line text entry
##  'i': _ignore_ -- really not necessary anymore...
## argOption    - list of options vectors (names should be the same as args)
## argList      - list of strings for lists (names should be the same as args)
## argSlider    - list of slider ranges (names should be the same as args)
## argCommand   - list of functions to execute on command
## argEdit      - list of (width,height) both optional, NULL/NA/missing for default
## argFilter    - list of file filters (empty for all files)
##
## exec - name of string to use when user should press button to have them
##         call your function
##      - empty indicates it should not be drawn
##
## callback - name of function to handle callbacks, takes one parameter,
##            which is a string for the arg that was updated
##
## output - one of the above, 't', 's', 'm', or NULL; will try
##           to auto-choose this as well. If not 'm', then
##           an initial value will be set by running
##           the default parameters
##
## helps - 'auto' indicates it will try to load in the help from the package help,
##          if possible
##       - otherwise this can be a list of strings for help
## helpsFunc - optional name of the function (string) of where to take the help info from
##
## grid - whether to grid the objects or not (otherwise, just let the user do it)
## modal - lock input away from R
## nameFix - boolean, tries to fix names (replaces '_' & '.' with ' ').
##
## clearEnv - TRUE for the most part
##          - FALSE added, for the idea of nesting GUI objects!
guiBosh <- function( func,
                 argOption=NULL, argFilename=NULL, argList=NULL, argSlider=NULL, argCommand=NULL, argEdit=NULL, argFilter=NULL,
                 argText=NULL, argType=NULL,
                 argGridOrder=1:length(formals(func)),
                  argGridSticky=rep("a",length(formals(func))),
                  argGridFrame=rep("f",length(formals(func))),
                 title=NULL,
                 exec="OK",
                 closeOnExec=is.null(output), cancelButton=TRUE,
                 callback=NULL,
                 output='m',
                 helps='auto',
                 helpsFunc=NULL,   # bosh: different default from gui(), krapka
                 grid=TRUE, modal=NULL, nameFix=TRUE, getFix=TRUE,
                 verbose=FALSE ) {
  # require( tcltk )

  if( verbose ) {
    print( "argGrid..." )
    print( argGridOrder )
    print( argGridSticky )
    print( argGridFrame )
  }
  #stop()

  ## Store the getFix (05/09/2008)
  guiSet( "GUIINTERNALS_getFix", getFix )

  ## 01/23/2009
  modalDefault <- FALSE
  if( is.null(modal) ) {
    modalDefault <- TRUE ## special message to developer
    modal <- TRUE ## and set the default to be true
  }

  ## Allow nesting
  if( modal )
    n <- fgui:::nest() ## allow nesting

  ## parses the passed in function to widgets!
  farg <- formals( func )
  farg <- farg[ names(farg) != "..." ] ## (05/09/2008)
  fargNames <- names(farg)
  fargTypes <- rep('t',length(farg))

  #print( "farg" )
  #print( farg )
  #print( farg[[1]] )

  ## Fix up farg a little more... infernal boxplot.default... (05/09/2008)
  for( i in 1:length(farg) ) {
    #if( !is.element( class(farg[[i]]), c("numeric","logical","character") ) || is.na(farg[[i]] ) )
    #  farg[[i]] <- ""
    if( !is.element( class(farg[[i]]), c("numeric","logical","character","NULL") ) )
      farg[[i]] <- ""

    ## 01.23.2009
    #cat( str(farg[[i]]), "before\n" )
    ####try(  if( is.nan( farg[[i]] ) ) farg[[i]] <- "NaN",  silent=TRUE  ) ## A little confused between NA and NaN??
    ####try(  if( is.na( farg[[i]] ) ) farg[[i]] <- "NA",  silent=TRUE  )
    tryCatch(  if( is.nan( farg[[i]] ) ) farg[[i]] <- "NaN",  warning=function(e){},  error=function(e){}  ) ## A little confused between NA and NaN??
    tryCatch(  if( is.na( farg[[i]] ) ) farg[[i]] <- "NA",  warning=function(e){}, error=function(e){}  )
    #try(  if( is.null( farg[[i]] ) ) farg[[i]] <- "NULL" )  ## Not Necessary?
    #cat( str(farg[[i]]), "after\n" )
  }

  ## store in the text (in case user wants a different name)
  fargText <- fargNames
  textNames <- names(argText)
  for( i in 1:length(fargText) ) {
    if( is.element(fargText[i],textNames) ) {
      fargText[i] <- fgui:::elt(argText,fargText[i])
    }else if( nameFix ) {
      fargText[i] <- fgui:::fixName( fargText[i] )
    }
  }
  if( verbose ) {
    cat( "Arg text:\n" )
    print( fargText )
  }

  ## Get the function name
  call <- match.call(expand.dots=FALSE)
  funcName <- call[[match("func",names(call))]]
  funcName <- as.character( as.expression( funcName ) )
  if( is.null(title) ) title <- funcName

  ## Try to get help on that function
  if( is.character(helps) && helps=='auto' ) {
    helps <- NULL
                          # ??? !!! Bosh: sega parseHelp izpolzva funktsiyata ot moya package!
    if( !is.null(helpsFunc) ) {
      # try( helps <- fgui:::parseHelp( helpsFunc ), silent=TRUE )
      try( helps <- parseHelp( helpsFunc ), silent=TRUE )
    }else{
      # try( helps <- fgui:::parseHelp( helpsFunc ), silent=TRUE )
      try( helps <- parseHelp( funcName ), silent=TRUE )
    }
    if( verbose ) {
      cat( "Help parsing:\n" )
      print( helps )
    }
  }
  if( verbose ) cat( "Finished help parsing...\n" )

  ## prereqs
  ## - need to run a few things - e.g. do the lengths of argType and function match?

  ## Clear the environment (doesn't really do anything right now)
  fgui:::clearEnvironment()

  ## Set the lists (can be modified by user later...)
  guiSet( "argList", argList )
  ##KILL##, guiGet( "argList" )
  argListSelected <- list()
  guiSet( "argListSelected", argListSelected )

  guiSet( "func", func )

  if( !is.function(callback) )
    callback <- function(str){}; ## do nothing...
  ##guiSet( "callback", callback ) ## not needed?

  ############################
  ## auto-detect the types! ##
  ############################

  ## - get lists of names to search
  typeList <- names(argType)
  optionList <- names(argOption)
  filenameList <- names(argFilename)
  listList <- names(argList)
  sliderList <- names(argSlider)
  commandList <- names(argCommand)
  editList <- names(argEdit)
  filterList <- names(argFilter)

  ## - do the search
  gv <- function( i, default="" ) {
    if( i > length(farg) ) return( default )

    #print( farg[[i]] )
    if( class(farg[[i]]) != "name" ) {
      if( !is.null(farg[[i]]) && !is.na(farg[[i]]) )
        return( farg[[i]] )
    }
    return( default )
  }
  for( i in 1:length(farg) ) {
    if( verbose ) cat( "Determining widget type ", i, "\n" )
    curArg <- fargNames[i]

    if( is.element( curArg, typeList ) ) {
      ## The user specified a type
      fargTypes[i] <- fgui:::elt(argType,curArg)
    }else if( is.element( curArg, optionList ) ) {
      fargTypes[i] <- 'o'
    }else if( is.element( curArg, sliderList ) ) {
      fargTypes[i] <- 's'
    }else if( is.element( curArg, listList ) ) {
      fargTypes[i] <- 'l'
    }else if( is.element( curArg, filenameList ) || is.element( curArg, filterList ) ) {
      fargTypes[i] <- 'f'
    }else if( is.element( curArg, commandList ) ) {
      fargTypes[i] <- 'c'
    }else if( is.element( curArg, editList ) ) {
      fargTypes[i] <- 'm'
    }else{
      fargTypes[i] <- 't'
    }
  }
  if( verbose ) {
    cat( "Widget types determined...\n" )
    print( fargTypes )
  }

  ## Add a command button on the end if exec isn't empty
  okButtonGridOrder <- NULL
  if( !is.null(exec) && nchar(exec)>0 ) {
    if( verbose ) cat( "Adding command button...\n" )

    pos <- length(fargTypes)+1
    fargTypes[pos] <- 'c'
    fargNames[pos] <- exec
    fargText[pos] <- exec ## "Execute ..."  (05/09/2008)
    if( is.null(argCommand) ) argCommand <- list()
    pos2 <- length(argCommand)+1
    argCommand[[pos2]] <- guiExecBosh
    if( closeOnExec ) { ## 01.27.2009
      argCommand[[pos2]] <- function() {
        res <- guiExecBosh()
        tkdestroy(guiGet("MAIN"))
        guiSet( "FGUI_INTERNAL_last_result", res ) ## 02.02.2009
        return(res)
      }
    }
    names(argCommand)[pos2] <- exec

    commandList <- names(argCommand)
    argGridOrder[pos] <- max(argGridOrder)+1
    argGridSticky[pos] <- "ns"
    argGridFrame[pos] <- "f"
    okButtonGridOrder <- argGridOrder[pos]
  }
  if( verbose ) cat( "Determined output format 1...\n" )

  ## New, add a cancel button
  guiSet( "GUIINTERNALS_cancelled", FALSE )
  if( cancelButton ) {
    if( verbose ) cat( "Adding cancel button...\n" )

    pos <- length(fargTypes)+1
    fargTypes[pos] <- 'c'
    fargNames[pos] <- "cancel"
    fargText[pos] <- "Cancel"
    if( is.null(argCommand) ) argCommand <- list()
    pos2 <- length(argCommand)+1
    argCommand[[pos2]] <-
      function() {
        cat( "Cancelled.\n" )
        guiSet( "GUIINTERNALS_cancelled", TRUE )
        tkdestroy(main)
      }
    names(argCommand)[pos2] <- "cancel"
    commandList <- names(argCommand)
    if( !is.null(okButtonGridOrder) ) {
      argGridOrder[pos] <- okButtonGridOrder  ## Right next to the OK button
      argGridSticky[pos] <- "ns"
      argGridFrame[pos] <- "f"
    }else{
      argGridOrder[pos] <- max(argGridOrder)+1
      argGridSticky[pos] <- "ns"
      argGridFrame[pos] <- "f"
    }
  }

  ## add in a status box
  #if( statusBox && (output!='m'&&output!='t') ) {
  #  pos <- length(fargTypes)+1
  #  fargTypes[pos] <- 't'
  #  fargNames[pos] <- 'status'
  #  if( is.null(argEdit) ) argEdit <- list()
  #  pos2 <- length(argEdit)+1
  #  argEdit[[pos2]] <- NA
  #  names(argEdit)[pos2] <- 'status'
  #
  #  editList <- names(argEdit)
  #  argGridOrder[pos] <- max(argGridOrder)+1
  #}

  ## add in the output
  ## - first auto-choose if possible
  curArg <- "output"
  if( is.element( curArg, sliderList ) ) {
    output <- 's'
  }else if( is.element( curArg, editList ) ) {
    output <- 'm'
  }
  if( verbose ) cat( "Determined output format 2...\n" )
  #else{
  #  fargTypes[i] <- 't'
  #}
  ## - then add it into the lists
  if( !is.null(output) && (output=='m' || output=='t' || output=='s') ) {
    pos <- length(fargTypes)+1
    fargNames[pos] <- 'output'
    fargText[pos] <- "Output:"
    if( is.element('output',textNames) )
      fargText[pos] <- fgui:::elt(argText,'output')
    if( output=='m' ) {
      fargTypes[pos] <- 'm'
      if( is.null(argEdit) ) argEdit <- list()
      pos2 <- length(argEdit)+1
      argEdit[[pos2]] <- NA
      names(argEdit)[pos2] <- 'output'

      editList <- names(argEdit)
    }else if( output=='s' ) {
      fargTypes[pos] <- 's'
      if( is.null(argSlider) ) argSlider <- list()
      pos2 <- length(argSlider)+1
      argSlider[[pos2]] <- NA
      names(argSlider[[pos2]]) <- 'output'

      sliderList <- names(argSlider)
    }else if( output=='t' ) {
      fargTypes[pos] <- 't'
    }
    argGridOrder[pos] <- max(argGridOrder)+1
    argGridSticky[pos] <- "news"
    argGridFrame[pos] <- "g"
  }
  if( verbose ) cat( "Output added to lists...\n" )

  ##print( argCommand )

  ########################
  ## create the widgets ##
  ########################

  ## Create the main window
  # require( tcltk )
  main <- tktoplevel()
  tkwm.title( main, title )
  if( verbose ) cat( "Created main window, proceeding to create all widgets...\n" )

  guiSet( "MAIN", main ) ## for cursor

  ## Create all the widgets
  ##for( i in 1:length(fargNames) ) {
  object <- list()
  guiObject <- list()
  i <- 1
  for( ago in unique(argGridOrder) ) {
    #print( "ago" )
    #print( ago )
    #print( "argGridOrder" )
    #print( argGridOrder )

    agos <- which( argGridOrder == ago )

    sframe <- main
    #print( "argGridFrame" )
    #print( argGridFrame )
    #print( "agos" )
    #print( agos )
    if( argGridFrame[agos][1] == "f" ) {
      sframe <- guiFrame( main, borderwidth=0 )
      tkgrid( sframe )
      ##sticky <- "news" ## codeTools -- 02/11/2009
      if( argGridSticky[agos][1] != 'a' ) {
        tkgrid.configure( sframe, sticky=argGridSticky[agos][1] )
      }else if( fargTypes[agos][1] == 't' || fargTypes[agos][1] == 'f' ) {
        tkgrid.configure( sframe, sticky="nes" )
      }else{
        tkgrid.configure( sframe, sticky="nws" )
      }
      #if( fargTypes[agos][1] == 't' ) {
      #  if( argGridSticky[agos][1] == 'a' ) {
      #    tkgrid.configure( sframe, sticky="nes" ) ## default placement hack
      #  }else{
      #    tkgrid.configure( sframe, sticky=argGridSticky[agos][1] )
      #  }
      #}else{
      #  ## default (essentially nws, really)
      #  tkgrid.configure( sframe, sticky="news" )
      #}
    }

    ## Create an object for each
    for( a in agos ) {
      if( verbose ) cat( "Creating widget ", i, "\n" )
      helpsi <- fgui:::elt(helps,fargNames[i],die=FALSE)
      res <- NULL

      if( fargTypes[i] == 't' ) {
        ## Text entry
        #var <-  tclVar(gv(i))
        #res <- guiTextEntry( var, fargText[i], main, helps=helpsi )
        res <- guiTextEntry( sframe=sframe, text=fargText[i], default=gv(i), helps=helpsi )
        if( argGridSticky[i] == 'a' ) {
          argGridSticky[i] <- "nes"
          ##print( "WTF?" )
        }
      }else if( fargTypes[i] == 's' ) {
        ## Slider
        range <- fgui:::elt( argSlider, fargNames[i] )
        if( length(range) < 2 )
          stop( "Slider ranges must be a vector of two numeric values, with an optional 3rd argument stepsize." )
        if( length(range) == 2 )
          range[3] <- (range[2]-range[1])/100
        value <- gv( i, range[1] )
        res <- guiSlider( text=fargText[i], default=value, min=range[1], max=range[2], step=range[3], sframe=sframe, update=fgui:::guiExecUpdateFunc(fargNames[i],callback), helps=helpsi )
        if( argGridSticky[i] == 'a' )
          argGridSticky[i] <- "nws"
      }else if( fargTypes[i] == 'f' ) {
        ## Input filenames
        value <- gv( i )
        #callback <- NULL
        filter <- fgui:::elt( argFilter, fargNames[i], die=FALSE )
        if( is.null(filter) ) filter <- "{{All files} {.*}}"
        if( argGridSticky[i] == 'a' )
          argGridSticky[i] <- "nws"
        res <- guiFilename( sframe, text=fargText[i], default=value, filter=filter, callback=fgui:::guiExecUpdateFunc(fargNames[i],callback), helps=helpsi )
      }else if( fargTypes[i] == 'o' ) {
        ## options box
        res <- guiOption( sframe, fargText[i], fgui:::elt(argOption,fargNames[i]), update=fgui:::guiExecUpdateFunc(fargNames[i],callback), helps=helpsi )
        if( argGridSticky[i] == 'a' )
          argGridSticky[i] <- "nws"
      }else if( fargTypes[i] == 'l' ) {
        ## list box
        res <- guiList( sframe, fargText[i], fargNames[i], update=fgui:::guiExecUpdateFunc(fargNames[i],callback), helps=helpsi )
        if( argGridSticky[i] == 'a' )
          argGridSticky[i] <- "nws"
      }else if( fargTypes[i] == 'c' ) {
        ## command button
        res <- list()
        res$object <- "no object"
        #res$guiObject <- tkbutton( main, text=fargNames[i], command=fgui:::elt(argCommand,fargNames[i]) )
        res$guiObject <- tkbutton( sframe, text=fargText[i], command=fgui:::guiExecUpdateFunc(fargNames[i],callback,fgui:::elt(argCommand,fargNames[i],die=FALSE)) )
        if( argGridSticky[i] == 'a' )
          argGridSticky[i] <- "nws"
      }else if( fargTypes[i] == 'm' ) {
        ## multi-line edit
        values <- fgui:::elt(argEdit,fargNames[i])
        #print( values )
        width <- guiGet("EDIT_WIDTH")
        height <- guiGet("EDIT_HEIGHT")
        readonly <- TRUE
        if( length(values) > 0 && !is.na(values[1]) ) width <- values[1]
        if( length(values) > 1 && !is.na(values[2]) ) height <- values[2]
        if( length(values) > 2 && !is.na(values[3]) ) readonly <- (values[3]==TRUE)
        res <- guiEdit( sframe, fargText[i], gv(i), width, height, readonly, helps=helpsi )
        if( argGridSticky[i] == 'a' )
          argGridSticky[i] <- "news"
      }else if( fargTypes[i] == 'i' ) {
        res <- list(object=NULL,guiObject=NULL)
      }

      #i <- length(object) + 1
      object[[i]] <- res$object
      guiObject[[i]] <- res$guiObject
      i <- i + 1
    }## a

    ## Now grid the objects
    ##argGridSticky[agos] <- "nws"
    if( verbose ) cat( "Gridding widgets ", paste(agos,collapse="-"), "\n", sep="" )
    ##guiGrid( guiObject, sticky=argGridSticky[agos] )
    ###guiGrid( subset( guiObject, argGridOrder==ago ), sticky="nws" )
    fgui:::guiGrid( subset( guiObject, argGridOrder==ago ), sticky=argGridSticky[agos] )

  }## unique(argGridOrder)
  ##}

  ## Draw all the objects
  #if( grid ) {
  #  for( ago in unique(argGridOrder) ) {
  #    if( verbose ) cat( "Gridding widget",ago,"\n" )
  #    guiGrid( subset( guiObject, argGridOrder==ago ), sticky=sticky )
  #  }
  #}

  ## Set some things in the globs, so user can do it, etc.
  guiSet( "object", object )
  guiSet( "guiObject", guiObject )
  guiSet( "fargTypes", fargTypes )
  guiSet( "fargNames", fargNames )
  guiSet( "farg", farg )
  guiSet( "output", output )

  ## Update output if necessary
  if( !is.null(output) && output!='m' ) ##&& !closeOnExec ) ## Not sure if I want this last piece
    guiExecBosh()

  ## Bosh: otkomentiram the messages below since they do not appear when required.
  ##       I do not know the reason! something with buffering?

  ## lastly, should we go modal?
  if( modalDefault ) {
    ## Notify developer it can be set
    # cat( "Close '", title, "' window to allow entering commands in the R console;\n that window has gone modal. Note that the '", title, "' window may be hidden from view\n (esp. in windows), and you may have to find it in the taskbar and close it. To the developer, the modality of a window can be set.\n" )
    tkwait.window(main)
  }else if( modal ) {
    # cat( "Close '", title, "' window to allow entering commands in the R console;\n that window has gone modal.\nNote that the '", title, "' window may be hidden from view (esp. in windows),\n and you may have to find it in the taskbar and close it.\n" )
    ##cat( "Close '", title, "' window to return to R (sometimes R will steal the focus, esp. in windows).\n", sep="" )
    tkwait.window(main)
  }else{
    ## Not modal
    # cat( "The '", title, "' window has been launched.\n Note that the '", title, "' window may be hidden from view (esp. in windows)\n and you may need to click on it in the taskbar.\n" )
  }

  ## If cancelled, don't return a value
  if( guiGet( "GUIINTERNALS_cancelled" ) == TRUE )
    return( NULL )

  ## And return all the values...
  allValues <- guiGetAllValues()
  if( modal ) fgui:::unnest(n) ## RECOVER FROM NESTING
  return( allValues )
}











## Handle callback to the function
guiExecBosh <- function( lastTouched=NULL ) {
  cancelled <- guiGetSafe("GUIINTERNALS_cancelled")
  if( !is.na(cancelled) && !is.null(cancelled) && cancelled==TRUE )
    return(NULL)

  #print( "callback to gui function." );

  object <- guiGet( "object" )
  ## CODETOOLS ## fargTypes <- guiGet( "fargTypes" )
  ## CODETOOLS ## fargNames <- guiGet( "fargNames" )
  ## CODETOOLS ## farg <- guiGet( "farg" )
  func <- guiGet( "func" )
  output <- guiGet( "output" )
  main <- guiGet( "MAIN" )

  ## first need to parse out all of the options...
  ##print( "about to get all the options" )
  value <- guiGetAllValues()
  #print( "got the options" )
  ## debug for now..
  #print( value )

  ## Set the cursor to business
  try( tkconfigure( main, cursor="watch" ) )

  ## RUN THE FUNCTION!
  #if( is.list(value) ) {
  #  for( i in 1:length(value) )
  #    if( !is.na(value) )
  #
  #}
  if( !is.list(value) ) {
    value2 <- list;
    for( i in 1:length(value) )
      value2[[i]] <- value[i]
    value <- value2
  }else{
    #print( "is list" )
  }
  ## Below really should work just fine
  ## But some funniness with R deciding lists
  ## Are just vectors, to piss us off
  #formals(func) <- value
  ## (05/09/2008)

  namesValue <- names(value)
  namesFormalsFunc <- names(formals(func))

  boshgetFix <- guiGet("GUIINTERNALS_getFix")
  if(length(boshgetFix) == 1)
      boshgetFix <- rep(boshgetFix, length(value))

  for( v in 1:length(value) ) {                      ## v indices value
    f <- which( namesFormalsFunc == namesValue[v] )  ## f indices formals(func)

    ##cat( "names", namesValue[[v]], namesFormalsFunc[[f]], "\n" )

    ## precaution
    if( names(value)[1] == "..." ) {
      stop("guiExecBosh: '...' should have been caught earlier!")
    }

    if( is.character(value[[v]]) && nchar(value[[v]])==0 ) {
      ## Then it's an empty character, don't modify anything
    }else{
      formals(func)[[f]] <- value[[v]]
      if( boshgetFix[v] == TRUE )   # Bosh !!!
        try( formals(func)[[f]] <- eval(parse(text=value[[v]])), silent=TRUE )
    }
  }


  res <- NULL;
  tryCatch( {res <- func()},
            error=function(e){fgui:::gui_errorMessage(e$message)} )

  ## Unset the business
  try( tkconfigure( main, cursor="arrow") )

  if( !is.null(res) ) {
    if( is.null(output) || ( output!='m' && output!='t' && output!='s' ) ) { ## 01/27/2009
      return(res) ## If no output window, then return the value
    }else if( output=='m' ) {
      fgui:::gui_tkInsertText( object[[length(object)]], as.character(res) )    ## 05/09/2008 -- as.character
      fgui:::gui_tkInsertText( object[[length(object)]], "\n" )
    }else if( output=='t' ) {
      ##gui_tkSetText( object[[length(object)]], res )
      tclvalue(object[[length(object)]]) <- as.character(res)  ## 05/09/2008 -- as.character
    }else if( output=='s' ) {
      tclvalue(object[[length(object)]]) <- as.character(res)  ## 05/09/2008
    }

    ## 05/16/2008 addition!
    winExists <- guiGetSafe( "INTERNALMENU_USED" )
    if( is.na( winExists ) || !winExists ) {
      ## oops --- nothing really! had it backwards
    }else{
      fguiWindowPrint( as.character(res) )
    }
  }
}
