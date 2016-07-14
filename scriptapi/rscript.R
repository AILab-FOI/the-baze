library( "rjson" )
library( methods )

baze <- setRefClass(
  "baze",
  fields = list(
    infile = "character",
    outfile = "character",
    queries = "list",
    forms = "list",
    reports = "list",
    scripts = "list"
  ),
  methods = list(
    initialize = function(infile, outfile, queries, forms, reports, scripts)
    {
      "Initialize a baze."
      infile <<- infile
      outfile <<- outfile
      queries <<- queries
      forms <<- forms
      reports <<- reports
      scripts <<- scripts
      
      for( key in names( queries ) )
      {
          queries[[ key ]]$run <<- (function(){ 
            caller = toString(match.call())
            o = strsplit(caller,"$", fixed=TRUE)[[1]][1]
            query = eval( parse( text=paste( o, "$name" ) ) )
            .self$runQuery(query)
          })
      }
      
      for( key in names( forms ) )
      {
          forms[[ key ]]$show <<- (function(){ 
            caller = toString(match.call())
            o = strsplit(caller,"$", fixed=TRUE)[[1]][1]
            form = eval( parse( text=paste( o, "$name" ) ) )
            .self$showForm(form)
          })
      }
      
      for( key in names( reports ) )
      {
          reports[[ key ]]$show <<- (function(){ 
            caller = toString(match.call())
            o = strsplit(caller,"$", fixed=TRUE)[[1]][1]
            report = eval( parse( text=paste( o, "$name" ) ) )
            .self$showReport(report)
          })
      }
      
      for( key in names( scripts ) )
      {
          scripts[[ key ]]$run <<- (function(){ 
            caller = toString(match.call())
            o = strsplit(caller,"$", fixed=TRUE)[[1]][1]
            script = eval( parse( text=paste( o, "$name" ) ) )
            .self$runScript(script)
          })
      }
    },
    getResult = function(rid)
    {
      found <- FALSE
      result <- ""
      while(TRUE)
      {
        err <- FALSE
        tryCatch({
          r <- fromJSON( paste( readLines( infile, warn=FALSE ), collapse="" ) )
        },
        error = function( condition )
        {
          err <- TRUE
        })
        if((!err) && ( rid %in% names( r ) ) )
        {
          result <- r[[rid]]
          result$result <- tryCatch({
            result$result <- lapply(result$result, function(x) {
              x[sapply(x, is.null)] <- NA
              unlist(x)
            })
            do.call("rbind", result$result)
          })
          r[[rid]] <- NULL
          outjs = toJSON( r )
          write( outjs, infile )
          break
        }
        else
        {
          Sys.sleep( 0.1 )
        }
      }
      result
    },
    sendRequest = function( typ, oid, command, params ) 
    {
      # command and params are optional
      req <- list( type=typ, oid=oid )
      if( !missing( command ) )
      {
        req[[ "command" ]] <- command
      }
      if( !missing( params ) )
      {
        req[[ "params" ]] <- params
      }
      reqjs <- toJSON( req )
      write( reqjs, outfile )
    },
    runQuery = function(query)
    {
      sendRequest( "runquery", query )
      getResult(query)
    },
    showForm = function(form)
    {
      sendRequest( "showform", form )
      getResult(form)
    },
    showReport = function(report)
    {
      sendRequest( "showreport", report )
      getResult(report)
    },
    runScript = function(script)
    {
      sendRequest( "runscript", script )
      getResult(script)
    }
  )
)

