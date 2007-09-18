
rpygeo.build.env = function( 
    modules = "arcgisscripting",
    init = "gp = arcgisscripting.create()",
    workspace = NULL, 
    cellsize = NULL,
    extent = NULL,
    mask = NULL,
    overwriteoutput = 0,
    extensions = NULL,
    python.path = "C:\\software\\Python24",
    python.command = "python.exe" )
{
    return( list(
        modules = modules,
        init = init,
        workspace = workspace,
        cellsize = cellsize,
        extent = extent,
        mask = mask,
        overwriteoutput = overwriteoutput,
        extensions = extensions,
        python.path = python.path,
        python.command = python.command
    ) )
}

#rpygeo.env = rpygeo.build.env()

rpygeo.env = list(
    modules = "arcgisscripting",
    init = "gp = arcgisscripting.create()",
    workspace = NULL, 
    cellsize = NULL,
    extent = NULL,
    mask = NULL,
    overwriteoutput = 0,
    extensions = NULL,
    python.path = "C:\\software\\Python24",
    python.command = "python.exe" )
    

rpygeo.required.extensions = function(expr) {
    # See ArcGIS help on the CheckOutExtension method:
    rpygeo.match.extensions = c("sa","3d","stats","na","di")
    names(rpygeo.match.extensions) = c("Spatial","3d","geostats","network","datainteroperability")
    ext = c()
    for (s in expr) {
        sub.s = strsplit(s,"(",fixed=T)[[1]]
        for (t in sub.s) {
            t = gsub(" ","",t)
            if (substring(t,nchar(t)) == ")")  next
            for (i in 1:length(rpygeo.match.extensions)) { 
                the.match = paste("_",tolower(rpygeo.match.extensions[i]),sep="")
                if ( tolower(substring(t,nchar(t)+1-nchar(the.match))) == the.match )
                    ext = c( ext, names(rpygeo.match.extensions)[i] )
            }
        }
    }
    return(unique(ext))
}


rpygeo.geoprocessor = function(
        fun, args=NULL,
        py.file="rpygeo.py", msg.file="rpygeo.msg",
        env = rpygeo.env, extensions = NULL, working.directory = getwd(),
        quote.args = TRUE, add.gp = TRUE, wait = TRUE,
        clean.up = wait,
        detect.required.extensions = TRUE )
{
    if (is.logical(clean.up)) {
        if (clean.up) {
            clean.up = c("py","msg")
        } else
            clean.up = c()
    }

    # Convert to character string,
    # and add quotation marks if input was already a string:
    convert = function(x) {
        if (is.numeric(x)) {
            return( as.character(x) )
        } else
            return( paste('"', x, '"', sep="" ) )
    }
    # Consistent indentation is important in Python:
    indent = "    "

    # Expecting a list of arguments, not a vector,
    # because arguments may have different data types:
    if (is.vector(args)) args = as.list(args)
    if ((length(fun) > 1)  & (!is.null(args))) {
        warning("Multiple function calls only allowed if args is NULL. Using only first `fun' element.\n")
        fun = fun[1]
    }
    if (!is.null(args)) if (length(quote.args)==1) quote.args = rep(quote.args,length(args))

    # Create list of required ArcGIS extensions:
    extensions = c( env$extensions, extensions )
    if (detect.required.extensions)
        extensions = c( extensions, rpygeo.required.extensions(fun) )
    extensions = unique( extensions )

    # Have to distinguish between an R version and a Windows version of file names.
    to.windows.filename = function(x) gsub("/","\\",x,fixed=T)
    to.R.filename       = function(x) gsub("\\","/",x,fixed=T)
    py.file = to.windows.filename( paste(working.directory,"/",py.file,sep="") )
    R.py.file = to.R.filename(py.file)
    msg.file = to.windows.filename( paste(working.directory,"/",msg.file,sep="") )
    R.msg.file = to.R.filename(msg.file)

    #---------------------------------------
    # Build the Python geoprocessing script:
    expr = ""
    for (mod in env$modules)
        expr = paste( expr, "import ", mod, "\n", sep="" )
    for (init in env$init)
        expr = paste( expr, init, "\n", sep="" )
    if (!is.null(env$workspace))
        expr = paste( expr, 'gp.Workspace = "', to.R.filename(env$workspace), '"\n', sep="" )
    if (!is.null(env$cellsize))
        expr = paste( expr, "gp.Cellsize = ", convert(env$cellsize), "\n", sep="" )
    if (!is.null(env$extent))
        expr = paste( expr, 'gp.Extent = "', convert(env$extent), '"', "\n", sep="" )
    if (!is.null(env$mask))
        expr = paste( expr, 'gp.Mask = "', convert(env$mask), '"', "\n", sep="" )
    if (!is.null(env$overwriteoutput))
        expr = paste( expr, "gp.Overwriteoutput = ", convert(env$overwriteoutput), "\n", sep="" )
    if (!is.null(extensions))
        for (ext in extensions)
            expr = paste( expr, 'gp.CheckOutExtension("', ext, '")\n', sep="" )
    expr = paste( expr, 'rpygeoresult = ""', "\n", sep="" )
    expr = paste( expr, "\n", sep="" )
    
    expr = paste( expr, "try:\n", sep="" )
    
    if (is.null(args)) {
        # Simplest case - each element of `fun' is a complete Python expression:
        for (the.fun in fun)
            expr = paste( expr, indent, ifelse(add.gp,"gp.",""), the.fun, "\n", sep="" )
    } else {
        # More complicated:
        # Only one `fun' call, but many arguments need to be concatenated:
        expr = paste( expr, indent, ifelse(add.gp,"gp.",""), fun, "( ", sep="" )
        for (i.arg in 1:length(args)) {
            if (i.arg > 1)  expr = paste( expr, ", ", sep="" )
            # Character string arguments will usually have to be decorated with quotes,
            # assuming that they are string constants, not variable names or expressions:
            expr = paste( expr, 
                ifelse(quote.args[i.arg], convert(args[[i.arg]]), as.character(args[[i.arg]])), 
                sep="" )
            # to do: use intelligent line breaks in expressions??
        }
        expr = paste( expr, " )\n", sep="" )
    }

    # Catch exceptions: get error messages
    expr = paste( expr, "except:\n", sep="" )
    expr = paste( expr, indent, "rpygeoresult = gp.GetMessages()\n", sep="" )
    
    #expr = paste( expr, "\n", sep="")

    # If an error occurred, write the error message to the `msg.file':
    expr = paste( expr, 'if rpygeoresult != "":\n', sep="")
    expr = paste( expr, indent, 'f = open("', R.msg.file, '", "w")\n', sep="")
    expr = paste( expr, indent, "f.write(rpygeoresult)\n", sep="")
    expr = paste( expr, indent, "f.close()\n", sep="")
    #-----------------------------------------

    
    # Write the Python geoprocessing script to the `py.file':
    py.script = file( R.py.file, open="wt" )
    write(expr, file=py.script)
    close(py.script)
    rm(py.script)

    # Delete message file;
    # otherwise msg file would only be overwritten if an geoprocessing error occurred.
    if (file.exists(R.msg.file))
        unlink(R.msg.file)

    # Set up the system call expression:    
    py.call = ""
    if (!is.null(env$python.path))
        py.call = paste( py.call, env$python.path, "\\", sep="" )
    py.call = paste( py.call, env$python.command, " ", py.file, sep="" )
    py.call = to.windows.filename(py.call)

    ######## Run Python:
    system(py.call, invisible = TRUE, minimize = TRUE, wait = wait)

    # Read error messages from the `msg.file', if available:
    res = NULL
    if (file.exists(R.msg.file) & wait) {
        f.msg = file(R.msg.file,"rt")
        res = readLines(f.msg, warn=F)
        close(f.msg)
        if ("msg" %in% clean.up)
            unlink(R.msg.file)
    }

    # Delete the `py.file' script:
    if ("py" %in% clean.up)
        if (file.exists(R.py.file))
            unlink(R.py.file)

    # Return error message or NULL:
    return( res )
}


# Re-building some of the ArcGIS Geoprocessor functions in R:

rpygeo.Hillshade.sa = function( in.raster, out.raster,
    azimuth=315, altitude=45, model.shadows=c("NO_SHADOWS","SHADOWS"), z.factor=1, ...)
{
    rpygeo.geoprocessor( fun="Hillshade_sa",
        args=list(in.raster,out.raster,azimuth,altitude,model.shadows[1],z.factor),
        quote.args=c(T,T,F,F,T,F), ... )
}

rpygeo.Slope.sa = function( in.raster, out.raster,
    unit=c("DEGREE","PERCENT_RISE"), z.factor=1, ... )
{
    rpygeo.geoprocessor( fun="Slope_sa",
        args=list(in.raster,out.raster,unit[1],z.factor),
        quote.args=c(T,T,T,F), ... )
}

rpygeo.Aspect.sa = function( in.raster, out.raster, ... )
{
    rpygeo.geoprocessor( fun="Aspect_sa",
        args=list(in.raster,out.raster), quote.args=c(T,T), ... )
}

rpygeo.EucDistance.sa = function( in.data, out.raster,
    maxdist=NULL, cellsize=NULL, out.direction.raster=NULL, 
    env = rpygeo.env, ... )
{
    if (!is.null(maxdist)) if (maxdist==Inf) maxdist = NULL
    if (!is.null(out.direction.raster) & is.null(cellsize)) cellsize = env$cellsize
    if (!is.null(cellsize) & is.null(maxdist)) maxdist = 10000000
    args = list(in.data,out.raster)
    args = c(args,maxdist,cellsize,out.direction.raster)
    rpygeo.geoprocessor( fun="EucDistance_sa",
        args=args,
        quote.args=c(T,T,F,F,T)[1:length(args)], ... )
}

rpygeo.Delete.management = function( in.data, data.type=NULL, ... )
{
    rpygeo.geoprocessor( fun="Delete_management",
        args=c(in.data,data.type), quote.args=c(T,T), ... )
}
