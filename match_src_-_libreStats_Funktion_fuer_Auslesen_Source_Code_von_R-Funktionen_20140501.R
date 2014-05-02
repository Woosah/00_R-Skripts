stopper <- function(fun)
{
  stop(paste("in match_src() : function fun='", fun, "' not found", sep=""), call.=FALSE)
}
 
match_src <- function(fun, pattern, ignore.case=FALSE, perl=FALSE, value=FALSE, fixed=FALSE, useBytes=FALSE, invert=FALSE, remove.comments=TRUE)
{
  ### This is really too complicated, I apologize
  err <- try(test <- is.character(fun), silent=TRUE)
   
  if (inherits(x=err, what="try-error"))
    stopper(fun=deparse(substitute(fun)))
  else if (test)
  {
    err <- try(fun <- eval(as.symbol(fun)), silent=TRUE)
     
    if (inherits(x=err, what="try-error"))
      stopper(fun=fun)
  }
  err <- try(expr=src <- capture.output(fun), silent=TRUE)
  
  if (inherits(x=err, what="try-error"))
    stopper(fun=deparse(substitute(fun)))
   
   
  # Remove comments
  if (remove.comments) # test
  {
    src <- sub(src, pattern="#.*", replacement="")
    
    num.empty <- which(src == "")
    if (length(num.empty) > 0)
      src <- src[-num.empty]
     
    src <- sub(x=src, pattern="[ \t]+$", replacement="")
  }
  
  
  ### Get matches and scrub
  matches <- grep(x=src, pattern=pattern, ignore.case=ignore.case, perl=perl, value=value, fixed=fixed, useBytes=useBytes, invert=invert)
   
  src <- src[matches]
   
  # remove leading and trailing whitespace
  src <- sub(x=src, pattern="^[ \t]+|[ \t]+$", replacement="")
   
  return( src )
}


### Examples
match_src(match_src, pattern="comment")
#[1] "function(fun, pattern, ignore.case=FALSE, perl=FALSE, value=FALSE, fixed=FALSE, useBytes=FALSE, invert=FALSE, remove.comments=TRUE)"
#[2] "if (remove.comments)"                                                                                                               

match_src(match_src, pattern="comment", remove.comments=FALSE)
#[1] "function(fun, pattern, ignore.case=FALSE, perl=FALSE, value=FALSE, fixed=FALSE, useBytes=FALSE, invert=FALSE, remove.comments=TRUE)"
#[2] "# Remove comments"                                                                                                                  
#[3] "if (remove.comments) # test"

match_src("match_src", pattern="comment")
#[1] "function(fun, pattern, ignore.case=FALSE, perl=FALSE, value=FALSE, fixed=FALSE, useBytes=FALSE, invert=FALSE, remove.comments=TRUE)"
#[2] "if (remove.comments)"                                                                                                               

match_src(match_srcs, pattern="comment")
#Error: in match_src() : function fun='match_srcs' not found