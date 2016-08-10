#' stringToCM
#'
#' Conversion from a loop string to a community matrix.
#'
#' @param linkstring String of links separated by spaces and/or newlines. Each link connects two nodenames with "->" ")->" etc.
#' @return An NxN community matrix, where N=# distinct nodes.
#' @examples{
#' stringToCM(
#'    "a )-> b     b )-> c
#'        a -( a" )
#'  }

stringToCM = function(linkstring="a->b a-(a b-(a", use_o = FALSE,
                      o_rh = "\\(", o_lh = "\\)",
                      sep=" ") {
  # We need to deal with ambiguity of   xyo->z.
  # Is "o" part of the name, or the connector?
  # For now, detect and throw error.
  if(use_o)
    o_rh = o_lh = "o"
  ambiguity = (o_lh=="o" & length(grep("[^ ]o-", linkstring))>0 ) |
    (o_rh=="o" & length(grep("-o[^ ]o", linkstring))>0 )
  if(ambiguity)
    stop("Please resolve ambiguity: o in name or in connector?")
  theComment = gsub('.*#*', "", linkstring)
  linkstring = gsub('#.*', "", linkstring)   # remove comments
  linkstring = gsub("[ \n]+", sep, linkstring)
  # Replace newlines by word separator.
  # the next 2 lines remove spaces,
  #  Remove spaces inside each word.
  linkstring = gsub(
    paste0(" *([",o_lh, "<]*-[", o_rh, ">]*) *"),
    "\\1", linkstring)
  #Now, handle the ")->" and "<-(" cases:
  links  = strsplit(x = linkstring, split = "[ \n]")[[1]]
  two_way_pattern = paste0("[", o_lh, "<]-[", o_rh, ">]")
  is_two_way = grep(pattern = two_way_pattern, links)
  if(length(is_two_way) > 0) {
    # replace each two-way link element by the two links it represents.
    links = c(links, sub(paste0("[", o_lh, "<]-"), "-", links[is_two_way]))
    links = c(links, sub(paste0("-[", o_rh, ">]"), "-", links[is_two_way]))
    # Remove these link elements
    links = links[-is_two_way]
  }
  linknodes <- strsplit(links, paste0("[<", o_lh, "]*-[>", o_rh, "]*") )
  names = unique(unlist(strsplit(links, paste0("->|-", o_rh, "|<-|", o_lh, "-"))))
  n = length(names)
  cm = matrix(rep(0,n^2), nrow=n, dimnames=list(names,names))
  signs = rep(-1, length(links))
  signs[grep("->|<-", links)] = 1
  from_to = as.matrix(as.data.frame(linknodes, stringsAsFactors=FALSE))
  ## Reverse any pointing left instead of right.
  backwards = (regexpr(paste0("[", o_lh, "<]-"), links) > -1)
  if(any(backwards))
    from_to[ , backwards] = apply(from_to[ , backwards, drop=F], 2, rev)
  from = from_to[1, ]
  to = from_to[2, ]
  for(link in 1:length(links))
    cm[to[link], from[link] ] = signs[link]
  comment(cm) = theComment
  return (cm)
}


#' CMtoIPMnet
#'
#' Convert a community matrix to a string suitable for ipmnet.org/loop
#'
#' @param cm A community matrix
#' @return A string suitable for ipmnet.org/loop
#' @examples {
#' ABCDE = "A -(A A)->B B)->C C )->D D )->E"
#' CMtoIPMnet(stringToCM(ABCDE))
#' ### At ipmnet, generate the predictions. Then compare to LoopAnalyst:
#' library("LoopAnalyst")
#' LoopAnalyst::make.cem(stringToCM(ABCDE))
#' }
#'
CMtoIPMnet = function(cm) {
  N = nrow(cm)
  matstring =
    paste0(collapse=',',
           sapply(1:nrow(cm),
                  function(rownum)
                    paste0('[',
                           paste(cm[rownum, ], collapse=","), ']')
           )
    )
  namestring = paste(collapse=",", rownames(cm))
  paste0("n:=", N, ":A:=array(1..n,1..n,[",
         matstring,
         "]);[",
         namestring,
         ']'
  )
}

