#'stringToCM
#'
#'Conversion from a loop string to a community matrix.
#'
#' @param linkstring String of links separated by spaces and/or newlines. Each link connects two nodenames with "->" ")->" etc.
#' @return An NxN community matrix, where N=# distinct nodes.
#' @examples{
#' stringToCM(
#'    "a )-> b
#'     b )-> c
#'        a -( a")
#'  }

stringToCM = function(linkstring="a->b a-(a b-(a") {
  theComment = gsub('.*#', "", linkstring)
  linkstring = gsub('#.*', "", linkstring)   # remove comments
  linkstring = gsub("[ \n]+", " ", linkstring) # Remove newlines
  # the next 2 lines remove  spaces,
  #  allow user to put in spaces for readability.
  linkstring = gsub(" *([)<]*-[(>]*) *", "\\1", linkstring)
  #Now, handle the ")->" and "<-(" cases:
  links  = strsplit(x = linkstring, split = "[ \n]")[[1]]
  is_two_way = grep(pattern = "[)<]-[(>]", links)
  if(length(is_two_way) > 0) {
    # replace each two-way link element by the two links it represents.
    links = c(links, sub("[)<]-", "-", links[is_two_way]))
    links = c(links, sub("-[(>]", "-", links[is_two_way]))
    # Remove these link elements
    links = links[-is_two_way]
  }
  linknodes <- strsplit(links, "[<)]*-[>()]*")
  names = unique(unlist(strsplit(links, "->|-(|<-|)-")))
  n = length(names)
  cm = matrix(rep(0,n^2), nrow=n, dimnames=list(names,names))
  signs = rep(-1, length(links))
  signs[grep("->|<-", links)] = 1
  from_to = as.matrix(as.data.frame(linknodes, stringsAsFactors=FALSE))
  ## Reverse any pointing left instead of right.
  backwards = (regexpr("[)<]-", links) > -1)
  if(any(backwards))
    from_to[ , backwards] = apply(from_to[ , backwards, drop=F], 2, rev)
  from = from_to[1, ]
  to = from_to[2, ]
  for(link in 1:length(links))
    cm[to[link], from[link] ] = signs[link]
  comment(cm) = theComment
  return (cm)
}


stringToCM(
"a->b
a-(a
b-(a")
stringToCM(
  "a)->b
b )-> c
  a-(a")

RHxy =
"R-(R
R->H
H-(R
H->x
x-(H
H->y
y-(H
y-(y"

stringToCM(RHxy)

ABCDE=
"A-(A
A->B
B-(A
B->C
C-(B
C->D
D-(C
D->E
E->D"

#' CMtoIPMnet
#'
#' Convert a community matrix to a string suitable for ipmnet.org/loop
#'
#' @param cm A community matrix
#' @return A string suitable for ipmnet.org/loop
#' @example {
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

CMtoIPMnet(stringToCM(ABCDE))

library("LoopAnalyst")
LoopAnalyst::make.cem(stringToCM(ABCDE))
