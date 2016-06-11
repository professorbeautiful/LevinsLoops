#'stringToCM
#'
#'Conversion from a loop string to a community matrix.
#'
#' @param linkstring String of links separated by spaces and/or newlines. Each link connects two nodenames with "->" "o->" etc.
#' @return An NxN community matrix, where N=# distinct nodes.
#' @examples{
#' stringToCM(
#'    "a o-> b
#'     b o-> c
#'        a -o a")
#'  }

#'stringToCM = function(linkstring="a->b a-oa b-oa") {
  linkstring = gsub("[ \n]+", " ", linkstring) # Remove newlines
  # the next 2 lines remove  spaces,
  #  allow user to put in spaces for readability.
  linkstring = gsub(" *([o<]*-[o>]*) *", "\\1", linkstring)
  #Now, handle the "o->" and "<-o" cases:
  links  = strsplit(x = linkstring, split = "[ \n]")[[1]]
  is_two_way = grep(pattern = "[o<]-[o>]", links)
  if(length(is_two_way) > 0) {
    # replace each two-way link element by the two links it represents.
    links = c(links, sub("[o<]-", "-", links[is_two_way]))
    links = c(links, sub("-[o>]", "-", links[is_two_way]))
    # Remove these link elements
    links = links[-is_two_way]
  }
  linknodes <- strsplit(links, "[<o]*-[>o]*")
  names = unique(unlist(strsplit(links, "->|-o|<-|o-")))
  n = length(names)
  cm = matrix(rep(0,n^2), nrow=n, dimnames=list(names,names))
  signs = rep(-1, length(links))
  signs[grep("->|<-", links)] = 1
  from_to = as.matrix(as.data.frame(linknodes, stringsAsFactors=FALSE))
  ## Reverse any pointing left instead of right.
  backwards = (regexpr("[o<]-", links) > -1)
  if(any(backwards))
    from_to[ , backwards] = apply(from_to[ , backwards, drop=F], 2, rev)
  from = from_to[1, ]
  to = from_to[2, ]
  for(link in 1:length(links))
    cm[to[link], from[link] ] = signs[link]
  return (cm)
}


stringToCM(
"a->b
a-oa
b-oa")
stringToCM(
  "ao->b
b o-> c
  a-oa")

RHxy =
"R-oR
R->H
H-oR
H->x
x-oH
H->y
y-oH
y-oy"

stringToCM(RHxy)

ABCDE=
"A-oA
A->B
B-oA
B->C
C-oB
C->D
D-oC
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
s  matstring =
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
