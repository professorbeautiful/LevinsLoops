dfdp = c(1,0,0,0)  # vector of derivatives of f with respect to the moving parameter.
A = cm.levins  ### community matrix
solve(A, -dfdp) ### c(1,0,1,0), ok
solve(A, -c(0,1,0,0))  ## c(0,0,1,0)   ### aha!   LoopAnalyst is wrong!
solve(A, -c(0,0,1,0))  ## 1 -1  2 -1  #### LoopAnalyst says, + + + -
solve(A, -c(0,0,0,1))  ## 0  0 -1  1  #### LoopAnalyst says, 0 0 - +.  Agreement.
### I think that make.cem produces the transpose.

make.cem.roger = function(CM) {
  answer = #(-1)^nrow(CM) *
      sapply(1:nrow(CM), function(input) solve(CM, -( (1:nrow(CM))==input)))
  colnames(answer) = rownames(answer)
  out.cm(sign(answer))
}
make.cem.roger(cm.levins)   ### OK.

make.cem.roger(stringToCM(ABCDE))
out.cm(sign(t(make.cem(stringToCM(ABCDE)))))  ### they agree

'R o-> H H o-> x H o-> y y o-> y # Fig 2 Levins & Schultz 1996'
foodchain_1 = 'a -o a     a o-> b  #Simple prey-predator'
foodchain_2 = 'a -o a     a o-> b     b o-> c #two-level food chain'
foodchain_3 = 'a -o a     a o-> b     b o-> c c o-> d #three-level food chain'
foodchain_4 = 'a -o a     a o-> b     b o-> c c o-> d d o-> e # four-level food chain'

two_pred_pos_feedback =   'a -o a     a o-> b     b o-> p1     b o-> p2      p1 o-o p2 #Two predators, positive feedback'
make.cem.roger(stringToCM(two_pred_pos_feedback))  ## oops off by  *(-1)
out.cm(sign(t(make.cem(stringToCM(two_pred_pos_feedback))))) ## correct?

compare.make.cem.functions = function(CMstring) {
  mine <- (make.cem.roger(stringToCM(CMstring)))
  theirs <- out.cm(sign(round(t(make.cem(stringToCM(CMstring))))))
  return(identical(mine,theirs))
}

compare.make.cem.functions(foodchain_2)
compare.make.cem.functions(foodchain_3)
compare.make.cem.functions(foodchain_4)
compare.make.cem.functions(two_pred_pos_feedback)  ## opposite, they are right.
det(stringToCM(two_pred_pos_feedback))  ### -3.   System feedback is negative.

pbcopy = function(x) write(x, file=pipe("pbcopy"))
pbcopy(CMtoIPMnet(stringToCM(two_pred_pos_feedback)))
####IPMnet agrees with LoopAnalyst on predictions. But says system is unstable:
# "The characteristic polynomial coefficients are not of the same sign. The system is not stable."

