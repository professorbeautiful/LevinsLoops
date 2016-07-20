# plot(0:1,0:1, axes=F, pch="",xlab="",ylab="")
#
#
# parse(text=paste0('atop(', paste(c('dot(z)==7','dot(x) == 2*y - 3*x', 'dot(y)==5*z'), collapse=", "),  ")")  )
#
# text(1/2, 1/2,
#   expression(c(dot(z)==7, dot(x) == 2*y - 3*x, dot(y)==5*z)))
# )

formulas = c('over(partialdiff(z), partialdiff(t))==7', 'dot(x) == 2*y - 3*x', 'dot(y)==5*z')
plot(0:(length(formulas)+1),0:(length(formulas)+1), axes=F, pch="",xlab="",ylab="")

for(n in 1:length(formulas))
  text(length(formulas)/10, n, parse(text=formulas[n]), adj = 0)
