modelStringList = c(
  'R -(R R )-> H H )-> x H )-> y y )-> y # Fig 2 Levins & Schultz 1996',
  'a -( a     a )-> b  #Simple prey-predator',
  'a -( a     a )-> b     b )-> c #two-level food chain',
  'a -( a     a )-> b     b )-> c c )-> d #three-level food chain',
  'a -( a     a )-> b     b )-> c c )-> d d )-> e # four-level food chain',
  'a -( a     a )-> b     b )-> p1     b )-> p2      p1 )-( p2 #Two predators, positive feedback',
  'x1 )-> x2  x2 )-( x3 x3 ->x1 x3 -( x3 # Levins 1974 fig3A ',
  'Qout-(Qout    Pressure-( Pressure    Depth-> Pressure  Pressure ->Qout  Qout-(Depth  ### Denver Dash bathtub',
  'Pt1 -( Pt1    Pt1 )-> Hv1    Hv1 )-> Pred
   Pt2 -( Pt2    Pt2 )-> Hv2    Hv2 )-> Pred
   Hv1 )-> Para
     # Levins 1974 Parasite Fig 5 '
  ####I -( Para   I -( Hv1   I -( Pred   I -( I
  ### I_cide -( Parasite   I_cide -( Hvore1   I_cide -( Pred   I_cide -( I_cide
  ###  But I_cide as a node doesnt work.
)
