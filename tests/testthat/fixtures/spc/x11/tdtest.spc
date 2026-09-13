series{
  comptype = add 
  title = 'M10000: Crude oil (seats)'
  period = 12
  name = M10000
  file = 'M10000.dat'
  format = '1L'
  print = (a18 a19)
#  type=stock
#  span = (1989.01 , )
#  modelspan=(1991.1, )
}
transform{function = log }

arima{model = (0 1 1)(0 1 1)}

regression{ 
  variables = (td/1989.01/ 
  )
#  b = 0.03f
#   aictest = td
} 
estimate{#savelog=aicc
} 
outlier{types=all} 


seats { 
#  savelog=(smd xmd nrm tse cvr cee prs aad)
  save = (s10 s11) 
}
#slidingspans{cutseas=4.0 cutchng=4.0 cuttd=1.0 } 
#
