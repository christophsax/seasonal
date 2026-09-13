series {title = "Industrial Aggregate (Incl. Unclass.) A"
name = "VA00-91N" 
precision = 1 decimals = 2
file = "ftn10p2.txt" 
format = "2l2" 
start = 1991.jan}

arima {model = (0 1 2) (0 1 1)}
transform {
file = "tempprior.dat"
precision = 1
format = "2l2" name = "CSFEMP" mode = ratio type = (temporary)
function=log}
forecast{maxlead=24 print=none}
check{print=all savelog=(lbq nrm)}
x11 {  save = d11  }

