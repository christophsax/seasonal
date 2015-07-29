#SHOERS.spc was created automatically using X-12 Spec Wizard
# Created on Jun 24, 2006   at 14.26.37 
# Created using X-12 version 0.3s  build 121

series{
    title= 'SHOERS1982'
    file= 'SHOERS.DAT'
    format= '2L'
    period=12
    name=S0B566
    print=spc
   span=(1982.jan,1998.2)
#   save = b1
}
transform{
    function=log
}
spectrum{   savelog=(peaks tpk)
}
regression{
    variables=(TD1coef Easter[8] )
    #aictest=(TD Easter) savelog=aictest 
}
#outlier{
#    types=all
#    lsrun=3
#}
arima{model= (0 1 1)(0 1 1)}
forecast{
    maxlead=24
    print=none
}
estimate{#print=(roots regcmatrix) savelog=aicc
}
#check{print=all savelog=lbq}
x11{
    seasonalma=s3x5
#    savelog=(M1 M7 M8 M9 M10 M11 Q Q2)
   print = e18
}
#slidingspans{savelog=percent additivesa=percent}
#history{estimates=(fcst aic sadj sadjchng trend trendchng)}
#history{
##estimates=(seasonal trend)  
#estimates=( trend)  
#fixmdl=yes 
##print=sfr
#}
