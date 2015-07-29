# Example 10:  mw1.spc  

# Adjustment of Single-Family Housing Starts from the Midwest Region
# of the US as an additive component of Total US Single-Family Starts.

series{
   period=12
   comptype=add
   title='MIDWEST ONE FAMILY Housing Starts'
   file='start64.dat'
   name='MW1FAM'
   format='1R'
#   savelog=peaks 
   span=(1984.1, )
}
transform{function=log}
arima{model=(0 1 1 )(0 1 1)}
regression{variables=(ao1985.1 ao1985.2)}
estimate{ }
check{print=all savelog=lbq}
x11{
  calendarsigma=select
  sigmavec=(jan feb)
  seasonalma=(s3x5 s3x5 s3x9 s3x9 s3x9 s3x9 s3x9 s3x9 s3x9 s3x9 s3x9 s3x5)
  sigmalim=(1.8 2.8)
  savelog=(m7 m10 m11 q2)
}
#cseats{}
slidingspans{
  fixmdl=yes  
  savelog=percents
  cutseas=4.5
  cutchng=4.5
# Nondefault thresholds added 10-19-98
}
history{
  estimates=(sadj sadjchng aic fcst)
  start=1996.1
  savelog=(asa ach asf)
}

