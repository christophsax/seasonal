#H:\Tutorial\Warehouse clubs and supercenters_SuperBowl.spc was created on 7/16/2015 9:51:51 AM
#Created using X-13A-S version 1.1 build 17

series{ 
    file = "r:\data\share\timeseries\x13as\x13html.opt\Warehouse clubs and supercenters.dat"
    period = 12
    format = Datevalue
}
transform{ 
    function = log
}
regression{ 
    variables = (  td    AO2000.Mar TC2001.Feb )
    aictest = user #( td easter  user)
    file = "r:\data\share\timeseries\x13as\x13html.opt\MultiEaster.dat"
    format = Datevalue
    user = (BeforeEaster AfterEaster )
    usertype = (holiday holiday )
    savelog = aictest
}
outlier{ 
    types = ( AO LS TC )
}
arima{ 
    model =  (0 1 1)(0 1 1)
}
forecast{ 
    maxlead = 24
    print = none
}
estimate{ 
    print = (roots regcmatrix acm)
    savelog = (aicc aic bic hq afc)
}
check{ 
    print = all
    savelog = (lbq nrm)
}
x11{ 
    seasonalma = s3x3
    savelog = all
}
slidingspans{ 
    savelog = percent
    additivesa = percent
}
history{ 
    estimates = (fcst aic sadj sadjchng trend trendchng)
    savelog = (asa ach atr atc)
}
