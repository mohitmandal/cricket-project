library (cricketr)

kohli_ODI <- getPlayerDataOD(profile = 253802, 
                             dir = "app/raw_data",
                             file = "kohli_ODI.csv",
                             type = "batting")

kohli_T20 <- getPlayerDataTT(profile = 253802, 
                             dir = "app/raw_data",
                             file = "kohli_T20.csv",
                             type="batting")


