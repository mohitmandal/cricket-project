library (cricketr)

kohli_ODI <- getPlayerDataOD(profile = 253802, 
                             dir = "app/raw_data",
                             file = "kohli_ODI.csv",
                             type = "batting")

sharma_ODI <- getPlayerDataOD(profile = 34102, 
                              dir = "app/raw_data",
                              file = "sharma_ODI.csv",
                              type = "batting")
