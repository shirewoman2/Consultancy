test_that("Output is a list", {
   MyPath <- file.path("C:/Users/", Sys.info()["user"], 
                       "/Certara/Simcyp PBPKConsult R Files - Simcyp PBPKConsult R Files/R Working Group internal/Tester files/allpossiblecompounds-md-v22.xlsx")
   skip_if_not(file.exists(MyPath))
   MyInfo <- extractExpDetails(MyPath)
   expect_true("list" %in% class(MyInfo))
   expect_identical(names(MyInfo), 
                    c("MainDetails", "Dosing", "CustomDosing", "DissolutionProfiles", 
                      "ReleaseProfiles", "ConcDependent_fup", "ConcDependent_BP", "pH_dependent_solubility"))
})
