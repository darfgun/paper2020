total_burden_dis <- total_burden %>%
        distinct(Single.Year.Ages.Code,Gender.Code,Race,Year.Code,Hispanic.Origin,label_cause,#TODO regionagr
                 .keep_all = TRUE)
