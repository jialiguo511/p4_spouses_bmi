rm(list=ls());gc();source(".Rprofile")

baseline <- baseline %>% 
  left_join(lab,
            by = c('carrs','fup','pid','site','age','sex','fpg','tg','hba1c')) %>% 
  # add spouse indicator
  left_join(spousedyads_clean %>% 
              select(pid,hhid,spousedyad_new),
            by = c("pid","hhid")) 

c1bs  <- baseline %>% dplyr::filter(carrs == 1 & fup == 0)
c2bs  <- baseline %>% dplyr::filter(carrs == 2 & fup == 0)


followup <- followup %>% 
  left_join(lab,
            by = c('carrs','fup','pid','pcarrs','site')) %>% 
  # add spouse indicator
  left_join(spousedyads_clean %>% 
              select(pid,spousedyad_new),
            by = c("pid")) %>% 
  # recode fup to indicate pcarrs
  mutate(fup = case_when(carrs == 1 & pcarrs == 1 ~ 7,
                         carrs == 2 & pcarrs == 1 ~ 2,
                         TRUE ~ fup))

c1fu1 <- followup %>% dplyr::filter(carrs == 1 & fup == 1) %>% select(where(~ !all(is.na(.))))
c1fu2 <- followup %>% dplyr::filter(carrs == 1 & fup == 2) %>% select(where(~ !all(is.na(.))))
c1fu3 <- followup %>% dplyr::filter(carrs == 1 & fup == 3) %>% select(where(~ !all(is.na(.))))
c1fu4 <- followup %>% dplyr::filter(carrs == 1 & fup == 4) %>% select(where(~ !all(is.na(.))))
c1fu5 <- followup %>% dplyr::filter(carrs == 1 & fup == 5) %>% select(where(~ !all(is.na(.))))
c1fu6 <- followup %>% dplyr::filter(carrs == 1 & fup == 6) %>% select(where(~ !all(is.na(.))))
c1fu7 <- followup %>% dplyr::filter(carrs == 1 & fup == 7) %>% select(where(~ !all(is.na(.))))

c2fu1 <- followup %>% dplyr::filter(carrs == 2 & fup == 1) %>% select(where(~ !all(is.na(.))))
c2fu2 <- followup %>% dplyr::filter(carrs == 2 & fup == 2) %>% select(where(~ !all(is.na(.))))
