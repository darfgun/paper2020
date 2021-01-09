#TODO löschen
censData00_geo <- censData00$GEO_ID %>% unique
censData10_geo <- censData10$GEO_ID %>% unique
censData00_var <- censData00$variable %>% unique
censData10_var <- censData10$variable %>% unique



any(is.na(cens_agr))
