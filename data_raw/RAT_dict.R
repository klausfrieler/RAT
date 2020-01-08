RAT_dict_raw <- readRDS("data_raw/RAT_dict.RDS")
names(RAT_dict_raw) <- c("key", "DE", "EN")
RAT_dict_raw <- RAT_dict_raw[,c("key", "EN", "DE")]
RAT_dict <- psychTestR::i18n_dict$new(RAT_dict_raw)
usethis::use_data(RAT_dict, overwrite = TRUE)
