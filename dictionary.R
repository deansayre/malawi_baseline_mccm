# creates a properly formatted dictionary for R from Excel files

questions <- rio::import("redcap_baseline.xlsx") %>%
  select(2,3) %>%
  mutate(`Variable / Field Name` = ifelse(str_detect(`Variable / Field Name`,
                                                     "(?<!\\s)Show"), NA, `Variable / Field Name`)) %>%
  filter(!is.na(`Variable / Field Name`) &
           !is.na(`Field Label`))




redcap_2 <- rio::import("redcap_baseline1.xlsx") %>%
  select(-1) %>%
  slice(-c(1,2)) %>%
  discard(~all(is.na(.))) %>%
  mutate(`Variable / Field Name` = ifelse(str_detect(`Variable / Field Name`, "(?<!\\s)Show"), NA, `Variable / Field Name`),
         `Variable / Field Name` = ifelse(str_detect(`Variable / Field Name`, "\\]="), NA, `Variable / Field Name`),
         `Variable / Field Name` = ifelse(str_detect(`Variable / Field Name`, "\\[\\w+\\]"), NA, `Variable / Field Name`),
         `Variable / Field Name` = ifelse(str_detect(`Variable / Field Name`, "^0"), NA, `Variable / Field Name`)) %>%
  t() %>%
  as.data.frame() %>%
  discard(~all(is.na(.))) %>%                      # removes columns with no data
  t() %>%
  as.data.frame()


redcap_3 <- redcap_2 %>%
  filter(!is.na(`Variable / Field Name`) &
           !is.na(`Field Attributes (Field Type, Validation, Choices, Calculations, etc.)`)) %>%
  select(c(1,2))


options <- redcap_2 %>%
  fill(`Variable / Field Name`,
       .direction = "down") %>%
  mutate(var_length = str_length(`Field Attributes (Field Type, Validation, Choices, Calculations, etc.)`),
         att = ifelse(var_length > 4, NA,
                      `Field Attributes (Field Type, Validation, Choices, Calculations, etc.)`),
         `Variable / Field Name` = word(`Variable / Field Name`, 1, sep = " Show ")) %>%
  select(-c(`Field Attributes (Field Type, Validation, Choices, Calculations, etc.)`,
            var_length)) %>%
  relocate(att, .after = `Variable / Field Name`) %>%
  filter(!is.na(att)) %>%
  pivot_longer(!c(`Variable / Field Name`,att)) %>%
  select(-name) %>%
  filter(!is.na(value)) %>%
  mutate(value = str_remove_all(value, "^\\d+"),
         value = str_remove_all(value, "^[:punct:]"),
         value = str_remove_all(value, "^\\s+"),
         value = str_remove_all(value, "^- "),
         value = word(value, 1, sep = "->"),
         `Variable / Field Name` = na_if(`Variable / Field Name`,"_co7_hc_lastcare] = '4'"),
         `Variable / Field Name` = na_if(`Variable / Field Name`,"<=5)")) %>%
  fill(`Variable / Field Name`, .direction = "down") %>%
  filter(!str_detect(value, "_"))

rm(redcap_2, redcap_3)

export(options, file = "dictionary.xlsx")
export(questions, file = "questions.xlsx")
