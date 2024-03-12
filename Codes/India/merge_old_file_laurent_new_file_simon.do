
cd "C:\Users\wb602786\OneDrive - WBG\Tech Project\Analysis\Data\India\raw_data"


import delimited "IND_foreign_names_for_laurent.csv", clear 

rename foreign_address foreign_fulladdress

// Figure out longest length
gen len=length(foreign_fulladdress)
summ len
// Convert to a fixed-length string
recast str245 foreign_fulladdress, force  // If the longest is less than 2045, use that number instead of 2045


tempfile tt1 

save `tt1', replace 

use IND_foreign_forSimon, clear

// Figure out longest length
gen len=length(foreign_fulladdress)
summ len
recast str245 foreign_fulladdress, force  // If the longest is less than 2045, use that number instead of 2045

destring foreign_panjivaid, replace

merge 1:1 country_iso foreign_firm_name_raw foreign_fulladdress foreign_postalcod foreign_panjivaid  using `tt1'

