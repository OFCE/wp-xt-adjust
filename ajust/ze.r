
eurozone <- tibble::tribble(
  ~State,     ~Adopted, ~Population, ~Nominal_GNI, ~Relative_GNI, ~GNI_pc,    ~preeuro,          ~Exceptions, ~iso,
  "Austria", "01/01/1999",    8858775L,      456779L,       "3.18%",  51460L, "Schilling",                   NA, "AT",
  "Belgium", "01/01/1999",   11467923L,      551595L,       "4.18%",  48030L,     "Franc",                   NA, "BE",
  "Cyprus", "01/01/2008",     875898L,       24628L,       "0.18%",  27710L,     "Pound", "Northern Cyprus[a]", "CY",
  "Estonia", "01/01/2011",    1324820L,       30856L,       "0.20%",  23260L,     "Kroon",                   NA, "EE",
  "Finland", "01/01/1999",    5517919L,      276085L,       "2.08%",  50010L,    "Markka",                   NA, "FI",
  "France", "01/01/1999",   67028048L,     2846910L,      "22.41%",  42960L,     "Franc",   "New Caledonia[b]", "FR",
  "Germany", "01/01/1999",   83019214L,     4038526L,      "31.79%",  42450L,      "Mark",                   NA, "DE",
  "Greece", "01/01/2001",   10722287L,      211647L,       "1.97%",  19750L,   "Drachma",                   NA, "EL",
  "Ireland", "01/01/1999",    4904226L,      316269L,       "1.69%",  64000L,     "Pound",                   NA, "IE",
  "Italy", "01/01/1999",   60359546L,     2081972L,      "16.91%",  34530L,      "Lira",                   NA, "IT",
  "Latvia", "01/01/2014",    1919968L,       33932L,       "0.24%",  17740L,      "Lats",                   NA, "LV",
  "Lithuania", "01/01/2015",    2794184L,       53162L,       "0.36%",  19080L,     "Litas",                   NA, "LT",
  "Luxembourg", "01/01/1999",     613894L,       45817L,       "0.33%",  73910L,     "Franc",                   NA, "LU",
  "Malta", "01/01/2008",     493559L,       14089L,       "0.07%",  28030L,      "Lira",                   NA, "MT",
  "Netherlands", "01/01/1999",   17282163L,      920333L,       "6.89%",  53100L,   "Guilder",           "Aruba[c]", "NL",
  "Portugal", "01/01/1999",   10276617L,      238204L,       "1.75%",  23200L,    "Escudo",                   NA, "PT",
  "Slovakia", "01/01/2009",    5450421L,      104778L,       "0.76%",  19210L,    "Koruna",                   NA, "SK",
  "Slovenia", "01/01/2007",    2080908L,       54169L,       "0.38%",  25940L,     "Tolar",                   NA, "SI",
  "Spain", "01/01/1999",   46934632L,     1430766L,      "10.75%",  30390L,    "Peseta",                   NA, "ES"
) |> 
  dplyr::mutate(iso3c = countrycode::countrycode(State, "country.name", "iso3c"))
ez <- eurozone |> dplyr::filter(Population>0.00*sum(Population)) |> dplyr::arrange(-Population) |> dplyr::pull(iso3c)

# globals <- set_globals("FRA", version = "x", ameco_version="11/2021")
# globals$ameco |> filter(ccode%in%c("pibpot"), year==2022|year==2021, unit==0) |> select(country, ccode, value, year) |> group_by(country) |> transmute(gpot = value/lag(value)-1) |> drop_na() |> print(n=30)
# 
# 
# globals$ameco |> filter(ccode%in%c("tvnairu"), year==2022, unit==0) |> select(country, ccode, value, year) |> group_by(country)  |> print(n=30)
