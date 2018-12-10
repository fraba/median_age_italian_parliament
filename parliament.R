require(SPARQL)
endpoint <- "http://dati.camera.it/sparql"

foaf_person_dati_camera <- data.frame()

list_letters <- list(c("A","E"), c("F", "J"), c("K", "O"), c("P", "T"), c("U", "Z"))

for (ll in list_letters) {
  print(paste0(ll[1], " ", ll[2]))
  query <- sprintf("
                   SELECT DISTINCT ?entity ?firstName ?surname ?birthDate ?gender ?birthPlace ?deathDate ?deathPlace ?mandatoCamera ?startDate ?endDate WHERE {
                   
                   ?entity rdf:type <http://xmlns.com/foaf/0.1/Person>.
                   ?entity foaf:surname ?surname.
                   ?entity foaf:firstName ?firstName.
                   ?entity foaf:gender ?gender.
                   ?entity ocd:rif_mandatoCamera ?mandatoCamera.
                   ?mandatoCamera ocd:startDate ?startDate.
                   OPTIONAL {
                    ?mandatoCamera ocd:endDate ?endDate.
                   }
                   
                   OPTIONAL {
                   ?entity <http://purl.org/vocab/bio/0.1/Birth> ?birthNode.
                   ?birthNode <http://purl.org/vocab/bio/0.1/date> ?birthDate
                   }
                   
                   OPTIONAL {
                   ?entity <http://purl.org/vocab/bio/0.1/Birth> ?birthNode.
                   ?birthNode ocd:rif_luogo ?birthPlace
                   }
                   
                   OPTIONAL {
                   ?entity <http://purl.org/vocab/bio/0.1/Death> ?deathNode.
                   ?deathNode <http://purl.org/vocab/bio/0.1/date> ?deathDate
                   }
                   
                   OPTIONAL {
                   ?entity <http://purl.org/vocab/bio/0.1/Death> ?deathNode.
                   ?deathNode ocd:rif_luogo ?deathPlace
                   }
                   
                   FILTER(regex(str(?surname), \"^[%s-%s]\", \"i\"))
                   
                   } LIMIT 10000
                   ",ll[1], ll[2])
  qd <- SPARQL(endpoint, query)
  foaf_person_dati_camera <- rbind(foaf_person_dati_camera, qd$results)
}

date_format = "%Y%m%d"
foaf_person_dati_camera <- 
  foaf_person_dati_camera %>%
  mutate(birthDate = as.Date(birthDate, date_format), 
         deathDate = as.Date(birthDate, date_format), 
         startDate = as.Date(startDate, date_format),
         endDate = as.Date(endDate, date_format))

seq <- 
  as.character(seq(as.Date("1946-06-01"),
                   as.Date("2018-12-01"),
                   by = 'month'))
res.list <- list()
for (this_day_chr in seq) {
  print(this_day_chr)
  this_day <- as.Date(this_day_chr)
  this_df <- 
    foaf_person_dati_camera %>%
    filter(startDate <= this_day & (is.na(endDate) | endDate >= this_day))
  this_median <- round(as.numeric(median(this_day - this_df$birthDate, na.rm = T)) / 365, 0)
  res.list[[this_day_chr]] <- c(day = this_day_chr, median = this_median)
}

parliament_dat <- 
  data.frame(matrix(unlist(res.list), 
                    nrow=length(res.list), byrow=T))
colnames(parliament_dat) <- c('date','median_age')
parliament_dat$date <- as.Date(parliament_dat$date)
parliament_dat$median_age <- as.numeric(as.character(parliament_dat$median_age))


# Population age

## 1861 - 2011
istat_age1 <- 
  read_excel("istat_age1.xlsx")
istat_age1 <- gather(istat_age1, age, n, 2:22)

myFun <- function(x) {
  require(stringr)
  if (x == "<5") {
    return(mean(seq(0, 4, 1))
    )
  }
  x <- str_extract_all(x, "\\d+")[[1]]
  if (x[1] == 100) {
    return(102)
  }
  mean(seq(x[1], x[2], 1))
}
  
istat_age1$age_numeric <- sapply(istat_age1$age, myFun)
istat_age1$n[is.na(istat_age1$n)] <- 0

median_age_by_year1 <-
  istat_age1 %>%
  group_by(date = as.Date(paste0(year, "-01-01"))) %>%
  summarize(median_age = median(rep(age_numeric, n), na.rm = T))

## 2012 - 2018
istat_age2 <- 
  read_excel("istat_age2.xlsx")
istat_age2 <- gather(istat_age2, year, n, 2:8)
istat_age2$age <- as.numeric(istat_age2$age)
istat_age2$age[is.na(istat_age2$age)] <- 100

median_age_by_year2 <- 
  istat_age2 %>%
  group_by(date = as.Date(paste0(gsub("y", "", year), "-01-01"))) %>%
  summarize(median_age = median(rep(age, n), na.rm=T))

population_dat <- 
  rbind(median_age_by_year1, median_age_by_year2)

dat <-
  rbind(parliament_dat %>% mutate(what = 'Italian parliament'), 
        population_dat %>% mutate(what = 'Italian population'))


italian_general_elections <- 
  read_csv("~/public_git/media_age_ita_parliament/italian_general_elections.csv")

ggplot() + 
  geom_line(data = dat, 
            aes(x=date, y=median_age, colour = what),
            size = .8) + 
  scale_x_date(limits = as.Date(c("1948-01-01", "2019-01-01")),
               breaks = as.Date(italian_general_elections$date[
                 italian_general_elections$date >= as.Date("1948-01-01")]),
               date_labels = "%b-%Y") + 
  scale_y_continuous(breaks = c(30,40,46,50)) +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = 'Set1') + 
  labs(x=NULL, y='median age',colour=NULL, caption = "@FrBailo, Data: ")
  
  

