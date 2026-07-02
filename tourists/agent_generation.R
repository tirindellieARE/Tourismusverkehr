raw = fread("P:/Verkehrsmodellierung/06_Jobs/188_touristische_Verkehr/Model_trafic_touristique/data/AuGQPV_2015/Finale_Auswertungsdatenbank_AGQPV2015_V2.csv")
agqpv= dplyr::filter(raw, STARTORTLANDISO != "CH")
agqpv= dplyr::filter(agqpv, ZIELORTLAND == 1)
agqpv= dplyr::filter(agqpv, FAHRTZWECK == 5)

agqpv = dplyr::select(agqpv, c(BEFRAGUNGSORT, WOHNORTLAND, WOHNORTORTLATITUDE,WOHNORTORTLONGITUDE,STARTORTLAND, 
                               STARTORTORTLATITUDE,STARTORTORTLONGITUDE,ZIELORTORTLATITUDE,ZIELORTORTLONGITUDE,
                               ANZAHLUEBERNACHTUNGEN, GEWICHT))
new_names = c("grenz", "nationality", "residence_long", "residence_lat", "start_country","origin_long", "origin_lat", 
              "dest_long", "dest_lat", "n_nights", "weight")
names(agqpv) = new_names
agqpv = na.omit(agqpv)

# set scaling factor for computational reasons
SCALING_FACTOR = 1000

set.seed(1)
w = agqpv$weight/SCALING_FACTOR
reps = floor(w) + (runif(length(w)) < (w - floor(w)))
agents = agqpv[rep(1:.N, reps)]