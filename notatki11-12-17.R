#notatki 11/12/2017

#xgboostExplainer - technika pozwalajaca na pokazanie dlaczego zla decyzja zostala podjeta
install.packages("data.table")
devtools::install_github(repo="AppliedDataSciencePartners/xgboostExplainer")

#warto to sprawdzic dla ekstremalnie odstajacych zmiennych

#poza tym

#ICE Box, partial dependency plots
#dzialaja dla wielu obserwacji, a nie tylko dla pojedynczej decyzji
#wykres score od atrybutu
#pokazuje granice decyzji i istotnosc takich zmiennych
#pokazuje tez istotnosc factorow - na przyklad wykres score od marki, pokazuje jak mozna by je pogrupowac

#warto to raczej zastosowac by ulepszyc model