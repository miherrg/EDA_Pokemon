{
  #Loading of libraries
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(shiny)
  library(dplyr)
  library(psych)
}

#Read the dataset
pokemonDT = data.table(read.csv("PokemonData.csv"))

#Calculate the BST
pokemonDT[, BST := HP + Attack + Defense + SpAtk + SpDef + Speed]

#Correcting the names of certain Pokémon due to encoding errors
pokemonDT[Num == 29, Name := "NidoranF"]
pokemonDT[Num == 32, Name := "NidoranM"]
pokemonDT[Num == 669, Name := "Flabébé"]

#Ductape solution to find the true megas (without including Yanmega or Meganium)
pokemonDT[, count := 1]
pokemonDT[, count := sum(count), by = Num]
pokemonDT[count != 1, isMega := str_detect(Name, "Mega")]
pokemonDT[count == 1, isMega := FALSE]
pokemonDT[, count:=NULL]

#Assert the categorical variables as categorical
pokemonDT[, Type1 := as.factor(Type1)]
pokemonDT[, Type2 := as.factor(Type2)]
pokemonDT[, Generation := as.factor(Generation)]
pokemonDT[, Num := as.factor(Num)]
pokemonDT[Type2=="", Type2:=NA]


#The best way I could think of separating each type
bugPokemon = pokemonDT[Type1=="Bug" | Type2=="Bug"][,mainType := "Bug"]
darkPokemon = pokemonDT[Type1=="Dark" | Type2=="Dark"][,mainType := "Dark"]
dragonPokemon = pokemonDT[Type1=="Dragon" | Type2=="Dragon"][,mainType := "Dragon"]
electricPokemon = pokemonDT[Type1=="Electric" | Type2=="Electric"][,mainType := "Electric"]
fairyPokemon = pokemonDT[Type1=="Fairy" | Type2=="Fairy"][,mainType := "Fairy"]
fightingPokemon = pokemonDT[Type1=="Fighting" | Type2=="Fighting"][,mainType := "Fighting"]
firePokemon = pokemonDT[Type1=="Fire" | Type2=="Fire"][,mainType := "Fire"]
flyingPokemon = pokemonDT[Type1=="Flying" | Type2=="Flying"][,mainType := "Flying"]
ghostPokemon = pokemonDT[Type1=="Ghost" | Type2=="Ghost"][,mainType := "Ghost"]
grassPokemon = pokemonDT[Type1=="Grass" | Type2=="Grass"][,mainType := "Grass"]
groundPokemon = pokemonDT[Type1=="Ground" | Type2=="Ground"][,mainType := "Ground"]
icePokemon = pokemonDT[Type1=="Ice" | Type2=="Ice"][,mainType := "Ice"]
normalPokemon = pokemonDT[Type1=="Normal" | Type2=="Normal"][,mainType := "Normal"]
poisonPokemon = pokemonDT[Type1=="Poison" | Type2=="Poison"][,mainType := "Poison"]
psychicPokemon = pokemonDT[Type1=="Psychic" | Type2=="Psychic"][,mainType := "Psychic"]
rockPokemon = pokemonDT[Type1=="Rock" | Type2=="Rock"][,mainType := "Rock"]
steelPokemon = pokemonDT[Type1=="Steel" | Type2=="Steel"][,mainType := "Steel"]
waterPokemon = pokemonDT[Type1=="Water" | Type2=="Water"][,mainType := "Water"]
types = c("Bug", "Dark", "Dragon", "Electric", "Fairy", "Fighting", "Fire",
          "Flying", "Ghost", "Grass", "Ground", "Ice", "Normal", "Poison",
          "Psychic", "Rock", "Steel", "Water")

#Dictionary of the "official" colors of each type
colorByType = c("Bug" = "#aabb22", "Dark"="#775544", "Dragon"="#7766ee",
                "Electric"="#ffcc33", "Fairy"="#ee99ee","Fighting" = "#bb5544",
                "Fire"="#ff4422","Flying"= "#8899ff","Ghost"= "#6666bb",
                "Grass"= "#77cc55","Ground"= "#ddbb55","Ice"= "#66ccff",
                "Normal"= "#aaaa99","Poison"= "#aa5599","Psychic"= "#ff5599",
                "Rock"= "#bbaa66","Steel"= "#aaaabb","Water"= "#3399ff")
#Datatable to count each pokemon form each type
pokemonByType = data.table(
  type = types,
  count = c(nrow(bugPokemon), nrow(darkPokemon), nrow(dragonPokemon),
            nrow(electricPokemon), nrow(fairyPokemon), nrow(fightingPokemon),
            nrow(firePokemon), nrow(flyingPokemon), nrow(ghostPokemon),
            nrow(grassPokemon), nrow(groundPokemon), nrow(icePokemon),
            nrow(normalPokemon), nrow(poisonPokemon), nrow(psychicPokemon),
            nrow(rockPokemon), nrow(steelPokemon), nrow(waterPokemon)),
  colorCodes = colorByType
)

#Names of the stats to reference in the input
statNames = c("BST","HP", "Attack", "Defense", "SpAtk", "SpDef", "Speed")


#Reshape to long format for box plot.
boxDT = melt(pokemonDT, measure.vars = statNames, 
     variable.name = "stat")

#Legendary and Mega Comparison
pokemonDT[Legendary == FALSE & isMega == FALSE, category := "Normal"]
pokemonDT[Legendary == TRUE & isMega == FALSE, category := "Legendario"]
pokemonDT[Legendary == FALSE & isMega == TRUE, category := "Mega"]
pokemonDT[Legendary == TRUE & isMega == TRUE, category := "MegaLegendario"]

#Bug analysis
larvalPokemon = c("Caterpie", "Weedle", "Wurmple", 
                  "Sewaddle", "Venipede", "Scatterbug")
pupaPokemon = c("Metapod", "Kakuna", "Silcoon", "Cascoon",
                "Swadloon", "Whirlipede", "Spewpa")
adultPokemon = c("Butterfree", "Beedrill", "Beautifly", "Dustox",
                 "Leavanny", "Scolipede", "Vivillon")
pokemonDT[Name %in% larvalPokemon, bugStage := 1]
pokemonDT[Name %in% pupaPokemon, bugStage := 2]
pokemonDT[Name %in% adultPokemon, bugStage := 3]
pokemonDT[, bugStage := factor(bugStage, levels = c(1,2,3), labels = c("larval", "pupa", "adult"))]
