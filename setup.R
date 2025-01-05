{
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(shiny)
}

pokemonDT = data.table(read.csv("PokemonData.csv"))

pokemonDT[Num == 29, Name := "NidoranF"]
pokemonDT[Num == 32, Name := "NidoranM"]
pokemonDT[Num == 669, Name := "Flabébé"]

pokemonDT[, count := 1]
pokemonDT[, count := sum(count), by = Num]

pokemonDT[count != 1, isMega := str_detect(Name, "Mega")]
pokemonDT[count == 1, isMega := FALSE]

pokemonDT[, Type1 := as.factor(Type1)]
pokemonDT[, Type2 := as.factor(Type2)]
pokemonDT[, Generation := as.factor(Generation)]
pokemonDT[Type2=="", Type2:=NA]
pokemonDT[, isMonoType := is.na(Type2)]

pokemonDT[, BST := HP + Attack + Defense + SpAtk + SpDef + Speed]
pokemonDT[, bstCount := 1]
pokemonDT[, bstCount := sum(bstCount), by = BST]

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
colorByType = c("Bug" = "#aabb22", "Dark"="#775544", "Dragon"="#7766ee",
                "Electric"="#ffcc33", "Fairy"="#ee99ee","Fighting" = "#bb5544", 
                "Fire"="#ff4422","Flying"= "#8899ff","Ghost"= "#6666bb",
                "Grass"= "#77cc55","Ground"= "#ddbb55","Ice"= "#66ccff",
                "Normal"= "#aaaa99","Poison"= "#aa5599","Psychic"= "#ff5599",
                "Rock"= "#bbaa66","Steel"= "#aaaabb","Water"= "#3399ff")
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
statToShow = list("BST" = 16,"HP" = 5, "Attack" = 6, "Defense" = 7, "SpAttack" = 8, "SpDef" = 9, "Speed" = 10)
statNames = c("BST","HP", "Attack", "Defense", "SpAtk", "SpDef", "Speed")
statIndexes = c(16,5,6,7,8,9,10)
