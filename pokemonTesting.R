{
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(plotly)
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

bstPlot  = ggplot(data = pokemonDT, aes()) +
  geom_histogram(mapping = aes(x = BST), binwidth = 10)
ggplotly(bstPlot)

pokemonType1Plot = ggplot(data = pokemonDT) + geom_bar(aes(x = Type1))
ggplotly(pokemonType1Plot)

pokemonType2Plot = ggplot(data = pokemonDT) + geom_bar(aes(x = Type2))
ggplotly(pokemonType2Plot)
