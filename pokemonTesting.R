source("setup.R")
countByType = ggplot(data = pokemonByType) +
  geom_col(aes(x=type, y=count, fill = types)) +
  scale_fill_manual(values = pokemonByType[, colorCodes]) +
  theme(legend.position = "none") +
  labs(title = "Pokemon por tipo", x = "Tipo", y = "Cantidad de pokemon") + 
  scale_y_continuous(breaks = seq(10, 130, by = 20))
ggplotly(countByType, tooltip = "y")

bstPlot  = ggplot(data = pokemonDT, aes()) +
  geom_freqpoly(mapping = aes(x = BST), binwidth = 10) +
  labs(x = "Total de Características Base (BST)", 
        y = "Cantidad de pokemon") +
  scale_x_continuous(breaks = seq(min(pokemonDT[,BST]), max(pokemonDT[,BST]), 
                                  by = 20)) +
  scale_y_continuous(breaks = seq(0,50, by = 5))
  
ggplotly(bstPlot)
input = list("type1" = "Bug", "type2" = "Psychic", "stat" = BST)
BST = "BST"
ggplot() +
  #Type 1
  geom_freqpoly(aes(x = pokemonDT[Type1 == input$type1 | Type2 == input$type1, input$stat, with = FALSE][[1]], 
                    color = input$type1),
                binwidth = 10) + 
  #Type 2
  geom_freqpoly(aes(x = pokemonDT[Type1 == input$type2 | Type2 == input$type2, input$stat, with = FALSE][[1]], 
                    color = input$type2),
                binwidth = 10) + 
  scale_color_manual(colorByType)
labs()