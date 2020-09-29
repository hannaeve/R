library(ggplot2)
library(viridis)
library(scico)
library(ggthemes)
library(dplyr)

?diamonds
diamonds = diamonds

gg <- ggplot(diamonds, aes(x=carat, y=price))
gg +  geom_point(shape=18, aes(color=price, size=carat)) +
      scale_color_viridis(option="D", name = "Price") + 
      theme_minimal() +
      labs(title="Price and Carats of the Diamonds", 
       subtitle="From Diamonds dataset", 
       y="Price in US Dollars", 
       x="Weight of the Diamond",
       size="Weight of the \nDiamond"
      )

gg2 <- ggplot(diamonds, aes(x=carat, y=price, group=cut))
gg2 + geom_point(shape=18, aes(col=cut), size=3) + theme_clean() +
      scale_color_scico_d(palette="tokyo", begin=0, end=0.8, name="Quality of the Cut") +
      labs(title="Price and Carats of the Diamonds", 
           subtitle="From Diamonds dataset", 
           y="Price in US dollars", 
           x="Weight of the diamond"
           ) + 
      theme(legend.position=c(0.9,0.5),
            legend.background = element_rect(fill="white", 
                                         color="white")
        ) 


