library(magrittr)
library(ggplot2)

label_wrap_gen <- function(width = 25) {
  function(variable, value) {
    lapply(strwrap(as.character(value), width=width, simplify=FALSE), 
           paste, collapse="\n")
  }
}

library(RColorBrewer)
library(directlabels)

textConnection("Generic name,Brand,Company, Sales, Year
               insulin glargine, Lantus, Sanofi, 8433, 2014
               sitagliptin, Januvia, Merck & Co, 3931, 2014
               insulin lispro, Humalog, Eli Lilly, 2785, 2014
               insulin aspart, Novo rapid, Novo Nordisk, 2780, 2014
               insulin detemir, Levemir, Novo Nordisk, 2265, 2014
               liraglutide, Victoza, Novo Nordisk, 2139, 2014
               sitagliptin+metformin, Janumet, Merck & Co, 2071, 2014
               insulin aspart, NovoLog, Novo Nordisk, 1572, 2014
               insulin lispro, Humalin, Eli Lilly, 1400, 2014
               vildagliptin, Galvus, Novartis, 1224, 2014
               insulin glargine, Lantus, Sanofi, 4990, 2020
               sitagliptin, Januvia, Merck & Co, 3991, 2020
               insulin aspart, Novo rapid, Novo Nordisk, 3568, 2020
               insulin lispro, Humalog, Eli Lilly, 3047, 2020
               canagliflozin, Invokana, Johnson & Johnson, 3030, 2020
               liraglutide, Victoza, Novo Nordisk, 2794, 2020
               sitagliptin+metformin, Janumet, Merck & Co, 2827, 2020
               insulin detemir, Levemir, Novo Nordisk, 2608, 2020
               insulin degludec, Tresiba, Novo Nordisk, 1997, 2020
               insulin aspart, NovoLog, Novo Nordisk, 1800, 2020
               insulin glargine (U300), Toujeo, Sanofi, 1494, 2020
               insulin lispro, Humalin, Eli Lilly, 1452, 2020
               vildagliptin, Galvus, Novartis, 1421, 2020
               dapagliflozin, Farxiga, AstraZeneca, 1337, 2020
               saxagliptin, Onglyza, AstraZeneca, 1325, 2020
               dulaglutide, Trulicity, Eli Lilly Boehringer Ingelheim, 1266, 2020
               insulin degludec+liraglutide, Xultrophy, Novo Nordisk, 1168, 2020") %>%
  read.csv %>% {
    ann_brands <- .[.$Year==2020,]
    colourCount <- (.)%>%"["("Brand") %>% unique %>% nrow
    Mycolors <- colorRampPalette(brewer.pal(8,"Dark2"))(colourCount)
    
    ggplot(.,aes(x=Year,y=Sales/1000,color=Brand)) +
      geom_point(size=3) +
      geom_line(size=1) +
      scale_x_continuous(breaks=c(2014, 2020), limits=c(2013,2030)) +
      expand_limits(y = 0) +
      ylab("Sales ($b)") + 
      theme(legend.position="none") +
      facet_grid(~Company, labeller=label_wrap_gen(width=10)) +
      scale_colour_manual(values=Mycolors) +
      ggtitle("Blockbuster diabetes drugs 2014 and 2020\n(Data:FirstWord Lists)") +
      geom_dl(aes(label=Brand),list("last.points",
                                    "calc.boxes","calc.borders",
                                    qp.labels("y", "bottom", "top")))
  }
ggsave("Diabetesblockbusters.pdf",width = 12.5,height = 4)
ggsave("Diabetesblockbusters.png",width = 12.5,height = 4)