
library (dplyr)

person <- read.csv ( "m_person.txt" )
product <- read.csv ( "m_product.txt" )
supermaket <- read.csv ( "m_supermarket.txt" )
supermaket_sales <- read.csv ( "m_supermaket_sales.txt" )
supermaket_sales_breakdown <- read.csv ( "m_supermaket_sales_breakdown.txt" )

myJoin <-
    supermaket_sales %>%
    left_join ( supermaket_sales_breakdown, by="ss_id" ) %>%
    left_join ( supermaket, by="s_id" ) %>%
    left_join ( product, by="pr_id" ) %>%
    left_join ( person, by="pe_id" )

myJoin2 <-
    myJoin %>%
    mutate ( ssb_item_price = ssb_quantity * ssb_unitary_price_euro ) 

write.csv ( myJoin,  "myJoin.txt",  row.names=FALSE )
write.csv ( myJoin2, "myJoin2.txt", row.names=FALSE )

unique ( myJoin$pe_name )
unique ( myJoin$pr_name )
unique ( myJoin$s_name )