StoreLocation <- 
  data.frame(StoreLocation_id=c(1,2,3,4,5),
             address_id=c(2, 2, 1,1,1),
             city=c("alex", "cairo", "alex","cairo", "alex"),
             country=c(rep("egypt",5)),
             region=c("a","b","a","b","a"))

month_table <- 
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c(rep("q1",3),rep("q2",3),rep("q3",3),rep("q4",3)))

size <- 
  data.frame(key=c("personal", "small", "medium large","xlarge"))

dough <- 
  data.frame(key=c("whole wheat thin", " white regular", "stuffed crust"))

cheese <- 
  data.frame(key=c("Swiss", "cheddar", "Mozzarella"))

Topping <- 
  data.frame(key=c("tomatoes", "pepper", "onions","pepperoni")) 

price <- 
  data.frame(key=c(70,80,99))
             
    


gen_order <- function(no_of_recs) {
  id <- sample(c(1:no_of_recs), no_of_recs, replace=T)
  # Generate transaction data randomly
  loc <- sample(StoreLocation$StoreLocation_id, no_of_recs, 
                replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  
  time_year <- sample(c(2017,2018,2019, 2020), no_of_recs, replace=T)
  
  pizza_size <- sample(size$key, no_of_recs, replace=T,prob=c(1,1,1,2))
  
  Dough_type <- sample(dough$key, no_of_recs, replace=T)
  
  Cheese_type <- sample(cheese$key, no_of_recs, replace=T)
  
  Topping <- sample(Topping$key, no_of_recs, replace=T)
  
  quantity <- sample(c(1:3), no_of_recs, replace=T)
  
  price <- sample(price$key, no_of_recs, replace=T)
  
  Profit <- quantity*price
  
  Pizza <- data.frame( 
    id = id,
    size = pizza_size,
    cheese = Cheese_type,
    Dough =Dough_type,
    Topping = Topping
  )
  write.csv(Pizza,"Pizza.csv")
  read.csv(file = 'Pizza.csv')
  
  orders <- data.frame(
    id = id,
    month=time_month,
    year=time_year,
    store=loc,
    quantity = quantity,
    Profit=Profit)
  
  write.csv(orders,"order.csv")
  read.csv(file = "order.csv")
  
  date <- data.frame(
    id = id,
    month=time_month,
    year=time_year)
  
  write.csv(date,"date.csv")
  read.csv(file = "date.csv")
  
  ordersmerge <- merge(orders, Pizza, by = c('id'))
  
  return(orders)
}

# Now create the sales fact table
orders_fact <- gen_order(900)
write.csv(orders_fact,"orders.csv")
read.csv(file = "orders.csv")


# Look at a few records
head(orders_fact)


revenue_cube <- 
  tapply(orders_fact$Profit, 
         orders_fact[,c("size","quantity", "year","store" )], 
         FUN=function(x){return(sum(x))})


revenue_cube

dimnames(revenue_cube)


# cube data in  2019, store5
revenue_cube[, , "2019","5"]


#drilldown and roll-up operations show that customers are beginning to prefer bigger pizzas.

apply(revenue_cube, c("size","quantity","year"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c("size","quantity","store"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c("size","quantity"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
