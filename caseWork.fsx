type DrinkSize =
    | Small
    | Medium
    | Large

type CoffeeType = 
    | Americano
    | Espresso
    | Latte

type TeaType =
    | IceTea
    | Mango
    | GreenTea

type JuiceType =
    | Orange
    | Apple
    | Mix

type SodaType =
    | Fanta
    | Cola
    | Sprite

type MilkType =
    | LowFat
    | Regular
    | Soy

type Drink =
    | Coffee of drinkType: CoffeeType
    | Tea of drinkType: TeaType
    | Juice of drinkType: JuiceType
    | Soda of drinkType: SodaType
    | Milk of drinkType: MilkType

type DrinkFromInventory =
    { 
        Drink: Drink; 
        Price: double
    }

// function to compute price depending on the drink and size

let sizePriceMultiplier size =
    match size with
        | Small -> 0.5
        | Medium -> 0.6
        | Large -> 0.7

let getCoffeeTypePrice coffee =
    match coffee with 
        | Coffee(Americano) -> {Drink = coffee; Price = 5}
        | Coffee(Latte) -> {Drink = coffee; Price = 6}
        | Coffee(Espresso) -> {Drink = coffee; Price = 5.5}

let getTeaTypePrice tea =
    match tea with 
        | Tea(Mango) -> {Drink = tea; Price = 4}
        | Tea(IceTea) -> {Drink = tea; Price = 4.5}
        | Tea(GreenTea) -> {Drink = tea; Price = 5}

let getJuiceTypePrice juice =
    match juice with 
        | Juice(Orange) -> {Drink = juice; Price = 6}
        | Juice(Apple) -> {Drink = juice; Price = 5.5}
        | Juice(Mix) -> {Drink = juice; Price = 6.5}

let getSodaTypePrice soda =
    match soda with 
        | Soda(Cola) -> {Drink = soda; Price = 3.5}
        | Soda(Fanta) -> {Drink = soda; Price = 3}
        | Soda(Sprite) -> {Drink = soda; Price = 3.2}

let getMilkTypePrice milk =
    match milk with 
        | Milk(LowFat) -> {Drink = milk; Price = 3.5}
        | Milk(Soy) -> {Drink = milk; Price = 4}
        | Milk(Regular) -> {Drink = milk; Price = 3}
        
let getProduct (drink:Drink): DrinkFromInventory = 
    match drink with
        | Coffee(_) -> getCoffeeTypePrice drink
        | Tea(_) -> getTeaTypePrice drink
        | Juice(_) -> getJuiceTypePrice drink
        | Soda(_) -> getSodaTypePrice drink
        | Milk(_) -> getMilkTypePrice drink

let orderDrink drink size quantity =
    let sizePriceMultiplier = sizePriceMultiplier size;
    let selectedDrink = getProduct drink;
    let price = selectedDrink.Price * sizePriceMultiplier * quantity
    price

orderDrink (Drink.Coffee(CoffeeType.Americano)) Large 5

// adding VAT to the price 
let gtgVAT x = (x/100 * 25) + x

type orderDrinkMsg =
    | OrderDrink of Drink * DrinkSize * int 
    | LeaveAComment of string

let gtgAgent =
        MailboxProcessor<orderDrinkMsg>.Start(fun msgHandler ->
            let rec readNextMessage messages =
                async {
                    let! result = msgHandler.Receive()
                    match result with
                        | OrderDrink(d, ds, q) ->
                            let totalPrice = orderDrink d ds q
                            let drink = d
                            printfn $"Please pay DKK{totalPrice} for your {q} {drink} drinks. Thanks"
                            return! readNextMessage((d, ds, q)::messages)
                        | LeaveAComment (m) ->
                            let message = m
                            printfn "%A" m
                            return! readNextMessage messages
                }
            readNextMessage []
        )

gtgAgent.Post(OrderDrink(Drink.Milk(MilkType.Soy), Medium, 2))

gtgAgent.Post(LeaveAComment("I love you"))

