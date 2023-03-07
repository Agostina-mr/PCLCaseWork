type Payment (_amount, _paymentDate, _vat) =
    let float amount = _amount
    let string paymentDate = _paymentDate
    let float vat = _vat
    do
        printfn "Initialized" 
    new() = Payment(1004454, "01-04-2023", 21)

type Product(_price) =
    let price = _price
    new() = Product()

type Order (_numberOfItems, _product, _payment) =
    let float totalPrice = 0
    let int numberOfItems = _numberOfItems
    let Product product = _product
    new() = Order()

type Customer (_firstName, _lastName) = 
    let string firstName = _firstName
    let string lastName = _lastName
    new() = Customer()

type Drink (_price, _size, _drinkType) =
    inherit Product(_price)
    let USize size = _size
    let UType drinkType = _drinkType
    new() = Drink()

type VIAPerson(_viaId, _firstName, _lastName) =
    inherit Customer(_firstName, _lastName)
    let int viaId = _viaId
    new() = VIAPerson()

type SOSUPerson(_workPlace, _firstName, _lastName) =
    inherit Customer(_firstName, _lastName)
    let string workPlace = _workPlace
    new() = SOSUPerson()

type CreditCard(_cardType, _fee, _amount, _paymentDate, _vat) =
    inherit Payment(_amount, _paymentDate, _vat)
    let UCardType cardType = _cardType
    let float fee = _fee
    new() = CreditCard()

type Cash(_fee, _amount, _paymentDate, _vat) =
    inherit Payment(_amount, _paymentDate, _vat)
    let UCashRound roundingRule = 0
    new() = Cash()

type MobilePay(_phoneNumber, _fee, _amount, _paymentDate, _vat) =
    inherit Payment(_amount, _paymentDate, _vat)
    let int phoneNumber = _phoneNumber
    let float fee = _fee
    new() = MobilePay()



