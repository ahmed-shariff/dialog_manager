from easydict import EasyDict


def order_taxi(location):
    if location is not None:
        print(f"ordered taxi to {location}")
        return True
    return False


def book_room(number, nights, price, city):
    if all([i is not None for i in [number, nights, price, city]]):
        print(f"Booked room for {number} for {nights} nights in a {price} price-range in {city}")
        return True
    return False


def book_ticket(number, departure, to, price):
    if all([i is not None for i in [number, departure, to, price]]):
        print(f"Booked {number} tickets from {departure} to {to} in a {price} price-range")
        return True
    return False


class functions(EasyDict):
    plurality = "plurality"

    root_concern = "root_concern"

    order_taxi = "order_taxi"
    order_taxi_location = "order_taxi_location"

    book_room = "book_room"
    book_room_number = "book_room_number"
    book_room_nights = "book_room_nights"
    book_room_price = "book_room_price"
    book_room_city = "book_room_city"

    book_ticket = "book_ticket"
    book_ticket_number = "book_ticket_number"
    book_ticket_departure = "book_ticket_departure"
    book_ticket_to = "book_ticket_to"
    book_ticket_price = "book_ticket_price"


system_templates = {
    functions.root_concern: "How can I help you",
    functions.order_taxi_location: "Where would you like the taxi sent to",
    functions.book_room_number: "How many rooms would you like to reserve",
    functions.book_room_nights: "How many nights will you be staying for",
    functions.book_room_price: "In what price range will you be reserving",
    functions.book_room_city: "In which city would you like to have your rooms",
    functions.book_ticket_number: "How many tickets would you like to reserve",
    functions.book_ticket_departure: "Where will you be traveling from",
    functions.book_ticket_to: "Where will you be traveling to",
    functions.book_ticket_price: "In what price range will you be reserving tickets"
    }

templates = [
    # order-taxi
    ["can you send me a taxi", [functions.order_taxi], None],
    ["can you send me a taxi to {order_taxi_location}", [functions.order_taxi], [functions.order_taxi_location]],
    ["i would like to order a cab", [functions.order_taxi], None],
    ["can you send me a cab to {order_taxi_location}", [functions.order_taxi], [functions.order_taxi_location]],
    ["can you send me a taxi", [functions.order_taxi], None],
    ["i need a taxi", [functions.order_taxi], None],
    ["can i have a taxi at {order_taxi_location}", [functions.order_taxi], [functions.order_taxi_location]],
    ["i'd like to order a taxi", [functions.order_taxi], None],
    ["i'd like to order a taxi to go to {order_taxi_location}", [functions.order_taxi], [functions.order_taxi_location]],
    ["i'd like to order a cab to go to {order_taxi_location}", [functions.order_taxi], [functions.order_taxi_location]],
    ["can you order a taxi for me", [functions.order_taxi], None],

    ["send it to {order_taxi_location}", None, [functions.order_taxi_location]],
    ["to {order_taxi_location}", None, [functions.order_taxi_location]],
    ["i'll have it at {order_taxi_location}", None, [functions.order_taxi_location]],
    ["can you send it to {order_taxi_location}", None, [functions.order_taxi_location]],
    ["i'm at {order_taxi_location}", None, [functions.order_taxi_location]],

    # book-room
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]}", [functions.book_room], [functions.book_room_number]],
    ["i'd like to book rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_number, functions.book_room_nights]],
    ["i'd like to book rooms in {book_room_city}", [functions.book_room], [functions.book_room_city]],

    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_price]],
    ["i'd like to book rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["i'd like to book {book_room_price}ly priced rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_price, functions.book_room_nights]],
    ["i'd like to book for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_price]],
    ["i'd like to book rooms in {book_room_city} in a {book_room_price} price range", [functions.book_room], [functions.book_room_city, functions.book_room_price]],

    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],
    ["i'd like to book rooms in {book_room_city} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_city, functions.book_room_nights]],
    ["i'd like to book rooms for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_nights, functions.book_room_city]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_city]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],
    
    ["can i book {book_room_number[0]} room{book_room_number[1]}", [functions.book_room], [functions.book_room_number]],
    ["can i book rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["can i book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_number, functions.book_room_nights]],
    ["can i book rooms in {book_room_city}", [functions.book_room], [functions.book_room_city]],

    ["can i book {book_room_number[0]} room{book_room_number[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_price]],
    ["can i book rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["can i book {book_room_price}ly priced rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_price, functions.book_room_nights]],
    ["can i book for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
    ["can i book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_price]],
    ["can i book rooms in {book_room_city} in a {book_room_price} price range", [functions.book_room], [functions.book_room_city, functions.book_room_price]],

    ["can i book {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],
    ["can i book rooms in {book_room_city} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_city, functions.book_room_nights]],
    ["can i book rooms for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_nights, functions.book_room_city]],
    ["can i book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_city]],
    ["can i book {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],

    ["i'd like to reserve {book_room_number[0]} room{book_room_number[1]}", [functions.book_room], [functions.book_room_number]],
    ["i'd like to reserve rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["i'd like to reserve {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_number, functions.book_room_nights]],
    ["i'd like to reserve rooms in {book_room_city}", [functions.book_room], [functions.book_room_city]],

    ["i'd like to reserve {book_room_number[0]} room{book_room_number[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_price]],
    ["i'd like to reserve rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["i'd like to reserve {book_room_price}ly priced rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_price, functions.book_room_nights]],
    ["i'd like to reserve for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
    ["i'd like to reserve {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_price]],
    ["i'd like to reserve rooms in {book_room_city} in a {book_room_price} price range", [functions.book_room], [functions.book_room_city, functions.book_room_price]],

    ["i'd like to reserve {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],
    ["i'd like to reserve rooms in {book_room_city} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_city, functions.book_room_nights]],
    ["i'd like to reserve rooms for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_nights, functions.book_room_city]],
    ["i'd like to reserve {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_city]],
    ["i'd like to reserve {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],

    ["can you reserve {book_room_number[0]} room{book_room_number[1]}", [functions.book_room], [functions.book_room_number]],
    ["can you reserve rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["can you reserve {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_number, functions.book_room_nights]],
    ["can you reserve rooms in {book_room_city}", [functions.book_room], [functions.book_room_city]],

    ["can you reserve {book_room_number[0]} room{book_room_number[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_price]],
    ["can you reserve rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["can you reserve {book_room_price}ly priced rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_price, functions.book_room_nights]],
    ["can you reserve for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
    ["can you reserve {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_price]],
    ["can you reserve rooms in {book_room_city} in a {book_room_price} price range", [functions.book_room], [functions.book_room_city, functions.book_room_price]],

    ["can you reserve {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],
    ["can you reserve rooms in {book_room_city} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_city, functions.book_room_nights]],
    ["can you reserve rooms for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_nights, functions.book_room_city]],
    ["can you reserve {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_city]],
    ["can you reserve {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],

    ["{book_room_number[0]} room{book_room_number[1]}", None, [functions.book_room_number]],
    ["i'll take {book_room_number[0]} room{book_room_number[1]}", None, [functions.book_room_number]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]}", None, [functions.book_room_number]],
    ["i'll take {book_room_number[0]}", None, [functions.book_room_number]],
    ["i'll be needing {book_room_number[0]} room{book_room_number[1]}", None, [functions.book_room_number]],
    ["i'll be needing {book_room_number[0]}", None, [functions.book_room_number]],
    ["can i have {book_room_number[0]} room{book_room_number[1]}", None, [functions.book_room_number]],
    ["can i reserve {book_room_number[0]} room{book_room_number[1]}", None, [functions.book_room_number]],
    ["i'll be reserving {book_room_number[0]} room{book_room_number[1]}", None, [functions.book_room_number]],

    ["in {book_room_city}", None, [functions.book_room_city]],
    ["i'd like them in {book_room_city}", None, [functions.book_room_city]],
    ["i'll be staying in {book_room_city}", None, [functions.book_room_city]],

    ["for {book_room_nights[0]} night{book_room_nights[1]}", None, [functions.book_room_nights]],
    ["i'll be needing them for {book_room_nights[0]} night{book_room_nights[1]}", None, [functions.book_room_nights]],
    ["{book_room_nights[0]}", None, [functions.book_room_nights]],

    ["i'll take {book_room_price}", None, [functions.book_room_price]],
    ["{book_room_price} price range", None, [functions.book_room_price]],
    ["i'm interested in {book_room_price} rooms", None, [functions.book_room_price]],

    # book_ticket
    ["i'd like to reserve tickets", [functions.book_ticket], None],
    ["i'd like to book ticket", [functions.book_ticket], None],
    ["can you reserve ticket for me", [functions.book_ticket], None],
    ["can you book tickets", [functions.book_ticket], None],
    
    ["i'd like to reserve {book_ticket_number[0]} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number]],
    ["i'd like to reserve tickets for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["i'd like to reserve {book_ticket_price} tickets from {book_ticket_departure} to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_price, functions.book_ticket_departure, functions.book_ticket_to]],
    ["i'd like to reserve {book_ticket_number[0]} ticket{book_ticket_number[1]} leaving {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_departure]],
    ["i'd like to reserve {book_ticket_number[0]} {book_ticket_price} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_price]],
    ["i'd like to reserve {book_ticket_number[0]} ticket{book_ticket_number[1]} for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_to]],
    ["i'd like to reserve tickets in a {book_ticket_price} price range", [functions.book_ticket], [functions.book_ticket_price]],
    ["i'd like to reserve tickets for {book_ticket_to} leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_to, functions.book_ticket_departure]],
    ["i'd like to reserve tickets to go to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["i'd like to reserve tickets leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_departure]],

    ["i'd like to book {book_ticket_number[0]} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number]],
    ["i'd like to book tickets for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["i'd like to book {book_ticket_price} tickets from {book_ticket_departure} to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_price, functions.book_ticket_departure, functions.book_ticket_to]],
    ["i'd like to book {book_ticket_number[0]} ticket{book_ticket_number[1]} leaving {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_departure]],
    ["i'd like to book {book_ticket_number[0]} {book_ticket_price} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_price]],
    ["i'd like to book {book_ticket_number[0]} ticket{book_ticket_number[1]} for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_to]],
    ["i'd like to book tickets in a {book_ticket_price} price range", [functions.book_ticket], [functions.book_ticket_price]],
    ["i'd like to book tickets for {book_ticket_to} leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_to, functions.book_ticket_departure]],
    ["i'd like to book tickets to go to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["i'd like to book tickets leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_departure]],

    ["can you reserve reserve {book_ticket_number[0]} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number]],
    ["can you reserve tickets for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["can you reserve {book_ticket_price} tickets from {book_ticket_departure} to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_price, functions.book_ticket_departure, functions.book_ticket_to]],
    ["can you reserve {book_ticket_number[0]} ticket{book_ticket_number[1]} leaving {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_departure]],
    ["can you reserve {book_ticket_number[0]} {book_ticket_price} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_price]],
    ["can you reserve {book_ticket_number[0]} ticket{book_ticket_number[1]} for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_to]],
    ["can you reserve tickets in a {book_ticket_price} price range", [functions.book_ticket], [functions.book_ticket_price]],
    ["can you reserve tickets for {book_ticket_to} leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_to, functions.book_ticket_departure]],
    ["can you reserve tickets to go to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["can you reserve tickets leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_departure]],

    ["can i book {book_ticket_number[0]} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number]],
    ["can i book tickets for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["can i book {book_ticket_price} tickets from {book_ticket_departure} to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_price, functions.book_ticket_departure, functions.book_ticket_to]],
    ["can i book {book_ticket_number[0]} ticket{book_ticket_number[1]} leaving {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_departure]],
    ["can i book {book_ticket_number[0]} {book_ticket_price} ticket{book_ticket_number[1]}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_price]],
    ["can i book {book_ticket_number[0]} ticket{book_ticket_number[1]} for {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_number, functions.book_ticket_to]],
    ["can i book tickets in a {book_ticket_price} price range", [functions.book_ticket], [functions.book_ticket_price]],
    ["can i book tickets for {book_ticket_to} leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_to, functions.book_ticket_departure]],
    ["can i book tickets to go to {book_ticket_to}", [functions.book_ticket], [functions.book_ticket_to]],
    ["can i book tickets leaving from {book_ticket_departure}", [functions.book_ticket], [functions.book_ticket_departure]],

    ["{book_ticket_number[0]} ticket{book_ticket_number[1]}", None, [functions.book_ticket_number]],
    ["i'll take {book_ticket_number[0]} ticket{book_ticket_number[1]}", None, [functions.book_ticket_number]],
    ["i'd like to book {book_ticket_number[0]} ticket{book_ticket_number[1]}", None, [functions.book_ticket_number]],
    ["i'll take {book_ticket_number[0]}", None, [functions.book_ticket_number]],
    ["i'll be needing {book_ticket_number[0]} ticket{book_ticket_number[1]}", None, [functions.book_ticket_number]],
    ["i'll be needing {book_ticket_number[0]}", None, [functions.book_ticket_number]],
    ["can i have {book_ticket_number[0]} ticket{book_ticket_number[1]}", None, [functions.book_ticket_number]],
    ["can i reserve {book_ticket_number[0]} ticket{book_ticket_number[1]}", None, [functions.book_ticket_number]],
    ["i'll be reserving {book_ticket_number[0]} ticket{book_ticket_number[1]}", None, [functions.book_ticket_number]],

    ["leaving {book_ticket_departure}", None, [functions.book_ticket_departure]],
    ["i'll be needing them in {book_ticket_departure}", None, [functions.book_ticket_departure]],
    ["i'll be leaving from {book_ticket_departure}", None, [functions.book_ticket_departure]],
    ["from {book_ticket_departure}", None, [functions.book_ticket_departure]],

    ["headed to {book_ticket_to}", None, [functions.book_ticket_to]],
    ["to {book_ticket_to}", None, [functions.book_ticket_to]],
    ["i'm going to {book_ticket_to}", None, [functions.book_ticket_to]],
    ["i'll be needing them for {book_ticket_to}", None, [functions.book_ticket_to]],

    ["i'll take {book_ticket_price}", None, [functions.book_ticket_price]],
    ["{book_ticket_price} price range", None, [functions.book_ticket_price]],
    ["i'm interested in {book_ticket_price} tickets", None, [functions.book_ticket_price]],

]

counts = [{0: "two", 1: "s"}, {0: "four", 1: "s"}, {0: "six", 1: "s"}, {0: "eight", 1: "s"}]
# counts = [{0: "one", 1: ""}, {0: "two", 1: "s"}, {0: "three", 1: "s"}, {0: "four", 1: "s"}, {0: "five", 1: "s"}, {0: "six", 1: "s"}, {0: "seven", 1: "s"}, {0: "eight", 1: "s"}]
cities = ['paris', 'madrid', 'bombay', 'london', 'rome']
price = ["cheap", "moderate", "expensive"]
values = {
    functions.book_room_city: cities,
    functions.book_room_nights: counts,
    functions.book_room_number: counts,
    functions.book_room_price: price,
    functions.order_taxi_location: cities,
    functions.book_ticket_number: counts,
    functions.book_ticket_departure: cities,
    functions.book_ticket_to: cities,
    functions.book_ticket_price: price,
}

cities = ["hanoi", "beijing", "bangkok", "seoul", "tokyo"]
values_OOV = values.copy()
values_OOV[functions.book_room_city] = cities
values_OOV[functions.order_taxi_location] = cities
values_OOV[functions.book_ticket_departure] = cities
values_OOV[functions.book_ticket_to] = cities

function_groups = [
    [functions.book_room, [functions.book_room_city,
                           functions.book_room_nights,
                           functions.book_room_number,
                           functions.book_room_price]],
    [functions.order_taxi, [functions.order_taxi_location]],
    [functions.book_ticket, [functions.book_ticket_number,
                             functions.book_ticket_departure,
                             functions.book_ticket_to,
                             functions.book_ticket_price]]]
