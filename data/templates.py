from easydict import EasyDict


def order_taxi(location):
    if location is not None:
        print(f"ordered taxi to {location}")
    raise NotImplementedError()


def book_room(number, nights, price, city):
    if all([i is not None for i in [number, nights, price, city]]):
        print(f"Booked room for {number} for {nights} nights in a {price} price-range in {city}")
    raise NotImplementedError()


def book_ticket(from, to, price):
    if all([i is not None for i in [from, to, price]]):
        print(f"Booked ticket from {from} to {to} in a {price} price-range")
    raise NotImplementedError()


class functions(EasyDict):
    plurality = "plurality"

    order_taxi = "order_taxi"
    order_taxi_location = "order_taxi_location"

    book_room = "book_room"
    book_room_number = "book_room_number"
    book_room_nights = "book_room_nights"
    book_room_price = "book_room_price"
    book_room_city = "book_room_city"

    book_ticket = "book_ticket"
    book_ticket_from = "book_ticket_from"
    book_ticket_to = "book_ticket_to"
    book_ticket_price = "book_ticket_price"


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
    ["i'd like to book for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range}", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_price]],
    ["i'd like to book rooms in {book_room_city} in a {book_room_price} price range", [functions.book_room], [functions.book_room_city, functions.book_room_price]],

    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],
    ["i'd like to book rooms in {book_room_city} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_city, functions.book_room_nights]],
    ["i'd like to book rooms for {book_room_nights[0]} night{book_room_nights[1]}} in {book_room_city}", [functions.book_room], [functions.book_room_nights, functions.book_room_city]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_nights, functions.book_room_city]],
    ["i'd like to book {book_room_number[0]} room{book_room_number[1]} in {book_room_city}", [functions.book_room], [functions.book_room_number, functions.book_room_city]],
    
    ["can i book {book_room_number[0]} room{book_room_number[1]}", [functions.book_room], [functions.book_room_number]],
    ["can i book rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["can i book {book_room_number[0]} room{book_room_number[1]} for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_number, functions.book_room_nights]],
    ["can i book rooms in {book_room_city}", [functions.book_room], [functions.book_room_city]],

    ["can i book {book_room_number[0]} room{book_room_number[1]} in a {book_room_price} price range", [functions.book_room], [functions.book_room_number, functions.book_room_price]],
    ["can i book rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_nights]],
    ["can i book {book_room_price}ly priced rooms for {book_room_nights[0]} night{book_room_nights[1]}", [functions.book_room], [functions.book_room_price, functions.book_room_nights]],
    ["can i book for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range}", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
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
    ["i'd like to reserve for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range}", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
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
    ["can you reserve for {book_room_nights[0]} night{book_room_nights[1]} in a {book_room_price} price range}", [functions.book_room], [functions.book_room_nights, functions.book_room_price]],
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
]

# first_utterance = re.compile(r".*(book a table|restaurant reservation|have a table).*")
# cuisine = re.compile(r".*(spanish|british|french|italian|indian).*")
# count = re.compile(r".*(one|two|three|four|five|six|seven|eight|nine).*")
# price = re.compile(r".*(cheap|moderate|expensive).*")
# city = re.compile(r".*(paris|madrid|bombay|london|rome).*")

values = {
    functions.book_room_city: ['paris', 'madrid', 'bombay', 'london', 'rome'],
    functions.book_room_nights: [{0:"one", 1:""}, {0:"two", 1:"s"}, {0:"three", 1:"s"}, {0:"four", 1:"s"}, {0:"five", 1:"s"}, {0:"six", 1:"s"}, {0:"seven", 1:"s"}],
    functions.book_room_number: [{0:"one", 1:""}, {0:"two", 1:"s"}, {0:"three", 1:"s"}, {0:"four", 1:"s"}, {0:"five", 1:"s"}, {0:"six", 1:"s"}, {0:"seven", 1:"s"}],
    functions.book_room_price: ["cheap", "moderate", "expensive"]
}

function_groups = [
    [functions.book_room, [functions.book_room_city,
                           functions.book_room_nights,
                           functions.book_room_number,
                           functions.book_room_price]],
    [functions.order_taxi, [functions.order_taxi_location]],
    [functions.book_ticket, [functions.book_ticket_from,
                             functions.book_ticket_to,
                             functions.book_ticket_price]]]
