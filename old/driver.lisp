(load "functions.lisp")
(defparameter *document*
  '(;;("How may i help you")
    ("Can i book a train" "book-train" "root-concern")
    ;;("I would like to know you name")
    ("It's ahmed shariff" "nil" "book-train-name")
    ;;("may i know from where you are departing")
    ("from kandy" "nil" "book-train-departure")
    ;;("Where would you be traveling to")
    ("to colombo" "nil" "book-train-destination")
    ;;("can i know how many tickets you would like to book")
    ("5 tickets please" "nil" "book-train-number-of-tickets")
    
    ;;("How may i help you")
    ("I would like to book a ticket" "book-train" "root-concern")
    ;;("I would like to know you name")
    ("my name is pual aron" "nil" "book-train-name")
    ;;("may i know from where you are departing")
    ("i would be leaving from colombo" "nil" "book-train-departure")
    ;;("Where would you be traveling to")
    ("i am going to kandy" "nil" "book-train-destination")
    ;;("can i know how many tickets you would like to book")
    ("just 1 thank you" "nil" "book-train-number-of-tickets")

    ;;("How may i help you")
    ("i want to book a ticket" "book-train" "root-concern")
    ;;("I would like to know you name")
    ("diago luna" "nil" "book-train-name")
    ;;("may i know from where you are departing")
    ("from dhambulla" "nil" "book-train-departure")
    ;;("Where would you be traveling to")
    ("to jafna" "nil" "book-train-destination")
    ;;("can i know how many tickets you would like to book")
    ("2 tickets please" "nil" "book-train-number-of-tickets")

    ;;("How may i help you")
    ("can you please help me to book a train" "book-train" "root-concern")
    ;;("I would like to know you name")
    ("i am bhashithe abesinghe" "nil" "book-train-name")
    ;;("may i know from where you are departing")
    ("colombo fort" "nil" "book-train-departure")
    ;;("Where would you be traveling to")
    ("i am getting off from maradhana" "nil" "book-train-destination")
    ;;("can i know how many tickets you would like to book")
    ("three" "nil" "book-train-number-of-tickets")

    ;;("How may i help you")
    ("i need to book a train" "book-train" "root-concern")
    ;;("I would like to know you name")
    ("my name is roshan mahanama" "nil" "book-train-name")
    ;;("may i know from where you are departing")
    ("from dhambulla" "nil" "book-train-departure")
    ;;("Where would you be traveling to")
    ("colombo" "nil" "book-train-destination")
    ;;("can i know how many tickets you would like to book")
    ("just one ticket" "nil" "book-train-number-of-tickets")

    
    ;;"How may i help you"
    ("I'd like to order a taxi" "order-taxi" "root-concern")
    ;;may i know where you want the taxi at?
    ("at kandy" "nil" "order-taxi-location")

    ;;"How may i help you"
    ("can you send me a taxi" "order-taxi" "root-concern")
    ;;may i know where you want the taxi at?
    ("send it to colombo" "nil" "order-taxi-location")

    ;;"How may i help you"
    ("i need a cab" "order-taxi" "root-concern")
    ;;may i know where you want the taxi at?
    ("to maradhana" "nil" "order-taxi-location")

    ;;"How may i help you"
    ("i want a cab" "order-taxi" "root-concern")
    ;;may i know where you want the taxi at?
    ("i'll have it at dhambulla" "nil" "order-taxi-location")

    ;;How can i help you
    ("I'd like to know the train schedules" "get-train-info" "root-concern")
    ;;can i know from where you want to get the train
    ("I want to go from colombo" "nil" "get-train-info-departure")
    ;;where would you be going to?
    ("to kandy" "nil" "get-train-info-destination")

    ;;How can i help you
    ("I want to know the train times" "get-train-info" "root-concern")
    ;;can i know from where you want to get the train
    ("I would be leaving from dhambulla" "nil" "get-train-info-departure")
    ;;where would you be going to?
    ("I am going to maradhana" "nil" "get-train-info-destination")

    ;;How can i help you
    ("I want to get the train shedules" "get-train-info" "root-concern")
    ;;can i know from where you want to get the train
    ("moving from maradhana" "nil" "get-train-info-destination")
    ;;where would you be going to?
    ("I want to know the times of the trains to kandy" "nil" "get-train-info-destination")

    ;;How can i help you
    ("I want to know the shedules of the train nleaving from kandy" "get-train-info" "root-concern")
    ;;can i know from where you want to get the train
    ("from kandy" "nil" "get-train-info-destination")
    ;;where would you be going to?
    ("I am looking for trains that leave to colombo" "nil" "get-train-info-destination")

    ;;How can i help you
    ("can i find about the trains departing time" "get-train-info" "root-concern")
    ;;can i know from where you want to get the train
    ("fomr dhambulla" "nil" "get-train-info-destination")
    ;;where would you be going to?
    ("kandy" "nil" "get-train-info-destination")
    

    ;;How can i help you this evening
    ("I want to order some food please" "order-food" "root-concern")
    ;;"what would you like to have"
    ("I would like to have pizza" "nil" "order-food-food-type")
    ;;How many would you like to order
    ("Send us three pizza's  please" "nil" "order-food-number-of-orders")

    ;;How can i help you this evening
    ("I want some food" "order-food" "root-concern")
    ;;"what would you like to have"
    ("Can i have some rotti" "nil" "order-food-food-type")
    ;;How many would you like to order
    ("I'll take one" "nil" "order-food-number-of-orders")

    ;;How can i help you this evening
    ("I would like to buy food" "order-food" "root-concern")
    ;;"what would you like to have"
    ("give me some bread" "nil" "order-food-food-type")
    ;;How many would you like to order
    ("two please" "nil" "order-food-number-of-orders")

    ;;How can i help you this evening
    ("buy some food" "order-food" "root-concern")
    ;;"what would you like to have"
    ("give me a bun" "nil" "order-food-food-type")
    ;;How many would you like to order
    ("i need just one" "nil" "order-food-number-of-orders")

    ;;How can i help you this evening
    ("i am looking to order some food" "order-food" "root-concern")
    ;;"what would you like to have"
    ("a piece of cacke will do" "nil" "order-food-food-type")
    ;;How many would you like to order
    ("one" "nil" "order-food-number-of-orders")



    ;;how can i help you?
    ("I want to signup" "signup-online-account" "root-concern")
    ;;May i know your name
    ("It's chanaka" "nil" "signup-online-account-name")
    ;;"how old are you"
    ("I'm twnty five" "nil" "signup-online-age")
    ;;can i know your emaila ddress
    ("chanaka dot gunasekara at live dot com" "nil" "signup-online-account-email-address")

    ;;how can i help you?
    ("I want to create an online account" "signup-online-account" "root-concern")
    ;;May i know your name
    ("i am sahan" "nil" "signup-online-account-name")
    ;;"how old are you"
    ("i am twnty six years old" "nil" "signup-online-age")
    ;;can i know your emaila ddress
    ("it's sahan f fernando at gmail dot com" "nil" "signup-online-account-email-address")

    ;;how can i help you?
    ("Can i signup for an account online" "signup-online-account" "root-concern")
    ;;May i know your name
    ("my name is kshare" "nil" "signup-online-account-name")
    ;;"how old are you"
    ("I'm twnty three" "nil" "signup-online-age")
    ;;can i know your emaila ddress
    ("kshare at hotmail dot com" "nil" "signup-online-account-email-address")

    ;;how can i help you?
    ("to create an online account" "signup-online-account" "root-concern")
    ;;May i know your name
    ("roberta" "nil" "signup-online-account-name")
    ;;"how old are you"
    ("i am twenty one" "nil" "signup-online-age")
    ;;can i know your emaila ddress
    ("roberta underscore five five five at ymail dot com" "nil" "signup-online-account-email-address")

    ;;how can i help you?
    ("open an onilne account" "signup-online-account" "root-concern")
    ;;May i know your name
    ("mahanama" "nil" "signup-online-account-name")
    ;;"how old are you"
    ("i am fifty two" "nil" "signup-online-age")
    ;;can i know your emaila ddress
    ("mhana one two three at live dot com" "nil" "signup-online-account-email-address")


    ;;"how can i help you?"
    ("I would like to book a room please" "book-room" "root-concern")
    ;;"May i know your name"
    ("I am Shariff" "nil" "book-room-name")
    ;;"how many nights will you be staying for"
    ("For three days" "nil" "book-room-number-of-days")
    ;;"how many rooms are you going to book"
    ("i only need one room" "nil" "book-room-number-of-rooms")

    ;;"how can i help you?"
    ("I want a room please" "book-room" "root-concern")
    ;;"May i know your name"
    ("It's bhashithe" "nil" "book-room-name")
    ;;"how many nights will you be staying for"
    ("For one night" "nil" "book-room-number-of-days")
    ;;"how many rooms are you going to book"
    ("I need two" "nil" "book-room-number-of-rooms")

    ;;"how can i help you?"
    ("i want to book a few room" "book-room" "root-concern")
    ;;"May i know your name"
    ("My name is rankan" "nil" "book-room-name")
    ;;"how many nights will you be staying for"
    ("for one night" "nil" "book-room-number-of-days")
    ;;"how many rooms are you going to book"
    ("i need three rooms" "nil" "book-room-number-of-rooms")

    ;;"how can i help you?"
    ("I need to book a room" "book-room" "root-concern")
    ;;"May i know your name"
    ("I am warren" "nil" "book-room-name")
    ;;"how many nights will you be staying for"
    ("i am staying for one day" "nil" "book-room-number-of-days")
    ;;"how many rooms are you going to book"
    ("i'll take one room" "nil" "book-room-number-of-rooms")

    ;;"how can i help you?"
    ("i am loking for a roomI need to book a room" "book-room" "root-concern")
    ;;"May i know your name"
    ("it's mahanama" "nil" "book-room-name")
    ;;"how many nights will you be staying for"
    ("just one night" "nil" "book-room-number-of-days")
    ;;"how many rooms are you going to book"
    ("that also would be one" "nil" "book-room-number-of-rooms")))
    

(dolist (doc *document*)
  (apply #'add-utterence doc))

(train-resolving-fn *fn-list*)
(train-triggering-fn *fn-list*)
  
