module Hello (
    sayHello,
    greetPerson
) where

sayHello :: String
sayHello = "Hello from module!"

greetPerson :: String -> String
greetPerson name = "Hello, " ++ name ++ "!"
