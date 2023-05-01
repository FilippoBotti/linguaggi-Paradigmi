import Data.Maybe

type Name = String 
type PhoneNumber = String
type PhoneBook = [(Name,PhoneNumber)]


getPhoneNumber :: Name -> PhoneBook -> Maybe PhoneNumber
getPhoneNumber name [] = Nothing
getPhoneNumber name ((k,v):xs) = if name == k then Just v else getPhoneNumber name xs

getPhoneNumber' name xs = foldl (\acc x -> if fst(x)==acc then Just second(x) else acc) " " xs