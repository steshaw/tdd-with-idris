import Data.Vect

data GameState : (guesses : Nat) -> (letters : Nat) -> Type where
     MkGameState : (word : String)
                   -> (missing : Vect letters Char)
                   -> GameState guesses_remaining letters

data Finished : Type where
     Lost : (game : GameState 0 (S letters)) -> Finished
     Won  : (game : GameState (S guesses) 0) -> Finished

total
removeElem : (value : a) -> (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (y :: ys) {prf = There later}
                                          = y :: removeElem value ys

processGuess : (letter : Char) -> GameState (S guesses) (S letters) ->
                Either (GameState guesses (S letters))
                       (GameState (S guesses) letters)
processGuess letter (MkGameState word missing)
    = case isElem letter missing of
           Yes prf => Right (MkGameState word (removeElem letter missing))
           No contra => Left (MkGameState word missing)

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

isValidNil : ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidTwo : ValidInput (x :: (y :: xs)) -> Void
isValidTwo (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No isValidNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No isValidTwo

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess: "
               x <- getLine
               case isValidString (toUpper x) of
                    Yes prf => pure (_ ** prf)
                    No contra => do putStrLn "Invalid guess"
                                    readGuess

game : GameState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st
        = do (_ ** Letter letter) <- readGuess
             case processGuess letter st of
                  Left l => do putStrLn "Wrong!"
                               case guesses of
                                    Z => pure (Lost l)
                                    S k => game l
                  Right r => do putStrLn "Right!"
                                case letters of
                                     Z => pure (Won r)
                                     S k => game r

main : IO ()
main = do game {guesses=4} (MkGameState "" ['F', 'O'])
          pure ()
