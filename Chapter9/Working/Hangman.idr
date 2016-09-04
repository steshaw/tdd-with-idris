import Data.Vect

removeElem1 : (value : a) ->
              (xs : Vect (S n) a) ->
              (prf: Elem value xs) ->
              Vect n a
removeElem1 value (value :: ys) Here = ys
removeElem1 value (y :: []) (There later) {n = Z} = absurd later
removeElem1 value (y :: ys) (There later) {n = (S k)} = y :: removeElem1 value ys later

removeElem : (value : a) ->
             (xs : Vect (S n) a) ->
             {auto prf: Elem value xs} ->
             Vect n a
removeElem value xs {prf} = removeElem1 value xs prf

data GameState : (guessesRemaining : Nat) -> (letters : Nat) -> Type where
  MkGameState : (word : String) ->
                (missing : Vect letters Char) ->
                GameState guessesRemaining letters

data Finished : Type where
  Lost : (game : GameState 0 (S letters)) -> Finished
  Won  : (game : GameState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]

isValidInputNil : ValidInput [] -> Void
isValidInputNil (Letter _) impossible

notValidLenTwoOrMore : ValidInput (x :: (y :: xs)) -> Void
notValidLenTwoOrMore (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No isValidInputNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No notValidLenTwoOrMore

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do
  putStr "Guess: "
  s <- getLine
  case isValidString (toUpper s) of
       Yes prf => pure (_ ** prf)
       No contra => do putStrLn "Invalid guess"
                       readGuess

processGuess : (letter : Char) ->
               GameState (S guesses) (S letters) ->
               Either (GameState guesses (S letters))
                      (GameState (S guesses) letters)
processGuess letter (MkGameState word missing) =
  case isElem letter missing of
    Yes prf   => Right (MkGameState word (removeElem letter missing))
    No contra => Left (MkGameState word missing)

game : GameState (S guesses) (S letters) -> IO Finished
game st {guesses} {letters} = do
  (_ ** Letter letter) <- readGuess
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
main = do
  result <- game {guesses=2} (MkGameState "Test" ['T', 'E', 'S'])
  case result of
       Lost (MkGameState word missing) => putStrLn $ "You lose! The word was '" ++ word ++ "'"
       Won (MkGameState word missing)  => putStrLn $ "You win! The word was '" ++ word ++ "'"
