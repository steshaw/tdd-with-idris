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
  Lost : (game : GameState Z (S letters)) -> Finished
  Won  : (game : GameState (S guesses) Z) -> Finished

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
  putStr "Player 2. Enter a guess: "
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

dashes : (st : GameState (S guesses) (S letters)) -> String
dashes (MkGameState word missing) =
  pack $ intersperse ' ' $ map dashEm (unpack word)
  where
    dashEm : Char -> Char
    dashEm c = case Data.Vect.find (== (toUpper c)) missing of
                    Nothing => c
                    Just _  => '_'

hangman : Nat -> String
hangman (S (S (S (S (S (S Z)))))) = """
  +---------+
  |         |
  |
  |
  |
  |
  |
--+--
"""
hangman (S (S (S (S (S Z))))) = """
  +---------+
  |         |
  |         O
  |
  |
  |
  |
-----
"""
hangman (S (S (S (S Z)))) = """
  +---------+
  |         |
  |         O
  |         |
  |
  |
  |
--+--
"""
hangman (S (S (S Z))) = """
  +---------+
  |         |
  |         O
  |        /|
  |
  |
  |
--+--
"""
hangman (S (S Z)) = """
  +---------+
  |         |
  |         O
  |        /|\
  |
  |
  |
--+--
"""
hangman (S Z) = """
  +---------+
  |         |
  |         O
  |        /|\
  |        /
  |
  |
--+--
"""
hangman Z = """
  +---------+
  |         |
  |         O
  |        /|\
  |        / \
  |
  |
--+--
"""

game : GameState (S guesses) (S letters) -> IO Finished
game st {guesses} {letters} = do
  putStrLn (hangman (S guesses))
  putStrLn ""
  putStrLn (dashes st)
  putStrLn ""
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

isWord : String -> Bool
isWord s = all isAlpha (unpack s)

main : IO ()
main = do
  putStr "Player 1. Enter your word: "
  word <- getLine
  if isWord word then pure () else do {putStrLn "Invalid word"; main}
  let missing = nub (sort (unpack (toUpper word)))
  case missing of
       [] => do {putStrLn "Invalid word"; main}
       (c :: cs) => do putStr $ concat $ take 100 $ repeat "\n"
                       result <- game {guesses = 5} (MkGameState word (c :: fromList cs))
                       case result of
                            Lost (MkGameState word missing) => do putStrLn $ hangman Z
                                                                  putStrLn $ "You lose! The word was '" ++ word ++ "'"
                            Won (MkGameState word missing)  => putStrLn $ "You win! The word was '" ++ word ++ "'"
