module Main where
import Data.Time (getCurrentTime)
import Control.Monad.Trans.State (StateT, get, execStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))

data Book = Book { code :: String, title :: String, author :: String, publisher :: String, year :: String } deriving (Show)

logIODir :: String
logIODir = "app/logIO.txt"

logDataDir :: String
logDataDir = "app/logData.txt"

bookTableDir :: String
bookTableDir = "app/bookTable.txt"

-- MAIN SYSTEM
program :: [Book] -> IO ()
program bookData  =  do
                        execStateT printState "Library Management System\n"
                        execStateT printState "=========================\n"
                        execStateT printState "1. Insert Book Data\n"
                        execStateT printState "2. Update Book Data\n"
                        execStateT printState "3. Delete Book Data\n"
                        execStateT printState "4. View Book Data\n"
                        execStateT printState "5. Quit\n"
                        execStateT printState "=========================\n"
                        execStateT printState "Choose Menu : \n"
                        fitur <- getLine
                        loggingIO $ fitur ++ "\n"
                        case fitur of
                            "1" -> insertBook bookData
                            "2" -> updateBook bookData
                            "3" -> deleteBook bookData
                            "4" -> viewBook bookData
                            "5" -> do
                                      execStateT printState "Thank you!\n"
                                      return ()
                            _   -> do
                                      execStateT printState "Feature not available!!!\n"
                                      program bookData

insertBook :: [Book] -> IO ()
insertBook book = do
                  execStateT printState "INSERT BOOK DATA\n"

                  execStateT printState "Enter book code : \n"
                  bookCode <- getLine
                  loggingIO $ bookCode ++ "\n"

                  execStateT printState "Enter book title : \n"
                  bookTitle <- getLine
                  loggingIO $ bookTitle ++ "\n"

                  execStateT printState "Enter book author : \n"
                  bookAuthor <- getLine
                  loggingIO $ bookTitle ++ "\n"

                  execStateT printState "Enter book publisher : \n"
                  bookPublisher <- getLine
                  loggingIO $ bookPublisher ++ "\n"

                  execStateT printState "Enter book year : \n"
                  bookYear <- getLine
                  loggingIO $ bookYear ++ "\n"

                  putStrLn "\n"
                  let inputData =  Book {
                                        code = bookCode,
                                        title = bookTitle,
                                        author = bookAuthor,
                                        publisher = bookPublisher,
                                        year = bookYear
                                    }
                      newBookData = book ++ [inputData]

                  iDetail "Insert Book Data" inputData

                  execStateT printState "Are you sure you want to add this book data? [Y/n]\n"
                  isInsert <- getLine
                  loggingIO $ isInsert ++ "\n"

                  case isInsert of
                        "Y" -> do
                                saveTableBook $ convertTableBook newBookData
                                loggingData "insert" (show inputData)
                                execStateT printState "Book data added successfully...\n"
                                program newBookData
                        _   -> do
                                execStateT printState "Cancel add book...\n"
                                program book

updateBook :: [Book] -> IO ()
updateBook book = do
                  execStateT printState "UPDATE BOOK DATA\n"

                  execStateT printState "Please type book code that you wanted to be updated :\n"
                  searchCode <- getLine
                  loggingIO $ searchCode ++ "\n"

                  let searching = iSearch searchCode 0 book
                  case searching of
                        [] -> do
                                execStateT printState "Book data not found. Please try again...\n"
                                updateBook book
                        _  -> do
                                let bookIndex = fst $ head searching
                                    bookCode = code (snd $ head searching)
                                    bookTitle = title (snd $ head searching)
                                    bookAuthor = author (snd $ head searching)
                                    bookPublisher = publisher (snd $ head searching)
                                    bookYear = year (snd $ head searching)

                                execStateT printState $ "Enter new book code [" ++ bookCode ++ "] (press Enter if no changes) : \n"
                                bookCode' <- getLine
                                loggingIO $ bookCode' ++ "\n"

                                execStateT printState $ "Enter new book title [" ++ bookTitle ++ "] (press Enter if no changes) : \n"
                                bookTitle' <- getLine
                                loggingIO $ bookTitle' ++ "\n"

                                execStateT printState $ "Enter new book author [" ++ bookAuthor ++ "] (press Enter if no changes) :  \n"
                                bookAuthor' <- getLine
                                loggingIO $ bookAuthor' ++ "\n"

                                execStateT printState $ "Enter new book publisher [" ++ bookPublisher ++ "] (press Enter if no changes) : \n"
                                bookPublisher' <- getLine
                                loggingIO $ bookPublisher' ++ "\n"

                                execStateT printState $ "Enter new book year [" ++ bookYear ++ "] (press Enter if no changes) : \n"
                                bookYear' <- getLine
                                loggingIO $ bookYear' ++ "\n"

                                let oldData =  Book {
                                                    code = bookCode,
                                                    title = bookTitle,
                                                    author = bookAuthor,
                                                    publisher = bookPublisher,
                                                    year = bookYear
                                                }
                                    newData =  Book {
                                                    code = bookCode',
                                                    title = bookTitle',
                                                    author = bookAuthor',
                                                    publisher = bookPublisher',
                                                    year = bookYear'
                                                }
                                    inputData =  Book {
                                                    code = vData bookCode bookCode',
                                                    title = vData bookTitle bookTitle',
                                                    author = vData bookAuthor bookAuthor',
                                                    publisher = vData bookPublisher bookPublisher',
                                                    year = vData bookYear bookYear'
                                                }
                                    newBookData = iUpdate bookIndex inputData book

                                iDetail "Update Book Data" inputData

                                execStateT printState "Are you sure you want to update this book data? [Y/n]\n"
                                isUpdate <- getLine
                                loggingIO $ isUpdate ++ "\n"

                                case isUpdate of
                                        "Y" -> do
                                                loggingData "update" (show oldData ++ " >>> " ++ show newData)
                                                saveTableBook $ convertTableBook newBookData
                                                execStateT printState "Book data updated successfully...\n"
                                                program newBookData
                                        _   -> do
                                                execStateT printState "Cancel update book...\n"
                                                program book

deleteBook :: [Book] -> IO ()
deleteBook book = do
                  execStateT printState "DELETE BOOK DATA\n"
                  execStateT printState "Please type book code that you wanted to be deleted : \n"
                  searchCode <- getLine
                  loggingIO $ searchCode ++ "\n"

                  let searching = iSearch searchCode 0 book
                  case searching of
                        [] -> do
                                execStateT printState "Book data not found. Please try again...\n"
                                deleteBook book
                        _  -> do
                                let bookIndex = fst $ head searching
                                    bookCode = code (snd $ head searching)
                                    bookTitle = title (snd $ head searching)
                                    bookAuthor = author (snd $ head searching)
                                    bookPublisher = publisher (snd $ head searching)
                                    bookYear = year (snd $ head searching)
                                    newBookData = iDelete bookIndex book

                                iDetail "Delete Book Data" (Book {code = bookCode, title = bookTitle, author = bookAuthor, publisher = bookPublisher, year = bookYear})

                                execStateT printState "Are you sure you want to delete this book data? [Y/n]\n"
                                isDelete <- getLine
                                loggingIO $ isDelete ++ "\n"

                                case isDelete of
                                        "Y" -> do
                                                loggingData "delete" (show $ snd $ head searching)
                                                saveTableBook $ convertTableBook newBookData
                                                execStateT printState "Book data deleted successfully...\n"
                                                program newBookData
                                        _   -> do
                                                execStateT printState "Cancel book deletion...\n"
                                                program book

viewBook :: [Book] -> IO ()
viewBook book = do
                let bookData = iView book 1
                    bookView = if bookData /= "" then bookData else "No book records found\n"
                loggingData "select" (show book)
                execStateT printState "List of Book\n"
                execStateT printStateNoTime "---------------------------------------------------------------------------------------------------------------------------------------------\n"
                execStateT printStateNoTime "  No.  |      Code       |                       Title                        |          Author           |         Publisher         | Year \n"
                execStateT printStateNoTime "---------------------------------------------------------------------------------------------------------------------------------------------\n"
                execStateT printStateNoTime bookView
                execStateT printStateNoTime "---------------------------------------------------------------------------------------------------------------------------------------------\n"
                program book

main :: IO()
main = do
       fileContent <- readFile bookTableDir
       let database = lines fileContent
           datalist = parseTableBook database
       loggingIO "Loading database to system...\n"
       if fileContent /= ""
       then do
           loggingIONoTime fileContent
           loggingData "load" (show datalist)
       else
           loggingIONoTime "No data\n"
       program datalist


-- HELPER FUNCTION
readTableBook :: (Char -> Bool) -> String -> [String]
readTableBook p s = case dropWhile p s of
                        "" -> []
                        s' -> w : readTableBook p s''
                              where (w, s'') = break p s'

parseTableBook :: [String] -> [Book]
parseTableBook [] = []
parseTableBook (x:xs) = do
                            let bData = readTableBook (=='|') x
                                bCode = head bData
                                bTitle = bData !! 1
                                bAuthor = bData !! 2
                                bPublisher = bData !! 3
                                bYear = bData !! 4
                            Book {code = bCode, title = bTitle, author = bAuthor, publisher = bPublisher, year = bYear} : parseTableBook xs

convertTableBook :: Foldable t => t Book -> [Char]
convertTableBook = foldr(\x -> (++)
                   (concat [code x, "|", title x, "|", author x, "|", publisher x, "|", year x, "\n"])) []

saveTableBook :: String -> IO ()
saveTableBook = writeFile bookTableDir

pSpace :: Int -> String
pSpace x = concat $ replicate x " "

pNo :: Int -> String
pNo value = show value ++ pSpace (5 - length (show value))

pBook :: Int -> (Book -> String) -> Book -> String
pBook width field table = field table ++ pSpace (width - length (field table))

vData :: String -> String -> String
vData x "" = x
vData x y = y

sCode :: String -> Int -> Book -> [(Int, Book)]
sCode x idx (Book {code = vCode, title = vTitle, author = vAuthor, publisher = vPublisher, year = vYear})
        | x == vCode = [(idx, Book {code = vCode, title = vTitle, author = vAuthor, publisher = vPublisher, year = vYear})]
        | otherwise = []

iDetail :: String -> Book -> IO()
iDetail header book = do
                        execStateT printState $ header ++ "\n"
                        execStateT printState "---------------------------------------------------------------------------\n"
                        execStateT printState $ "Book code : " ++ code book ++ "\n"
                        execStateT printState $ "Book title : " ++ title book ++ "\n"
                        execStateT printState $ "Book author : " ++ author book ++ "\n"
                        execStateT printState $ "Book publisher : " ++ publisher book ++ "\n"
                        execStateT printState $ "Book year : " ++ year book ++ "\n"
                        execStateT printState "---------------------------------------------------------------------------\n"
                        return ()

iSearch :: String -> Int -> [Book] -> [(Int, Book)]
iSearch _ _ [] = []
iSearch code idx (x:xs) = sCode code idx x ++ iSearch code (idx + 1) xs

iView :: [Book] -> Int -> String
iView [] _ = ""
iView (x:xs) y = concat [" ", pNo y, " | ", pBook 15 code x, " | ", pBook 50 title x, " | ", pBook 25 author x, " | ", pBook 25 publisher x, " | ", pBook 5 year x, "\n"] ++ iView xs (y+1)

iUpdate :: Int -> Book -> [Book] -> [Book]
iUpdate pos newVal list = take pos list ++ newVal : drop (pos+1) list

iDelete :: Int -> [a] -> [a]
iDelete _ [] = []
iDelete 0 (x:xs) = xs
iDelete n (x:xs) = x : iDelete (n-1) xs

loggingData :: String -> String -> IO ()
loggingData operation text = do
                                time <- fmap show getCurrentTime
                                appendFile logDataDir $ "[" ++ time ++ "] " ++ "[" ++ operation ++ "] "
                                appendFile logDataDir $ text ++ "\n"

loggingIO :: String -> IO ()
loggingIO text = do
                  time <- fmap show getCurrentTime
                  appendFile logIODir $ "[" ++ time ++ "] "
                  appendFile logIODir text

loggingIONoTime :: String -> IO ()
loggingIONoTime = appendFile logIODir

printState :: StateT String IO ()
printState = do
                state <- get
                lift $ loggingIO state
                lift $ putStr state

printStateNoTime :: StateT String IO ()
printStateNoTime = do
                state <- get
                lift $ loggingIONoTime state
                lift $ putStr state
