myFiles  = [("f1", "someContent"), ("f2", "very important"), ("f3", "don't know"), ("f4", "don't care")]

-- File :: File (name, content)
type File = (String, String)

fileName :: File -> String
fileName (name, _) = name

fileContent :: File -> String
fileContent (_,cont) = cont

data Directory = Empty| Node String [Directory] [File] deriving Show
myDir = Node "/" [] myFiles

dirName :: Directory -> String
dirName Empty = ""
dirName (Node name _ _ ) = name

subDir :: Directory -> [Directory]
subDir Empty = []
subDir (Node _ directories _) = directories

dirFiles :: Directory -> [File]
dirFiles Empty = []
dirFiles (Node _ _ files) = files

getDir name directories = get [d | d <- directories, name == dirName d]
                        where get dir
                                        |null dir = Empty
                                        |otherwise = head dir
                        
                        
                        
getDirOut name directories = [d | d <- directories, name /= dirName d]

getFile :: String -> Directory -> File
getFile fName (Node _ _ files) 
                                    | null list = ("","")
                                    | otherwise = (head list)
                                    where list = [f| f <- files, (fileName f) == fName]
                                    
getFilesWithout fName (Node _ _ files) = [f | f <- files, fName /= fileName f]


------------------------------------------------------------
                          ---EXAMPLE---
fileSystem :: Directory
fileSystem = (Node "/" 
                        [
                        (Node "dir1" 
                                   [] [("somefile" , "noCont")]), 
                        (Node "dir2" 
                                    [(Node "dir1" [(Node "dirF" [] [])] [("file", "SomeText")])]
                                    [("file" , "Yoanna")]),
                        (Node "dir3" [] [("f1", "ala") , ("f2" , "bala")])
                        ] 
                        
                        [("f1" , "Joanna"), ("f2", "Nikolaeva"), ("f3", "Nikolova")]
               )
 
--------------------------------------
-- smartSplit :: дели дели текст спрямо разделител, но всъщото време гледа дали не е попаднал в неразделен подтекст (Нещо в кавички)
-- Когато в недразеделен подтекст се срещне разделитея, то подтекста не се дели 
-- Example : smartSplit ' ' "Inf 45079 "Yoanna Nikolova"" -> ["Inf", "45079", "Yoanna Nikolova"]
-- smartSplit delimiter String
smartSplit :: Char -> String -> [String]
smartSplit _ [] = []
smartSplit  del str = mySplit False str []
                where
                        mySplit _ [] res = [(reverse res)]
                        mySplit inString (x:xs) res 
                                            |x == del && inString == False = (reverse res) : mySplit inString xs []
                                            |x == del && inString == True = mySplit inString xs (x:res)
                                            |x == '\"' && inString == False = mySplit True xs res
                                            |x == '\"' && inString == True = mySplit False xs res
                                            |otherwise = mySplit inString xs (x:res)
                                            
-------------------------------------------------------------- 
-- Прави списък на имената на папките, през които трябва да се премине 
-- makePath "Path as text" -> ["names of folders"]
-- example "/FMI/FP" -> ["FMI", "FP"]                                  
makePath str = smartSplit '/' str
 
---------------------------------------------------------------
-- По даден път (нареден списък от име на папки) проверява дали е валиден във файловата система
-- isCreatedPath :: [names of folders] , FileSystem -> Last visit root
isCreatePath :: [String] -> Directory -> Directory
isCreatePath _ Empty = Empty
isCreatePath [] root = root
isCreatePath (cur:path) (Node name dirs _) 
                                       |null next = Empty
                                       |otherwise = isCreatePath path (head next)
                                       where next = [n| n <- dirs, dirName n == cur]
                                       
----------------------------------------------------------------
-- Определея дали пътя е пълен или относителен и спрямо това връща списък от 
--      имената на папките, през които трябва да се премине от корена до последната точка на пътя
-- createNewPath :: currentPath , inputPath -> newPath
createNewPath curPath newPath
             |(head newPath) == '/' = normalizePath (makePath (tail newPath))
             | otherwise = normalizePath (curPath ++ (makePath newPath))         
             
-----------------------------------------------------------------
-- Нормализира пътя -> когато имаме връщане назад (".."), то бива премахнато и отиваме директно в предния връх
-- Example : norm ["d1" "d2" ".."] -> ["d1"] 
-- НЕДОСТАТЪК ПРЕМАХВА САМО КОГАТО СЕ СРЕЩАТ ЕДНО ДО ДРУГО
-- Еxample : norm  ["d1" "d2" ".." ".."] -> ["d1", ".."]   
-- norm:: [path] -> [near to normalize path]           
norm :: [String] -> [String]
norm [] = []
norm [x] = [x]
norm (x:y:ys)
                  |x /= ".." && y == ".." = norm ys
                  |otherwise = x:norm (y:ys) 
                  
-- Решава проблема на горната функция, тъй като извършва нормализиране докато не изчезнат всички връщания от пътя
-- Еxample :: ["d1" "d2" ".." ".."] -> []
-- normalizePath :: [path] -> [normalized path]
normalizePath :: [String] -> [String]                 
normalizePath [] = []
normalizePath [x]
                 |x == ".." = []
                 |otherwise = [x]
normalizePath path = if (toBack) then normalizePath nPath else nPath
                where 
                        nPath = norm path
                        toBack = not (null [b| b <- nPath, b == ".."])
               
               

-----------------------------------------------------------
-- Изпечатва всички елементи на даден лист от string-ове
printList :: [String] -> IO()
printList [] = return ()
printList (x:xs) =  --- за да може ако е Empty да не изкарва нови редове
                        if (x /= "")
                        then 
                            do 
                                putStrLn x
                                printList xs
                        else 
                               printList xs

             
------------------------------------------------------------

                    ---TOUCH---
--touch приема текущия път, лист от файлове и файлова система --> 
--                  създава множеството от файлове в съответните посочени директории
-- Example: touch pwd ["f1", "/dir/f2"] fileSystem -> създава 2 файла един в текущата директория и един в /dir
-- Забележка :: При създаването на всеки файл цялото дърво се изгражда на ново
-- touch ::  currentPath in File system (pwd), [Paths to Files] fileSystem
touch _ [] fs = fs
touch curPath (p:ps) fs = touch curPath ps (crFile (init path) (last path) fs)  
                            where path = (createNewPath curPath p)

-- crFile приема път до родителската директория на файл, име на файл и файлова система --> 
--          ако не съществува файл с такова име в родителската директория на файла, то той се създава
--          иначе файловата система не се променя
-- Еxample:  touch "/dir1/dir2/file.txt" (LINUX)
--              crFile ["dir1", "dir2"] "file.txt" myFS -> създава се файл с име file.txt в /dir1/dir2
-- Забележка :: При създаване на файл цялото дърво се изгражда на ново
--crFile [Path to Root Folder], file name,  FileSystem

crFile _ _ Empty = Empty 
crFile [] fName root@(Node name dirs files) 
                                      |null [f|f<-files, fileName f == fName] =  Node name dirs ((fName,""):files)
                                      |otherwise = root   
                                      
crFile (d:ds) fName (Node name dirs files) = (Node name ((crFile ds fName (getDir d dirs)) : (getDirOut d dirs)) files)

-----------------------------------------------------------
                            ---MKDIR---
-- crDir приема път до директория и файлова система --> създава поддиректория в посочената родителската директория
--                  с име dName, ако вече не съществува такава
-- Идея :: вървим по пътя докато не стигнем последния връх, тъй като това ще е директорията, която ще създаваме
--              или докато пътя не се окаже грешен
-- crDir [Path to Folder], FileSystem -> newFileSystem
-- ТУК ТРЯБВА ДА НАСТЪПЯТ ПРОМЕНИ
crDir :: [String] -> Directory -> Directory
crDir _ Empty = Empty
crDir [dName] root@(Node name dirs files)
                                    |null [d|d<-dirs, dirName d == dName] = Node name ((Node dName [] []): dirs) files
                                    |otherwise = root
                                    
crDir (d:ds) (Node name dirs files) = (Node name ((crDir ds (getDir d dirs)): (getDirOut d dirs)) files )

-- mkdir създава множество от директории
-- mkdir :: Current Path (pwd), [Directory to creat], FileSystem -> newFileSystem
-- По подадените относителни или абсолютни пътища се създава път от върха до папката, която ще създаваме
-- папките се създават една по една с помоща на crDir
mkdir :: [String] -> [String] -> Directory -> Directory
mkdir _ [] fs = fs
mkdir currentPath (filePath:fps) fs = mkdir currentPath fps (crDir (createNewPath currentPath filePath) fs)

------------------------------------------------------------
                                ---RM--- 

-- Взима път до файла, който ще трием и файлова система 
--                ако има файл с такова име на посоченото място във файловата система, той  ще бъде изтрит изтрива
--                иначе - системата си остава същата
-- remove :: [Path to File], FileSystem -> newFileSystem
remove :: [String] -> Directory -> Directory 
remove _ Empty = Empty
remove [fName] (Node name dirs files) = (Node name dirs [f | f <- files, fileName f /= fName])
remove (x:xs) (Node name directories files) = (Node name 
                                        ((remove xs (getDir x directories)): (getDirOut x directories)) 
                                        files) 
                                        
-- rm изтрива множество от директории
-- По подадените относителни или абсолютни пътища се създава път от върха до папката, която искаме да изтрием
-- папките се трият една по една с помоща на remove
-- Ако някой от файловете не бъде намерен, то той просто не се изтрива
-- rm:: current Path(pwd), [Files to delete], FileSystem -> newFileSystem
rm :: [String] -> [String] -> Directory -> Directory
rm _ [] fs = fs
rm currentPath (filePath:fps) fs = rm currentPath fps (remove (createNewPath currentPath filePath) fs) 
---------------------------------------------------------------
                                ---PWD---
-- pwd изпечатва текущия път (пътя от корена на файловата система до директорията, 
--        в която се намираме в момента)                                 
pwd :: [String] -> IO()
pwd [] = putStrLn "/"

pwd [x] = do
            putStr "/"
            putStrLn x
            
pwd (curNode:path) = do 
                        putStr "/"
                        putStr curNode
                        pwd path


             
-----------------------------------------------------------------------
                          ---CD---
-- cd  -> променяме текущата директория -> може да се промени спрямо текущата директория
--                                      -> спрямо корено на дърво
-- Позволява се връщане назад и ходене напред
-- Еxample: cd /dir1/../dir2/../dir1 -> /dir1 
-- Ако подадения път не се намери, то се задапазва текущия
-- cd :: proposed path (as text), [current Path (pwd)], filesystem ->  new current path                      
cd :: String -> [String] -> Directory -> [String]  
cd newPath curPath fs = changePath (res (isCorrect))
                where 
                       path = createNewPath curPath newPath
                       isCorrect = isCreatePath path fs
                       res Empty = False 
                       res _ = True
                       changePath cond
                                |cond == True =  path 
                                |otherwise = curPath
                                
                        
-------------------------------------------------------------------
                           ---LS---    
-- ls - изпечатва съдържанието на дадена директия с помоща на lsDir
-- Директорията може да бъде подадена чрез
--                          -> пълен път
--                          -> относителен път
--                          -> без аргументи ->  показва съдържанието на текущата директория                          
ls :: [String] -> String -> Directory -> IO()
ls curPath newPath fs
                            | null newPath = lsDir (isCreatePath curPath fs) -- значи не е подаден аргумент, т.е. сме в текущия връх
                            | (validPath findPath) == False = putStrLn ("Unknow path") -- пътя не е валиден
                            | otherwise = lsDir findPath -- при подадено extend-ване на сgoегашния текущ път или 
                            where path = createNewPath curPath newPath
                                  findPath = isCreatePath path fs
                                  validPath Empty = False
                                  validPath _ = True

-- Изпечатва имената на файловете и директориите, които се съдържат в подадената директория
lsDir :: Directory -> IO()                                   
lsDir (Node _ directories files) =
                                do
                                printList [dn | d <- directories,let dn = dirName d]
                                printList [fn | f <- files, let fn = fileName f]

------------------------------------------------------------------
                            ---CAT---
-- catFile приема файл и извежда съдържанието му на екрана
-- catFile :: File@(name, content) -> cout << content
catFile :: File -> IO()
catFile file = putStrLn . fileContent $ file

-- cat извежда на екрана съдържанието на множество файлове (последователно)
-- cat :: Current path, [Files], FileSystem 
cat :: [String] -> [String] -> Directory -> IO()
cat _ [] _ = return ()
cat currentPath (x:xs) fs = do
                                    catFile (getFile fName$ root)
                                    cat currentPath xs fs
                    where path y = createNewPath currentPath y
                          newPath = path x  
                          fName =  last newPath
                          root = isCreatePath (init newPath) fs -- проверка дали има такъв път и ако има се връща директорията,
                                                           -- в която се намира файла
                          
                          ---CAT + > ---
-- changeFileContent - по даден път до родителската му директори + името на файла + ново съдържание 
--                                      + файловата система,в която ще се извършва действието 
-- Променя се съдържанието на файл с filename, до който може да се достигне по пътя path, 
--    съдържанието на файла се изтрива и се заменя с ново -- newContent
-- changeFileContent :: Path to File, file name, new content, fileSystem -> newFileSystem                       
changeFileContent :: [String] -> String -> String -> Directory -> Directory 
changeFileContent _ _ _ Empty =Empty                    
changeFileContent [] fn newContent root@(Node name dirs files)= (Node name dirs ((fn, newContent):(getFilesWithout fn root)))                     
changeFileContent (x:xs) fn newContent (Node name dirs files) = (Node name 
                                    ((changeFileContent xs fn newContent (getDir x dirs)):(getDirOut x dirs)) 
                                    files ) 
                                    
-- catInFile -- конкатенира съдържанието на множество от файлове в съдържанието на друг файл                                  
-- Идея :: записваме съдържанието на всички файлове в даден връх и след това подменяме с него оригиналния връх                                    
-- catInFile:: currentPath, [Files to concat], [Path to output file], (output file), fileSystem
catInFile _ [] path (name, content) fs = changeFileContent path name content fs
catInFile curPath (x:xs) path file fs = catInFile 
                                                        curPath xs path 
                                                        (writeContInFile (createNewPath curPath x) file fs) fs

-- writeContInFile -- добавя към съдържанието на един файл  съдържанието на друг файл                                                        
-- writeContInFile [path to inputFile], output file, fileSystem -> newFileSystem
writeContInFile :: [String] -> File -> Directory -> File
writeContInFile _ file Empty = file
writeContInFile [x] (fName, content) root = (fName, content ++ fileContent(getFile x root))
writeContInFile (x:xs) file (Node _ dirs _) = writeContInFile xs file (getDir x dirs)

------------------------------------------------------------------------
                            ---CHOICE--AND--PROCESS--COMMAND---
------------------
changePath cmd 
                | cmd == "cd" = True
                | otherwise = False
                
processPathChange cmd arg curPath fs 
                    |cntArg == 0  = do
                                    putStrLn ("Invalid number of arguments\n")    
                                    return (curPath)
                    |cntArg > 1 = do
                                    putStrLn ("Invalid number of arguments\n")    
                                    return (curPath)
                    |cmd == "cd" = return (cd (head arg) curPath fs)
                    |otherwise = return (curPath)
                    where 
                            cntArg = length arg
                       

              
-------------------                
changeFileSystem cmd changeOutPut
                | cmd == "cat" = changeOutPut
                | cmd == "mkdir" = True
                | cmd == "touch" = True
                | cmd == "rm" = True
                | otherwise = False
      
processFSChange cmd arg curPath fs
                | cntArg == 0 = do
                                    putStrLn ("Invalid number of arguments\n")    
                                    return (fs)
                | cmd == "cat" = return (catInFile curPath files (init path) (name, "" ) fs)
                | cmd == "mkdir" = return (mkdir curPath arg fs)
                | cmd == "touch" = return (touch curPath arg fs)
                | cmd == "rm" = return (rm curPath arg fs)
                where 
                            cntArg = length arg
                            files = (takeWhile (\x -> x /= ">") arg)
                            path = (createNewPath curPath (last arg))
                            name = (last path)
---------------------- 
dontChange cmd = elem cmd ["cmd","ls","pwd"]
                
processOther cmd  arg curPath fs
                | cmd == "cat" = cat curPath arg fs 
                | cmd == "pwd" = pwd curPath 
                | cmd == "ls"  && null arg = ls curPath [] fs
                              
                | cmd == "ls" = ls curPath (head arg) fs
                
-------------------------------------------------------------------------------                
-------------------------------------------------------------------------------
                            ---MAIN---
main = startFileSystem fileSystem []
                                

process cmd arg curPath fs
          |changeFileSystem cmd isChangeOutput = do
                                                    newFs <- processFSChange cmd arg curPath fs
                                                    startFileSystem newFs curPath
          |changePath cmd = do
                                newPath <- processPathChange cmd arg curPath fs
                                startFileSystem fs newPath
          |dontChange cmd = do
                                processOther cmd arg curPath fs
                                startFileSystem fs curPath
          |cmd == "stop" = putStrLn "Sleep!"
          |otherwise = do
                            putStrLn ("Wrong command!")
                            startFileSystem fs curPath
                            
           where isChangeOutput = not(null [l|l <- arg, elem '>' l])
                            
startFileSystem Empty _ = putStrLn "Break file system\n"
startFileSystem  fs curPath = do 
                                 putStr "$ "
                                 command <- getLine
                                 let instruct = smartSplit ' ' command
                                     cmd = head instruct
                                     arg = tail instruct
                                 process cmd arg curPath fs
                                 
--------------------------------------------------------------------------------------------