# Function-programming---Project

# ФАЙЛОВА СИСТЕМА             
Да се напише програма, която симулира файлова система, и позволява работа с нея в
интерактивен режим.
Файловите системи служат за организация на данни в именувани блокове данни,
наречени файлове. Файловете от своя страна са организирани в йерархична структура
чрез използването на директории. В Unixбазираните
системи, коренът на файловата система се означава с / , като същият символ се използва и за разделител на директории.
Целта на проекта е да се реализира програма, която записва информацията за
файловата система в подходящо избрана структура (в паметта или във файл).
Програмата да предлага конзолен интерфейс за работа със системата, като поддържа
командите:

● pwd — извежда пълния път до текущата директория (в началото / )
-----------------------------------------------------------------

● cd — променя текущата директория
-----------------------------------
○ синтаксисът е cd <директория>  
○ cd /full/path сменя текущата директория с тази, зададена като
параметър  
○ cd relative/path сменя текущата директория с тази, зададена с
относителен път относно текущата директория  
○ .. означава родителската директория, например cd
scheme/../haskell/../scheme/../haskell е еквивалентно на cd
haskell  

● ls — извежда съдържанието на директория
------------------------------------------
○ ls — извежда съдържанието на текущата директория  
○ ls relative/path извежда съдържанието на директория, зададена с  
относителен път относно текущата  
○ ls /full/path извежда съдържанието на директория, зададена с
абсолютен път  

● cat — конкатенира съдържанието на файлове
--------------------------------------------
○ общият синтаксис е cat <файл 1 > <файл 2 > … <файл n > > <изходенфайл>  
○ <файл 1 > <файл 2 > … <файл n > са относителни или абсолютни пътища  
○ <изходенфайл> е файл, в който се записва резултатът  
○ съдържанията на <файл 1 > <файл 2 > … <файл n > се слепват в реда, в който са
зададени пътищата на файловете, и резултатът се извежда в изходен файл  
○ ако няма нито един <файл>, съдържанието се въвежда от стандартния вход,
като въвеждането приключва при въвеждане на знак точка самостоятелно на
ред.  
○ ако няма <изходенфайл>,
съдържанието се извежда на стандартния изход  

● rm — изтриване на файл
--------------------------
○ синтаксисът е rm <файл 1 > <файл 2 > … <файл n >  
○ изтрива всичките файлове, подадени като аргументи  

Пример
------
$ pwd  
/  
$ ls  
folder1 folder2 file1 file2  
$ cd folder1  
$ pwd  
/folder1  
$ cd ..  
$ rm file2  
$ ls  
folder1 folder2 file1  
$ cat > file1  
This is the content for file1  
.  
$ cat file1  
This is the content for file1  
$ cat file1 > file3  
$ cat file3  
This is the content for file1  
$ cat file1 file3 > filte4  
$ cat file4  
This is the content for file1  
This is the content for file1  
