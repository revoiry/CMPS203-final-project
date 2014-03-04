import Graphics.UI.GLUT
import Data.List
import System.Random
import Data.IORef

--set colorlist and aetlist
--main = do
--  col_num  <- newIORef (4::Int)
--  print col_num

--get the number of colors and the number of randon numbers
num_of_col :: GLfloat
num_of_col = 4

num_of_ran :: Int
num_of_ran = 4

--get the list of color
color_list :: [(GLfloat,GLfloat,GLfloat)]
color_list = [(sin(2*pi*k/num_of_col ), cos(2*pi*k/num_of_col), 0) | k <- [1..num_of_col] ]

--set_list :: [Int]
--set_list = take col_num (repeat 0)


--get rendom number, x y are the bounds and z is the length of list 
--ran_int_list :: Random a => a -> a -> Int -> [a]
ran_int_list z = take z (randomRs (1,num_of_ran) (mkStdGen 12))

--get the nth number in ran_int_list
n_ran_num :: Int -> [Int] -> Int
n_ran_num n ran_int_list = last ( ran_int_list)


--ran_number :: Int -> [(GLfloat, GLfloat, GLfloat)]
ran_number n color_list = last (take m (color_list ))
          where m = (n_ran_num n (ran_int_list n))


