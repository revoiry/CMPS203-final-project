import Graphics.UI.GLUT
import Data.List
import System.Random
import Data.IORef

-------global static variables-------

--get the number of colors and the number of randon numbers
num_of_col_f :: GLfloat
num_of_col_f = 4

num_of_col_i :: Int
num_of_col_i = 4

--the half of the block number
half_num_of_blocks :: Int
half_num_of_blocks = 18


-------functions-------

--get the list of color
color_list :: [(GLfloat,GLfloat,GLfloat)]
color_list = [(sin(2*pi*k/num_of_col_f ), cos(2*pi*k/num_of_col_f), 0) | k <- [1..num_of_col_f] ]

--get rendom number from 1 to the number of colors 
--get the list of half length of the block list
h_ran_int_list :: [Int]
h_ran_int_list = take half_num_of_blocks (randomRs (1,num_of_col_i) (mkStdGen 12))

--duplicate the privious list and one list to the other's end
--so every random number appears even times
ran_int_list :: [Int]
ran_int_list = h_ran_int_list ++ h_ran_int_list 

--get the nth number in ran_int_list
n_ran_num :: Int -> [Int] -> Int
n_ran_num n ran_int_list = last (take n ran_int_list)

--get a random number for nth block in blick list, n is the index
ran_number :: Int -> (GLfloat, GLfloat, GLfloat)
ran_number n = last (take m (color_list ))
          where m = (n_ran_num n (ran_int_list ))


