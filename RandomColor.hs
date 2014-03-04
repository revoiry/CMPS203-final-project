import Graphics.UI.GLUT
import Data.List
import System.Random
import Data.IORef

--to initialize color for the block list, just call init_color parm1 parm2
--parm1 is the length of the block list and the parm 2 is the block_list to
--be initiate
--
--For example, init_color 36 block_list
--you may need to set the global static variables for custom use


-------global static variables-------

--set the number of colors 
num_of_col_f :: GLfloat
num_of_col_f = 4

num_of_col_i :: Int
num_of_col_i = 4

--set the half of the block number
half_num_of_blocks :: Int
half_num_of_blocks = 18

--the blocklist,fortest
block_list :: [(GLfloat,GLfloat,GLfloat,Int,Int,GLfloat,Int,(GLfloat,GLfloat,GLfloat))]
block_list = [(0.0, 0.0, 0.0, 0, 0, 0.0, 0, (0.0,0.0,0.0))| k <- [1..(2*half_num_of_blocks)] ]

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

--init the color for the block list
--init_color :: t -> [t1] -> [a]
init_color _ [] = []
init_color n ((t0,t1,t2,t3,t4,t5,t6,_):xs) = (t0,t1,t2,t3,t4,t5,t6,(ran_number n)):(init_color (n-1) xs)


