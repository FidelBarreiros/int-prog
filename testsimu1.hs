import MisFunciones
import Test.HUnit


testFibo = test [
            "Caso Base 1: fib 0" ~: (fib 0) ~?= 0, 
            "Caso Base 2: fib 1" ~: (fib 1) ~?= 1,
            "Caso Recursivo: fib 2" ~: (fib 2) ~?= 1   
            ]