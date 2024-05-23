import ParcialUno
import Test.HUnit


nMaterias = runTestTT testNMaterias

buenasNotas = runTestTT testBuenasNotas

mejorAlumno = runTestTT testMejorPromedio

graduado = runTestTT testGraduado

testNMaterias = test[
        "mayor a n, pepe" ~: aproboMasDeNMaterias [(['p','e','p','e'],[2,6,3,9,6,3,4])] ['p','e','p','e'] 3 ~?= True,
        "mayor a n, pedro" ~: aproboMasDeNMaterias [(['p','e','p','e'],[2,6,3,9,6,3,4])] ['p','e','d','r','o'] 3 ~?= False,
        "menor a n" ~: aproboMasDeNMaterias [(['m','a','r','i'],[2,2,1,3,6,3,4])] ['m','a','r','i'] 3 ~?= False,
        "igual a n" ~: aproboMasDeNMaterias [(['m','a','r','i'],[2,2,1,3,6,4,4])] ['p','e','d','r','o'] 3 ~?= False
    ]

testBuenasNotas = test[
        "0 alumnos" ~: buenosAlumnos [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4])] ~?= [],
        "1 alumno" ~: buenosAlumnos [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[4,7,9,5,8,9,10,10,10])] ~?= [['r','o','s','a']],
        "2 alumnos" ~: buenosAlumnos [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[9,7,9,5,8,9,10,10,10]), (['i','v','a','n'],[9,7,9,8,8,9,10,10,10])] ~?= [['r','o','s','a'], ['i','v','a','n']]
    ]

testMejorPromedio = test[
        "pepe" ~: mejorPromedio [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4])] ~?= ['p','e','p','e'],
        "rosa" ~: mejorPromedio [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[4,7,9,5,8,9,10,10,10])] ~?= ['r','o','s','a'],
        "ivan" ~: mejorPromedio [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[9,7,9,5,8,9,10,10,10]), (['i','v','a','n'],[9,7,9,8,8,9,10,10,10])] ~?= ['i','v','a','n']
    ]

testGraduado = test[
        "rosa" ~: seGraduoConHonores [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[9,7,9,5,8,9,10,10,10]), (['i','v','a','n'],[9,7,9,8,8,9,10,10,10])] 7 ['r','o','s','a'] ~?= True,
        "ivan" ~: seGraduoConHonores [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[9,7,9,5,8,9,10,10,10]), (['i','v','a','n'],[9,7,9,8,8,9,10,10,10])] 7 ['i','v','a','n'] ~?= True,
        "no graduado" ~: seGraduoConHonores [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[9,7,9,5,8,9,10,10,10]), (['i','v','a','n'],[9,7,9,8,8,9,10,10,10])] 7 ['p','e','p','e'] ~?= False,
        "no graduada" ~: seGraduoConHonores [(['m','a','r','i','a'],[2,2,1,3,6,3,4]),(['p','e','p','e'],[2,6,3,9,6,3,4]),(['r','o','s','a'],[9,7,9,5,8,9,10,10,10]), (['i','v','a','n'],[9,7,9,8,8,9,10,10,10])] 7 ['m','a','r','i','a'] ~?= False
    ]