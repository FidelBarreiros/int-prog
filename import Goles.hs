import Goles
import Test.HUnit

equipoVal = runTestTT testEquiposValidos
testEquiposValidos = test[
        "no repetidos" ~: equiposValidos [("Sacachispas", "Neyder Aragon"), ("Fenix", "Nahuel Galardi"), ("Buenos Aires", "Gabriel")] ~?= True,
        "club repetido" ~: equiposValidos [("Sacachispas", "Neyder Aragon"), ("Fenix", "Nahuel Galardi"), ("Buenos Aires", "Gabriel"), ("Fenix", "Lucio")] ~?= False,
        "arquero repetido" ~: equiposValidos [("Sacachispas", "Neyder Aragon"), ("Fenix", "Nahuel Galardi"), ("Buenos Aires", "Gabriel"), ("Tigre", "Nahuel Galardi")] ~?= False,
        "arquero nombre de club" ~: equiposValidos [("Sacachispas", "Neyder Aragon"), ("Fenix", "Nahuel Galardi"), ("Buenos Aires", "Gabriel"), ("Tigre", "Buenos Aires")] ~?= False,
        "iguales" ~: equiposValidos [("Sacachispas", "Sacachispas"), ("Fenix", "Nahuel Galardi"), ("Buenos Aires", "Gabriel")] ~?= False
    ]

