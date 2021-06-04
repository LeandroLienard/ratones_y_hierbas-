module Spec where
import PdePreludat
import Library
import Test.Hspec


ratonPrueba = UnRaton {
    nombre = "rata de laboratorio"
    ,edad = 25
    ,peso = 2
    ,enfermedades = ["asma","bronquitis","refriado","tos aguda","influenza"]
}

listaInfConRatonPrueba = (ratonPrueba: repeat huesudo)
correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El ratonPrueba con pdepCilina tiene 4 enfermedades  y pesa 2 kilos" $ do
       peso (administrarMedicamentoA ratonPrueba pdepCilina) `shouldBe` 2
       (length.enfermedades) (administrarMedicamentoA ratonPrueba pdepCilina) `shouldBe` 4
    it "NO DEBE lograr Estabilizar lita inf De ratones con raton prueba" $ do
       lograEstabilizar pdepCilina listaInfConRatonPrueba `shouldBe` False
