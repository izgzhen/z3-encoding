import qualified Z3.Test
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Z3" Z3.Test.spec
