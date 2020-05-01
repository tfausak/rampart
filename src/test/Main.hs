module Main
  ( main
  ) where

import qualified Rampart
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Rampart" $ do

  Hspec.describe "toInterval" $ do

    Hspec.it "sorts the tuple" $ do
      Rampart.toInterval ('a', 'b') `Hspec.shouldBe` Rampart.toInterval ('b', 'a')

  Hspec.describe "lesser" $ do

    Hspec.it "returns the lesser element" $ do
      Rampart.lesser (Rampart.toInterval ('a', 'b')) `Hspec.shouldBe` 'a'

  Hspec.describe "greater" $ do

    Hspec.it "returns the greater element" $ do
      Rampart.greater (Rampart.toInterval ('a', 'b')) `Hspec.shouldBe` 'b'

  Hspec.describe "isEmpty" $ do

    Hspec.it "returns true when the interval is empty" $ do
      Rampart.isEmpty (Rampart.toInterval ('a', 'a')) `Hspec.shouldBe` True

    Hspec.it "returns false when the interval is not empty" $ do
      Rampart.isEmpty (Rampart.toInterval ('a', 'b')) `Hspec.shouldBe` False

  Hspec.describe "isNonEmpty" $ do

    Hspec.it "returns false when the interval is empty" $ do
      Rampart.isNonEmpty (Rampart.toInterval ('a', 'a')) `Hspec.shouldBe` False

    Hspec.it "returns true when the interval is non-empty" $ do
      Rampart.isNonEmpty (Rampart.toInterval ('a', 'b')) `Hspec.shouldBe` True

  Hspec.describe "relate" $ do

    let
      relate :: (Int, Int) -> (Int, Int) -> Rampart.Relation
      relate x y = Rampart.relate (Rampart.toInterval x) (Rampart.toInterval y)

    Hspec.it "identifies the before relation" $ do
      relate (1, 2) (3, 7) `Hspec.shouldBe` Rampart.Before

    Hspec.it "identifies the meets relation" $ do
      relate (2, 3) (3, 7) `Hspec.shouldBe` Rampart.Meets

    Hspec.it "identifies the overlaps relation" $ do
      relate (2, 4) (3, 7) `Hspec.shouldBe` Rampart.Overlaps

    Hspec.it "identifies the finished by relation" $ do
      relate (2, 7) (3, 7) `Hspec.shouldBe` Rampart.FinishedBy

    Hspec.it "identifies the contains relation" $ do
      relate (2, 8) (3, 7) `Hspec.shouldBe` Rampart.Contains

    Hspec.it "identifies the starts relation" $ do
      relate (3, 4) (3, 7) `Hspec.shouldBe` Rampart.Starts

    Hspec.it "identifies the equal relation" $ do
      relate (3, 7) (3, 7) `Hspec.shouldBe` Rampart.Equal

    Hspec.it "identifies the started by relation" $ do
      relate (3, 8) (3, 7) `Hspec.shouldBe` Rampart.StartedBy

    Hspec.it "identifies the during relation" $ do
      relate (4, 6) (3, 7) `Hspec.shouldBe` Rampart.During

    Hspec.it "identifies the finishes relation" $ do
      relate (6, 7) (3, 7) `Hspec.shouldBe` Rampart.Finishes

    Hspec.it "identifies the overlapped by relation" $ do
      relate (6, 8) (3, 7) `Hspec.shouldBe` Rampart.OverlappedBy

    Hspec.it "identifies the met by relation" $ do
      relate (7, 8) (3, 7) `Hspec.shouldBe` Rampart.MetBy

    Hspec.it "identifies the after relation" $ do
      relate (8, 9) (3, 7) `Hspec.shouldBe` Rampart.After

    Hspec.describe "empty left interval" $ do

      Hspec.it "before" $ do
        relate (2, 2) (3, 7) `Hspec.shouldBe` Rampart.Before

      Hspec.it "at lesser" $ do
        relate (3, 3) (3, 7) `Hspec.shouldBe` Rampart.Overlaps

      Hspec.it "during" $ do
        relate (5, 5) (3, 7) `Hspec.shouldBe` Rampart.During

      Hspec.it "at greater" $ do
        relate (7, 7) (3, 7) `Hspec.shouldBe` Rampart.OverlappedBy

      Hspec.it "after" $ do
        relate (8, 8) (3, 7) `Hspec.shouldBe` Rampart.After

    Hspec.describe "empty right interval" $ do

      Hspec.it "before" $ do
        relate (3, 7) (2, 2) `Hspec.shouldBe` Rampart.After

      Hspec.it "at lesser" $ do
        relate (3, 7) (3, 3) `Hspec.shouldBe` Rampart.OverlappedBy

      Hspec.it "during" $ do
        relate (3, 7) (5, 5) `Hspec.shouldBe` Rampart.Contains

      Hspec.it "at greater" $ do
        relate (3, 7) (7, 7) `Hspec.shouldBe` Rampart.Overlaps

      Hspec.it "after" $ do
        relate (3, 7) (8, 8) `Hspec.shouldBe` Rampart.Before

    Hspec.describe "both empty intervals" $ do

      Hspec.it "before" $ do
        relate (4, 4) (5, 5) `Hspec.shouldBe` Rampart.Before

      Hspec.it "equal" $ do
        relate (5, 5) (5, 5) `Hspec.shouldBe` Rampart.Equal

      Hspec.it "after" $ do
        relate (6, 6) (5, 5) `Hspec.shouldBe` Rampart.After

  Hspec.describe "invert" $ do

    Hspec.it "inverts the after relation" $ do
      Rampart.invert Rampart.After `Hspec.shouldBe` Rampart.Before

    Hspec.it "inverts the before relation" $ do
      Rampart.invert Rampart.Before `Hspec.shouldBe` Rampart.After

    Hspec.it "inverts the contains relation" $ do
      Rampart.invert Rampart.Contains `Hspec.shouldBe` Rampart.During

    Hspec.it "inverts the during relation" $ do
      Rampart.invert Rampart.During `Hspec.shouldBe` Rampart.Contains

    Hspec.it "inverts the equal relation" $ do
      Rampart.invert Rampart.Equal `Hspec.shouldBe` Rampart.Equal

    Hspec.it "inverts the finished by relation" $ do
      Rampart.invert Rampart.FinishedBy `Hspec.shouldBe` Rampart.Finishes

    Hspec.it "inverts the finishes relation" $ do
      Rampart.invert Rampart.Finishes `Hspec.shouldBe` Rampart.FinishedBy

    Hspec.it "inverts the meets relation" $ do
      Rampart.invert Rampart.Meets `Hspec.shouldBe` Rampart.MetBy

    Hspec.it "inverts the met by relation" $ do
      Rampart.invert Rampart.MetBy `Hspec.shouldBe` Rampart.Meets

    Hspec.it "inverts the overlapped by relation" $ do
      Rampart.invert Rampart.OverlappedBy `Hspec.shouldBe` Rampart.Overlaps

    Hspec.it "inverts the overlaps relation" $ do
      Rampart.invert Rampart.Overlaps `Hspec.shouldBe` Rampart.OverlappedBy

    Hspec.it "inverts the started by relation" $ do
      Rampart.invert Rampart.StartedBy `Hspec.shouldBe` Rampart.Starts

    Hspec.it "inverts the starts relation" $ do
      Rampart.invert Rampart.Starts `Hspec.shouldBe` Rampart.StartedBy
