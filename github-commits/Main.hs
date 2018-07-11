{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                    (FromJSON (..), Value (Object),
                                                decodeStrict, (.:))
import qualified Data.JSString                 as J
import           JavaScript.Web.XMLHttpRequest
import           Miso
import           Miso.String                   (fromMisoString, ms)

main :: IO ()
main = do
    startApp App {
      initialAction = FetchCommits
    , model = initialModel
    , update = updateModel
    , view = viewModel
    , subs = []
    , events = defaultEvents
    , mountPoint = Nothing
    }

type Branch = String

data Model = Model {
      branches      :: [Branch]
    , currentBranch :: Branch
    , commits       :: Maybe [Commit]
    } deriving (Eq, Show)

data Commit = Commit {
      cmtSHA        :: String
    , cmtURL        :: String
    , cmtMessage    :: String
    , cmtAuthorName :: String
    , cmtAuthorURL  :: String
    , cmtDate       :: String
    } deriving (Eq, Show)

instance FromJSON Commit where
    parseJSON (Object v) = do
        Object cmt <- v .: "commit"
        Object cmtAuthor <- cmt .: "author"
        Object author <- v .: "author"
        sha <- v .: "sha"
        url <- v .: "html_url"
        msg <- cmt .: "message"
        name <- cmtAuthor .: "name"
        date <- cmtAuthor .: "date"
        aUrl <- author .: "html_url"
        pure $ Commit sha url msg name aUrl date

data Action =
      FetchCommits
    | SetCommits (Maybe [Commit])
    | SetBranch Branch

initialModel :: Model
initialModel = Model {
      branches      = [ "master", "0.21.1.0" ]
    , currentBranch = "master"
    , commits       = Nothing
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel FetchCommits model = model <# do
    mCommits <- fetchCommits (currentBranch model)
    pure $ SetCommits mCommits
updateModel (SetCommits mCommits) model =
    noEff model { commits = mCommits }
updateModel (SetBranch branch) model =
    model { currentBranch = branch } <# do
        pure FetchCommits

fetchCommits :: Branch -> IO (Maybe [Commit])
fetchCommits branch = do
    Just json <- contents <$> xhrByteString req
    pure $ decodeStrict json
    where
        req = Request
            { reqMethod = GET
            , reqURI = J.pack url
            , reqLogin = Nothing
            , reqHeaders = []
            , reqWithCredentials = False
            , reqData = NoData
            }
        url =
            "https://api.github.com/repos/dmjio/miso/commits?per_page=3&sha="
            ++ branch

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "demo" ] $
       [ h1_ [] [ text "Latest Miso Commits" ] ]
    ++ concatMap (toRadioButton $ currentBranch model) (branches model)
    ++ [ p_  [] [ text . ms $ "dmjio/miso@" ++ currentBranch model ] ]
    ++ [ ul_ [] $ maybe [] (map toListItem) (commits model) ]

toRadioButton :: Branch -> Branch -> [View Action]
toRadioButton current branch =
    let msb = ms branch
    in  [ input_
            [ type_ "radio"
            , id_ msb
            , value_ msb
            , name_ "branch"
            , checked_ $ current == branch
            , onChange (SetBranch . fromMisoString)
            ]
        , label_ [ for_ msb ] [ text msb ]
        ]

toListItem :: Commit -> View action
toListItem commit = li_ [] [
      a_ [
          href_ . ms $ cmtURL commit
        , target_ "_blank"
        , class_ "commit"
        ] [
          text . ms . take 7 $ cmtSHA commit
        ]
    , text " - "
    , span_ [ class_ "message" ] [ text . ms . trancate $ cmtMessage commit ]
    , br_ []
    , text "by "
    , span_
        [ class_ "author" ]
        [ a_ [
              href_ . ms $ cmtAuthorURL commit
            , target_ "_blank"
            ] [
              text . ms $ cmtAuthorName commit
            ]
        ]
    , text " at "
    , span_ [ class_ "date" ] [ text . ms . formatDate $ cmtDate commit ]
    ]

trancate :: String -> String
trancate = takeWhile (/= '\n')

formatDate :: String -> String
formatDate = map (\c -> if c `elem` ['T', 'Z'] then ' ' else c)
