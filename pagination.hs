{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import qualified Data.List as L
import qualified Data.Text as T

data User = User { userName :: T.Text, userAge :: Int, userAddress :: T.Text }

sampleUsers :: [User]
sampleUsers = [ User "Alice" 25 "123 Main St."
              , User "Bob" 30 "456 Oak Ave."
              , User "Charlie" 35 "789 Elm St."
              , User "Dave" 40 "321 Maple St."
              , User "Eve" 45 "654 Pine Ave."
              ]

data UserSort = SortByName | SortByAge | SortByAddress

sortUsers :: UserSort -> [User] -> [User]
sortUsers SortByName = L.sortOn userName
sortUsers SortByAge = L.sortOn userAge
sortUsers SortByAddress = L.sortOn userAddress

data Page = Page { pageUsers :: [User], pageSort :: UserSort, pageOffset :: Int }

pageSize :: Int
pageSize = 2

pageCount :: [User] -> Int
pageCount users = (length users + pageSize - 1) `div` pageSize

paginateUsers :: Int -> Int -> [User] -> [User]
paginateUsers offset size = take size . drop offset

renderUser :: MonadWidget t m => User -> m ()
renderUser user =
    el "tr" $ do
        el "td" $ text (userName user)
        el "td" $ text (T.pack $ show $ userAge user)
        el "td" $ text (userAddress user)

renderTable :: MonadWidget t m => [User] -> m (Event t UserSort)
renderTable users = do
    elClass "table" "users" $ do
        el "thead" $ do
            el "th" $ text "Name"
            el "th" $ text "Age"
            el "th" $ text "Address"
            el "th" $ text "Action"
        el "tbody" $ do
            let sortedUsers = sortUsers SortByName users
                pageCount' = pageCount sortedUsers
            pageOffset' <- holdDyn 0 =<< fmap (+ 1) <$> button "Next page"
            let currentPage = Page { pageUsers = paginateUsers (pageOffset pageOffset') pageSize sortedUsers
                                   , pageSort = SortByName
                                   , pageOffset = pageOffset pageOffset'
                                   }
            _ <- dyn $ renderUser <$> pageUsers currentPage
            sortClicks <- elClass "tfoot" "controls" $ do
                sortButton "Name" SortByName (pageSort currentPage)
                sortButton "Age" SortByAge (pageSort currentPage)
                sortButton "Address" SortByAddress (pageSort currentPage)
            return $ switchPromptlyDyn sortClicks
  where sortButton name sortKey currentSort =
            elClass "th" ("sort " <> if sortKey == currentSort then "active" else "") $ do
                ev <- button name
                return $ sortUsers sortKey <$ ev

main :: IO ()
main = mainWidget $ renderTable sampleUsers