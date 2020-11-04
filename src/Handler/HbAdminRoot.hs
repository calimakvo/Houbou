module Handler.HbAdminRoot where

import Import

getHbAdminRootR :: Handler Html
getHbAdminRootR = redirect DashboardR
