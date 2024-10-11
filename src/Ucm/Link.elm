module Ucm.Link exposing (..)

import Html exposing (Html, text)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import UI.Click as Click exposing (Click)
import Url exposing (Url)



{-

   Link
   ====

   Various UI.Click link helpers for Routes and external links

-}
-- EXTERNAL


link : String -> Click msg
link url =
    Click.externalHref url


unisonCloudWebsite : Click msg
unisonCloudWebsite =
    Click.externalHref "https://unison.cloud"


unisonCloudWebsiteLearn : Click msg
unisonCloudWebsiteLearn =
    Click.externalHref "https://unison.cloud/learn"


website : Click msg
website =
    Click.externalHref "https://unison-lang.org"


conference : Click msg
conference =
    Click.externalHref "https://unison-lang.org/conference"


whatsNew : Click msg
whatsNew =
    Click.externalHref "https://unison-lang.org/whats-new"


whatsNewPost : String -> Click msg
whatsNewPost postPath =
    Click.externalHref ("https://unison-lang.org/whats-new/" ++ postPath)


github : Click msg
github =
    Click.externalHref "https://github.com/unisonweb/unison"


githubReleases : Click msg
githubReleases =
    Click.externalHref "https://github.com/unisonweb/unison/releases"


githubRelease : String -> Click msg
githubRelease releaseTag =
    Click.externalHref ("https://github.com/unisonweb/unison/releases/tag/" ++ releaseTag)


reportUnisonBug : Click msg
reportUnisonBug =
    Click.externalHref "https://github.com/unisonweb/unison/issues/new"


reportShareBug : Click msg
reportShareBug =
    Click.externalHref "https://github.com/unisoncomputing/share-ui/issues/new"


docs : Click msg
docs =
    Click.externalHref "https://unison-lang.org/docs"


share : Click msg
share =
    Click.externalHref "https://share.unison-lang.org"


unisonShareDocs : Click msg
unisonShareDocs =
    Click.externalHref "https://unison-lang.org/learn/tooling/unison-share"


tour : Click msg
tour =
    Click.externalHref "https://unison-lang.org/docs/tour"


codeOfConduct : Click msg
codeOfConduct =
    Click.externalHref "https://www.unison-lang.org/community/code-of-conduct/"


status : Click msg
status =
    Click.externalHref "https://unison.statuspage.io"


discord : Click msg
discord =
    Click.externalHref "https://unison-lang.com/discord"


login : HttpApi -> Url -> Click msg
login api returnTo =
    let
        returnTo_ =
            returnTo
                |> Url.toString
                |> Url.percentEncode
    in
    Click.externalHref_ Click.Self (HttpApi.baseApiUrl api ++ "login?return_to=" ++ returnTo_)


logout : HttpApi -> Url -> Click msg
logout api returnTo =
    let
        returnTo_ =
            returnTo
                |> Url.toString
                |> Url.percentEncode
    in
    Click.externalHref_ Click.Self (HttpApi.baseApiUrl api ++ "logout?return_to=" ++ returnTo_)



-- VIEW


view : String -> Click msg -> Html msg
view label click =
    view_ (text label) click


view_ : Html msg -> Click msg -> Html msg
view_ label_ click =
    Click.view [] [ label_ ] click
