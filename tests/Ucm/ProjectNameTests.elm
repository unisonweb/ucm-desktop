module Ucm.ProjectNameTests exposing (..)

import Expect
import Test exposing (..)
import Ucm.ProjectName as ProjectName


equals : Test
equals =
    describe "ProjectName.equals"
        [ test "is true for equal names with handles" <|
            \_ ->
                let
                    a =
                        ProjectName.unsafeFromString "@unison/base"

                    b =
                        ProjectName.unsafeFromString "@unison/base"
                in
                Expect.true "`@unison/base` is equal to `@unison/base`" (ProjectName.equals a b)
        , test "is true for equal names without handles" <|
            \_ ->
                let
                    a =
                        ProjectName.unsafeFromString "html"

                    b =
                        ProjectName.unsafeFromString "html"
                in
                Expect.true "`html` is equal `html`" (ProjectName.equals a b)
        , test "is false for non-equal names and handles" <|
            \_ ->
                let
                    a =
                        ProjectName.unsafeFromString "@alice/projectA"

                    b =
                        ProjectName.unsafeFromString "@bob/projectB"
                in
                Expect.false "@alice/projectA is not equal to @bob/projectB" (ProjectName.equals a b)
        , test "is false for non-equal names without handles" <|
            \_ ->
                let
                    a =
                        ProjectName.unsafeFromString "projectA"

                    b =
                        ProjectName.unsafeFromString "projectB"
                in
                Expect.false "projectA is not equal to projectB" (ProjectName.equals a b)
        , test "is false for equal names but not equal handles" <|
            \_ ->
                let
                    a =
                        ProjectName.unsafeFromString "@alice/base"

                    b =
                        ProjectName.unsafeFromString "@bob/base"
                in
                Expect.false "@alice/base is not equal to @bob/base" (ProjectName.equals a b)
        , test "is false for non-equal names but equal handles" <|
            \_ ->
                let
                    a =
                        ProjectName.unsafeFromString "@alice/projectA"

                    b =
                        ProjectName.unsafeFromString "@alice/projectB"
                in
                Expect.false "@alice/projectA is npt equal to @alice/projectB" (ProjectName.equals a b)
        ]
